%% Copyright (c) 2017 Guilherme Andrade <locus.lib@gandrade.net>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
%% locus is an independent project and has not been authorized, sponsored,
%% or otherwise approved by MaxMind.

%% @private
-module(locus_http_loader).
-behaviour(gen_statem).

-include_lib("kernel/include/file.hrl").
-include("locus_logger.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).                    -ignore_xref({start_link, 2}).
-export([wait/2]).

%% ------------------------------------------------------------------
%% gen_statem Function Exports
%% ------------------------------------------------------------------

-export([callback_mode/0]).
-export([init/1]).
%% states
-export([initializing/3]).                  -ignore_xref({initializing,3}).
-export([ready/3]).                         -ignore_xref({ready,3}).
-export([waiting_stream_start/3]).          -ignore_xref({waiting_stream_start,3}).
-export([waiting_stream_end/3]).            -ignore_xref({waiting_stream_end,3}).
-export([processing_update/3]).             -ignore_xref({processing_update,3}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).

-define(PRE_READINESS_UPDATE_PERIOD, (timer:minutes(1))).
-define(POST_READINESS_UPDATE_PERIOD, (timer:hours(6))).

-define(HTTP_CONNECT_TIMEOUT, (timer:seconds(8))).
-define(HTTP_IDLE_STREAM_TIMEOUT, (timer:seconds(5))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type state_data() ::
    #{ id := atom(),
       url := string(),
       request_id => reference(),
       last_response_headers => [{string(), string()}],
       last_response_body => binary(),
       last_modified => calendar:datetime(),
       last_version => calendar:datetime(),
       waiters => [gen_statem:from()]
     }.

-type headers() :: [{string(), string()}].

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(atom(), string()) -> {ok, pid()}.
start_link(Id, URL) ->
    ServerName = server_name(Id),
    gen_statem:start_link({local, ServerName}, ?CB_MODULE, [Id, URL], []).

-spec wait(atom(), timeout())
        -> {ok, LoadedVersion :: calendar:datetime()} |
           {error, database_unknown | timeout | {loading, term()}}.
wait(Id, Timeout) ->
    ServerName = server_name(Id),
    try gen_statem:call(ServerName, wait, Timeout) of
        {ok, LoadedVersion} ->
            {ok, LoadedVersion};
        {error, LoadingError} ->
            {error, {loading, LoadingError}}
    catch
        exit:{timeout, {gen_statem,call,[ServerName|_]}} when Timeout =/= infinity ->
            {error, timeout};
        %exit:{{nodedown,_RemoteNode}, {gen_statem,call,[ServerName|_}} ->
        %    % Cannot happen (loader is always local)
        %    {error, database_unknown};
        exit:{noproc, {gen_statem,call,[ServerName|_]}} ->
            {error, database_unknown};
        exit:{normal, {gen_statem,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{shutdown, {gen_statem,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{{shutdown,_Reason}, {gen_statem,call, [ServerName|_]}} ->
            {error, database_unknown}
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec callback_mode() -> [state_functions | state_enter, ...].
callback_mode() -> [state_functions, state_enter].

-spec init([atom() | string(), ...])
        -> gen_statem:init_result(initializing).
init([Id, URL]) ->
    locus_mmdb:create_table(Id),
    StateData = #{ id => Id, url => URL, waiters => [] },
    {ok, initializing, StateData, {next_event, internal, load_from_cache}}.

-spec initializing(enter, atom(), state_data())
                   -> keep_state_and_data;
                  (internal, load_from_cache, state_data())
                   -> {next_state, ready, state_data(), {next_event, internal, update_database}}.
initializing(enter, _PrevState, _StateData) ->
    keep_state_and_data;
initializing(internal, load_from_cache, StateData) ->
    CachedTarballName = cached_tarball_name(StateData),
    CachedTarballLookup = read_file_and_its_modification_date(CachedTarballName),
    StateData2 = handle_cached_tarball_lookup(CachedTarballLookup, CachedTarballName, StateData),
    {next_state, ready, StateData2, {next_event, internal, update_database}}.

-spec ready(enter, atom(), state_data())
            -> {keep_state_and_data, {state_timeout, pos_integer(), update_database}};
           ({call,gen_statem:from()}, wait, state_data())
           -> {keep_state, state_data(), [gen_statem:reply_action()]};
           (internal, update_database, state_data())
            -> {next_state, waiting_stream_start, state_data()};
           (state_timeout, update_database, state_data())
            -> {repeat_state_and_data, {next_event, internal, update_database}}.
ready(enter, _PrevState, StateData) ->
    {keep_state_and_data, {state_timeout, update_period(StateData), update_database}};
ready({call,From}, wait, StateData) ->
    {StateData2, Actions} = maybe_enqueue_waiter(From, StateData),
    {keep_state, StateData2, Actions};
ready(internal, update_database, StateData) ->
    ?log_info("sending request to download database ~p",
              [maps:get(id, StateData)]),
    URL = maps:get(url, StateData),
    Headers = request_headers(StateData),
    Request = {URL, Headers},
    HTTPOptions = [{connect_timeout, ?HTTP_CONNECT_TIMEOUT}],
    Options = [{sync, false}, {stream, self}],
    {ok, RequestId} = httpc:request(get, Request, HTTPOptions, Options),
    true = is_reference(RequestId),
    UpdatedStateData = StateData#{ request_id => RequestId },
    {next_state, waiting_stream_start, UpdatedStateData};
ready(state_timeout, update_database, _StateData) ->
    {repeat_state_and_data, {next_event, internal, update_database}}.

-spec waiting_stream_start(enter, atom(), state_data())
                            -> {keep_state_and_data, {state_timeout, pos_integer(), timeout}};
                          ({call,gen_statem:from()}, wait, state_data())
                            -> {keep_state, state_data(), [gen_statem:reply_action()]};
                          (info, {http, {reference(), stream_start, headers()}}, state_data())
                            -> {next_state, waiting_stream_end, state_data()};
                          (info, {http, {reference(), {{string(), integer(), string()},
                                                       headers(), binary()}}}, state_data())
                            -> {next_state, ready, state_data()} |
                               {next_state, ready, state_data(), [gen_statem:reply_action()]};
                          (info, {http, {reference(), {error, term()}}}, state_data())
                            -> {next_state, ready, state_data(), [gen_statem:reply_action()]};
                          (state_timeout, timeout, state_data())
                            -> {next_state, ready, state_data(), [gen_statem:reply_action()]}.
waiting_stream_start(enter, _PrevState, _StateData) ->
    {keep_state_and_data, {state_timeout, ?HTTP_IDLE_STREAM_TIMEOUT, timeout}};
waiting_stream_start({call,From}, wait, StateData) ->
    {StateData2, Actions} = maybe_enqueue_waiter(From, StateData),
    {keep_state, StateData2, Actions};
waiting_stream_start(info, {http, {RequestId, stream_start, Headers}},
                     #{ request_id := RequestId } = StateData) ->
    ?log_info("~p database download has started", [maps:get(id, StateData)]),
    UpdatedStateData = StateData#{ last_response_headers => Headers },
    {next_state, waiting_stream_end, UpdatedStateData};
waiting_stream_start(info,
                     {http, {RequestId, {{_HttpVersion, StatusCode, _StatusDesc}, _Headers, _Body}}},
                     #{ request_id := RequestId } = StateData)
  when StatusCode =:= 304 ->
    ?log_info("~p database download canceled - remote file not modified",
              [maps:get(id, StateData)]),
    UpdatedStateData = maps:remove(request_id, StateData),
    {next_state, ready, UpdatedStateData};
waiting_stream_start(info,
                     {http, {RequestId, {{_HttpVersion, StatusCode, StatusDesc}, _Headers, _Body}}},
                     #{ request_id := RequestId } = StateData) ->
    ?log_error("error downloading ~p database while ~p: ~p",
               [maps:get(id, StateData), waiting_stream_start,
                {http, StatusCode, StatusDesc}]),
    StateData2 = maps:remove(request_id, StateData),
    {StateData3, Replies} = reply_to_waiters({error, {http, StatusCode, StatusDesc}}, StateData2),
    {next_state, ready, StateData3, Replies};
waiting_stream_start(info, {http, {RequestId, {error, Reason}}},
                     #{ request_id := RequestId } = StateData) ->
    ?log_error("error downloading ~p database while ~p: ~p",
               [maps:get(id, StateData), waiting_stream_start,
                Reason]),
    StateData2 = maps:remove(request_id, StateData),
    {StateData3, Replies} = reply_to_waiters({error, {http, Reason}}, StateData2),
    {next_state, ready, StateData3, Replies};
waiting_stream_start(state_timeout, timeout, StateData) ->
    ?log_error("timeout downloading ~p database while ~p",
               [maps:get(id, StateData), waiting_stream_start]),
    {RequestId, StateData2} = maps:take(request_id, StateData),
    {StateData3, Replies} = reply_to_waiters({error, {timeout, waiting_stream_start}}, StateData2),
    ok = httpc:cancel_request(RequestId),
    clear_inbox_of_late_http_messages(RequestId),
    {next_state, ready, StateData3, Replies}.

-spec waiting_stream_end(enter, atom(), state_data())
                        -> {keep_state_and_data, {state_timeout, pos_integer(), timeout}};
                        ({call,gen_statem:from()}, wait, state_data())
                        -> {keep_state, state_data(), [gen_statem:reply_action()]};
                        (info, {http, {reference(), stream, binary()}}, state_data())
                        -> {keep_state, state_data(), {state_timeout, pos_integer(), timeout}};
                        (info, {http, {reference(), stream_end, headers()}}, state_data())
                        -> {next_state, processing_update, state_data(),
                            {next_event, internal, execute}};
                        (info, {http, {reference(), {error, term()}}}, state_data())
                        -> {next_state, ready, state_data(), [gen_statem:reply_action()]};
                        (state_timeout, timeout, state_data())
                        -> {next_state, ready, state_data(), [gen_statem:reply_action()]}.
waiting_stream_end(enter, _PrevState, _StateData) ->
    {keep_state_and_data, {state_timeout, ?HTTP_IDLE_STREAM_TIMEOUT, timeout}};
waiting_stream_end({call,From}, wait, StateData) ->
    {StateData2, Actions} = maybe_enqueue_waiter(From, StateData),
    {keep_state, StateData2, Actions};
waiting_stream_end(info, {http, {RequestId, stream, BinBodyPart}},
                   #{ request_id := RequestId } = StateData) ->
    UpdatedStateData =
        maps:update_with(
          last_response_body,
          fun (Body) -> <<Body/binary, BinBodyPart/binary>> end,
          BinBodyPart, StateData),
    %?log_info("~p database download in progress - ~.3f MiB received so far",
    %          [maps:get(id, StateData),
    %           byte_size(maps:get(last_response_body, UpdatedStateData)) / (1 bsl 20)]),
    {keep_state, UpdatedStateData, {state_timeout, ?HTTP_IDLE_STREAM_TIMEOUT, timeout}};
waiting_stream_end(info, {http, {RequestId, stream_end, Headers}}, % no chunked encoding
                   #{ request_id := RequestId } = StateData) ->
    ?log_info("~p database download finished", [maps:get(id, StateData)]),
    StateData2 =
        maps:update_with(
          last_response_headers,
          fun (PrevHeaders) -> PrevHeaders ++ Headers end,
          Headers, StateData),
    StateData3 = maps:remove(request_id, StateData2),
    {next_state, processing_update, StateData3,
     {next_event, internal, execute}};
waiting_stream_end(info, {http, {RequestId, {error, Reason}}},
                   #{ request_id := RequestId } = StateData) ->
    ?log_error("error downloading ~p database while ~p: ~p",
               [maps:get(id, StateData), waiting_stream_end, Reason]),
    StateData2 =
        maps:without([request_id, last_response_headers, last_response_body],
                     StateData),
    {StateData3, Replies} = reply_to_waiters({error, {http, Reason}}, StateData2),
    {next_state, ready, {StateData3, Replies}};
waiting_stream_end(state_timeout, timeout, StateData) ->
    ?log_error("timeout downloading ~p database while ~p",
               [maps:get(id, StateData), waiting_stream_end]),
    {RequestId, StateData2} = maps:take(request_id, StateData),
    StateData3 = maps:without([last_response_headers, last_response_body], StateData2),
    {StateData4, Replies} = reply_to_waiters({error, {timeout, waiting_stream_end}}, StateData3),
    ok = httpc:cancel_request(RequestId),
    clear_inbox_of_late_http_messages(RequestId),
    {next_state, ready, StateData4, Replies}.

-spec processing_update(enter, atom(), state_data())
                        -> keep_state_and_data;
                       (internal, execute, state_data())
                       -> {next_state, ready, state_data(), [gen_statem:reply_action()]}.
processing_update(enter, _PrevState, _StateData) ->
    keep_state_and_data;
processing_update(internal, execute, StateData) ->
    Id = maps:get(id, StateData),
    ?log_info("now processing ~p database", [Id]),
    {Headers, StateData2} = maps:take(last_response_headers, StateData),
    {Body, StateData3} = maps:take(last_response_body, StateData2),
    case load_database_from_tarball(Id, Body) of
        {ok, Version} ->
            ?log_info("~p database version is now ~p", [Id, Version]),
            LastModified = extract_last_modified_datetime_from_response_headers(Headers),
            StateData4 = StateData3#{ last_modified => max(LastModified, Version),
                                      last_version => Version },
            try_saving_cached_tarball(Body, LastModified, StateData4),
            {StateData5, Replies} = reply_to_waiters({ok, Version}, StateData4),
            {next_state, ready, StateData5, Replies};
        {error, Error} ->
            ?log_warning("failed to load ~p database from downloaded tarball: ~p",
                         [Id, Error]),
            {StateData4, Replies} = reply_to_waiters({error, Error}, StateData3),
            {next_state, ready, StateData4, Replies}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec server_name(atom()) -> atom().
server_name(Id) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Id)).

-spec cached_tarball_name(state_data()) -> nonempty_string().
cached_tarball_name(StateData) ->
    #{ url := URL } = StateData,
    Hash = crypto:hash(sha256, URL),
    HexHash = bin_to_hex_str(Hash),
    Filename = HexHash ++ ".tgz",
    UserCachePath = filename:basedir(user_cache, "locus_erlang"),
    filename:join(UserCachePath, Filename).

-spec handle_cached_tarball_lookup(LookupResult, nonempty_string(), state_data())
        -> state_data() when LookupResult :: ({ok, binary(), calendar:datetime()} |
                                              {error, term()}).
handle_cached_tarball_lookup({ok, Content, ModificationDate}, CachedTarballName, StateData) ->
    Id = maps:get(id, StateData),
    case load_database_from_tarball(Id, Content) of
        {ok, Version} ->
            ?log_info("~p database version is now ~p (loaded from cache)", [Id, Version]),
            StateData#{ last_modified => max(ModificationDate, Version),
                        last_version => Version };
        {error, Error} ->
            ?log_warning("failure loading ~p database from cache file ~p: ~p",
                         [Id, CachedTarballName, Error]),
            StateData
    end;
handle_cached_tarball_lookup({error, Error}, CachedTarballName, StateData) ->
    ?log_warning("failure loading ~p database from cache file ~p: ~p",
                 [maps:get(id, StateData), CachedTarballName, Error]),
    StateData.

-spec load_database_from_tarball(atom(), binary())
        -> {ok, calendar:datetime()} |
           {error, {exception, atom(), term()}}.
load_database_from_tarball(Id, Tarball) ->
    try
        BinDatabase = extract_database_from_tarball(Tarball),
        Version = locus_mmdb:decode_and_update(Id, BinDatabase),
        {ok, Version}
    catch
        Class:Reason ->
            {error, {exception, Class, Reason}}
    end.

-spec try_saving_cached_tarball(binary(), calendar:datetime(), state_data()) -> ok.
try_saving_cached_tarball(Tarball, LastModified, StateData) ->
    case save_cached_tarball(Tarball, LastModified, StateData) of
        {ok, Filename} ->
            ?log_info("~p database cached in ~p",
                      [maps:get(id, StateData), Filename]);
        {{error, Error}, Filename} ->
            ?log_error("error caching ~p database in ~p: ~p",
                       [maps:get(id, StateData), Filename, Error])
    end.

-spec save_cached_tarball(binary(), calendar:datetime(), state_data())
        -> {Status, Filename} when Status :: ok | {error, {exception, atom(), term()}},
                                   Filename :: nonempty_string().
save_cached_tarball(Tarball, LastModified, StateData) ->
    Filename = cached_tarball_name(StateData),
    TmpSuffix = ".tmp." ++ integer_to_list(rand:uniform(1 bsl 32), 36),
    TmpFilename = Filename ++ TmpSuffix,
    try
        ok = filelib:ensure_dir(Filename),
        {ok, IoDevice} = file:open(TmpFilename, [write, exclusive, raw]),
        ok = file:write(IoDevice, Tarball),
        ok = file:close(IoDevice),
        ok = file:change_time(TmpFilename, LastModified),
        ok = file:rename(TmpFilename, Filename),
        {ok, Filename}
    catch
        Class:Reason ->
            {{error, {exception, Class, Reason}}, Filename}
    end.

-spec bin_to_hex_str(binary()) -> [$0..$9 | $a..$f].
bin_to_hex_str(Bin) ->
    bin_to_hex_str_recur(Bin, []).

-spec bin_to_hex_str_recur(bitstring(), [$0..$9 | $a..$f]) -> [$0..$9 | $a..$f].
bin_to_hex_str_recur(<<Nibble:4, Rest/bits>>, Acc) when Nibble < 10 ->
    bin_to_hex_str_recur(Rest, [$0 + Nibble | Acc]);
bin_to_hex_str_recur(<<Nibble:4, Rest/bits>>, Acc) ->
    bin_to_hex_str_recur(Rest, [$a + Nibble | Acc]);
bin_to_hex_str_recur(<<>>, Acc) ->
    lists:reverse(Acc).

-spec read_file_and_its_modification_date(nonempty_string())
        -> {ok, binary(), calendar:datetime()} |
           {error, {read_file | read_file_info, term()}}.
read_file_and_its_modification_date(Filename) ->
    case file:read_file_info(Filename, [{time,universal}]) of
        {ok, #file_info{ mtime = ModificationDate }} ->
            case file:read_file(Filename) of
                {ok, Content} ->
                    {ok, Content, ModificationDate};
                {error, Error} ->
                    {error, {read_file, Error}}
            end;
        {error, Error} ->
            {error, {read_file_info, Error}}
    end.

-spec update_period(state_data()) -> pos_integer().
update_period(#{ last_modified := _ } = _StateData) ->
    ?POST_READINESS_UPDATE_PERIOD;
update_period(#{} = _StateData) ->
    ?PRE_READINESS_UPDATE_PERIOD.

request_headers(#{ last_modified := LastModified } = _StateData) ->
    [{"if-modified-since", httpd_util:rfc1123_date(LastModified)}
     | base_request_headers()];
request_headers(#{} = _StateData) ->
    base_request_headers().

base_request_headers() ->
    [{"accept", join_header_values(
                  ["application/gzip",
                   "application/x-gzip",
                   "application/x-gtar",
                   "application/x-tgz"])},
     {"content-encoding", "identity"},
     {"transfer-encoding", "identity"},
     {"connection", "close"}
    ].

join_header_values(Values) ->
    string:join(Values, "; ").

-spec extract_last_modified_datetime_from_response_headers(headers()) -> calendar:datetime().
extract_last_modified_datetime_from_response_headers(Headers) ->
    CiHeaders = lists:keymap(fun string:to_lower/1, 1, Headers),
    case lists:keyfind("last-modified", 1, CiHeaders) of
        {"last-modified", LastModified} ->
            ({_,_} = ModificationDate) = httpd_util:convert_request_date(LastModified),
            ModificationDate;
        false ->
            {{1970,1,1}, {0,0,0}}
    end.

-spec extract_database_from_tarball(binary()) -> binary().
extract_database_from_tarball(Tarball) ->
    {ok, ContainedPaths} = erl_tar:table({binary, Tarball}, [compressed]),
    {true, DatabasePath} = lists_anymap(fun has_mmdb_extension/1, ContainedPaths),
    {ok, [{DatabasePath, BinDatabase}]} =
        erl_tar:extract({binary, Tarball}, [{files, [DatabasePath]}, memory, compressed]),
    BinDatabase.

lists_anymap(Fun, [H|T]) ->
    case Fun(H) of
        %{true, Mapped} -> {true, Mapped};
        true -> {true, H};
        false -> lists_anymap(Fun, T)
    end;
lists_anymap(_Fun, []) ->
    false.

-spec has_mmdb_extension(nonempty_string()) -> boolean().
has_mmdb_extension(Filename) ->
    filename:extension(Filename) =:= ".mmdb".

-spec clear_inbox_of_late_http_messages(reference()) -> ok.
clear_inbox_of_late_http_messages(RequestId) ->
    % Only a best effort. It's possible that a delayed message could
    % arrive after the timeout.
    receive
        {http, {RequestId, stream_start, _Headers}} ->
            clear_inbox_of_late_http_messages(RequestId);
        {http, {RequestId, stream, _BinBodyPart}} ->
            clear_inbox_of_late_http_messages(RequestId);
        {http, {RequestId, stream_end, _Headers}} ->
            clear_inbox_of_late_http_messages(RequestId);
        {http, {RequestId, {error, _Reason}}} ->
            clear_inbox_of_late_http_messages(RequestId)
    after
        1000 -> ok
    end.

-spec maybe_enqueue_waiter(gen_statem:from(), state_data())
        -> {state_data(), [gen_statem:reply_action()]}.
maybe_enqueue_waiter(From, #{ last_version := LastVersion } = StateData) ->
    {StateData, [{reply, From, {ok, LastVersion}}]};
maybe_enqueue_waiter(From, StateData) ->
    StateData2 =
        maps:update_with(
          waiters,
          fun (PrevWaiters) -> [From | PrevWaiters] end,
          StateData),
    {StateData2, []}.

-spec reply_to_waiters({ok, calendar:datetime()} | {error, term()}, state_data())
        -> {state_data(), [gen_statem:reply_action()]}.
reply_to_waiters(Result, StateData) ->
    Waiters = maps:get(waiters, StateData),
    Replies = [{reply, From, Result} || From <- Waiters],
    {StateData#{ waiters := [] }, Replies}.
