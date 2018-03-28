%% Copyright (c) 2017-2018 Guilherme Andrade
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

-include("locus_pre_otp19_compat.hrl").

-module(locus_http_loader).
-behaviour(?gen_statem).

-include_lib("kernel/include/file.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).                    -ignore_xref({start_link, 3}).
-export([wait/2]).

-ifdef(TEST).
-export([whereis/1]).
-export([list_subscribers/1]).
-export([cached_tarball_name_for_url/1]).
-endif.

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
%%
-export([code_change/4]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).

-define(PRE_READINESS_UPDATE_PERIOD, (timer:minutes(1))).
-define(POST_READINESS_UPDATE_PERIOD, (timer:hours(6))).

-define(DEFAULT_HTTP_CONNECT_TIMEOUT, (timer:seconds(8))).
-define(DEFAULT_HTTP_DOWNLOAD_START_TIMEOUT, (timer:seconds(5))).
-define(DEFAULT_HTTP_IDLE_DOWNLOAD_TIMEOUT, (timer:seconds(5))).

-define(is_timeout(V), ((is_integer((V)) andalso ((V) >= 0)) orelse ((V) =:= infinity))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type from() :: {To :: pid(), Tag :: term()}.

-type opt() ::
    {event_subscriber, module() | pid()} |
    {connect_timeout, timeout()} |
    {download_start_timeout, timeout()} |
    {idle_download_timeout, timeout()} |
    no_cache.
-export_type([opt/0]).

-ifdef(POST_OTP_18).
-type state_data() ::
    #{ id := atom(),
       url := url(),
       waiters := [from()],
       event_subscribers := [module() | pid()],
       connect_timeout := timeout(),
       download_start_timeout := timeout(),
       idle_download_timeout := timeout(),
       no_cache => true,

       request_id => reference(),
       last_response_headers => headers(),
       last_response_body => binary(),
       last_modified => calendar:datetime(),
       last_version => calendar:datetime()
     }.
-else.
-type state_data() ::
    #{ id => atom(),
       url => url(),
       waiters => [from()],
       event_subscribers => [module() | pid()],
       connect_timeout => timeout(),
       download_start_timeout => timeout(),
       idle_download_timeout => timeout(),
       no_cache => true,

       request_id => reference(),
       last_response_headers => headers(),
       last_response_body => binary(),
       last_modified => calendar:datetime(),
       last_version => calendar:datetime()
     }.
-endif.

-type filename() :: string().
-export_type([filename/0]).

-type url() :: string().
-export_type([url/0]).

-type response_status() :: {100..999, binary()}.
-export_type([response_status/0]).

-type headers() :: [{string(), string()}].
-export_type([headers/0]).

-type body() :: binary().
-export_type([body/0]).

-type event() ::
        event_request_sent() |
        event_download_dismissed() |
        event_download_failed_to_start() |
        event_download_started() |
        event_download_finished() |
        event_load_attempt_finished() |
        event_cache_attempt_finished().
-export_type([event/0]).

-type event_request_sent() ::
        {request_sent, url(), headers()}.
-export_type([event_request_sent/0]).

-type event_download_dismissed() ::
        {download_dismissed, {http, response_status(), headers(), body()}}.
-export_type([event_download_dismissed/0]).

-type event_download_failed_to_start() ::
        {download_failed_to_start, {http, response_status(), headers(), body()}} |
        {download_failed_to_start, {error, term()}} |
        {download_failed_to_start, timeout}.
-export_type([event_download_failed_to_start/0]).

-type event_download_started() ::
        {download_started, headers()}.
-export_type([event_download_started/0]).

-type event_download_finished() ::
        {download_finished, BodySize :: non_neg_integer(), {ok, TrailingHeaders :: headers()}} |
        {download_finished, BodySize :: non_neg_integer(), {error, term()}} |
        {download_finished, BodySize :: non_neg_integer(), {error, timeout}}.
-export_type([event_download_finished/0]).

-type event_load_attempt_finished() ::
        {load_attempt_finished, locus_mmdb:http_loader_source(), {ok, Version :: calendar:datetime()}} |
        {load_attempt_finished, locus_mmdb:http_loader_source(), {error, term()}}.
-export_type([event_load_attempt_finished/0]).

-type event_cache_attempt_finished() ::
        {cache_attempt_finished, filename(), ok} |
        {cache_attempt_finished, filename(), {error, term()}}.
-export_type([event_cache_attempt_finished/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(atom(), string(), [opt()]) -> {ok, pid()}.
%% @private
start_link(Id, URL, Opts) ->
    ServerName = server_name(Id),
    ?gen_statem:start_link({local, ServerName}, ?CB_MODULE, [Id, URL, Opts], []).

-spec wait(atom(), timeout())
        -> {ok, LoadedVersion :: calendar:datetime()} |
           {error, database_unknown | timeout | {loading, term()}}.
%% @private
wait(Id, Timeout) ->
    ServerName = server_name(Id),
    try ?gen_statem:call(ServerName, wait, Timeout) of
        {ok, LoadedVersion} ->
            {ok, LoadedVersion};
        {error, LoadingError} ->
            {error, {loading, LoadingError}}
    catch
        exit:{timeout, {?gen_statem,call,[ServerName|_]}} when Timeout =/= infinity ->
            {error, timeout};
        %exit:{{nodedown,_RemoteNode}, {?gen_statem,call,[ServerName|_}} ->
        %    % Cannot happen (loader is always local)
        %    {error, database_unknown};
        exit:{noproc, {?gen_statem,call,[ServerName|_]}} ->
            {error, database_unknown};
        exit:{normal, {?gen_statem,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{shutdown, {?gen_statem,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{{shutdown,_Reason}, {?gen_statem,call, [ServerName|_]}} ->
            {error, database_unknown}
    end.

-ifdef(TEST).
%% @private
whereis(Id) ->
    ServerName = server_name(Id),
    erlang:whereis(ServerName).

%% @private
list_subscribers(Id) ->
    ServerName = server_name(Id),
    {_State, StateData} = sys:get_state(ServerName),
    maps:get(event_subscribers, StateData).
-endif.

%% ------------------------------------------------------------------
%% gen_statem Function Definitions
%% ------------------------------------------------------------------

-spec callback_mode() -> [state_functions | state_enter, ...].
%% @private
callback_mode() -> [state_functions, state_enter].

-spec init([atom() | string() | opt(), ...])
        -> ?gen_statem:init_result(initializing).
%% @private
init([Id, URL, Opts]) ->
    locus_rand_compat:seed(),
    locus_mmdb:create_table(Id),
    init(Id, URL, Opts).

-spec initializing(enter, atom(), state_data())
                   -> keep_state_and_data;
                  (internal, maybe_load_from_cache, state_data())
                   -> {next_state, ready, state_data(),
                       {next_event, internal, update_database}}.
%% @private
initializing(enter, _PrevState, _StateData) ->
    keep_state_and_data;
initializing(internal, maybe_load_from_cache, #{ no_cache := true } = StateData) ->
    {next_state, ready, StateData, {next_event, internal, update_database}};
initializing(internal, maybe_load_from_cache, StateData) ->
    CachedTarballName = cached_tarball_name(StateData),
    CachedTarballLookup = locus_util:read_file_and_its_modification_date(CachedTarballName),
    StateData2 = handle_cached_tarball_lookup(CachedTarballLookup, CachedTarballName, StateData),
    {next_state, ready, StateData2, {next_event, internal, update_database}}.

-spec ready(enter, atom(), state_data())
            -> {keep_state_and_data, {state_timeout, pos_integer(), update_database}};
           ({call,from()}, wait, state_data())
           -> {keep_state, state_data(), [?gen_statem:reply_action()]};
           (info, {'DOWN', reference(), process, pid(), term()}, state_data())
           -> {keep_state, state_data()};
           (internal, update_database, state_data())
            -> {next_state, waiting_stream_start, state_data()};
           (state_timeout, update_database, state_data())
            -> {repeat_state_and_data, {next_event, internal, update_database}}.
%% @private
ready(enter, _PrevState, StateData) ->
    {keep_state_and_data, {state_timeout, update_period(StateData), update_database}};
ready({call,From}, wait, StateData) ->
    {StateData2, Actions} = maybe_enqueue_waiter(From, StateData),
    {keep_state, StateData2, Actions};
ready(info, {'DOWN', _Ref, process, Pid, _Reason}, StateData) ->
    handle_monitored_process_death(Pid, StateData);
ready(internal, update_database, StateData) ->
    URL = maps:get(url, StateData),
    Headers = request_headers(StateData),
    Request = {URL, Headers},
    ConnectTimeout = maps:get(connect_timeout, StateData),
    HTTPOptions = [{connect_timeout, ConnectTimeout}],
    Options = [{sync, false}, {stream, self}],
    {ok, RequestId} = httpc:request(get, Request, HTTPOptions, Options),
    true = is_reference(RequestId),
    UpdatedStateData = StateData#{ request_id => RequestId },
    report_event({request_sent, URL, Headers}, StateData),
    {next_state, waiting_stream_start, UpdatedStateData};
ready(state_timeout, update_database, _StateData) ->
    {repeat_state_and_data, {next_event, internal, update_database}}.

-spec waiting_stream_start(enter, atom(), state_data())
                            -> {keep_state_and_data, {state_timeout, pos_integer(), timeout}};
                          ({call,from()}, wait, state_data())
                            -> {keep_state, state_data(), [?gen_statem:reply_action()]};
                          (info, {http, {reference(), stream_start, headers()}}, state_data())
                            -> {next_state, waiting_stream_end, state_data()};
                          (info, {http, {reference(), {{string(), integer(), string()},
                                                       headers(), binary()}}}, state_data())
                            -> {next_state, ready, state_data()} |
                               {next_state, ready, state_data(), [?gen_statem:reply_action()]};
                          (info, {http, {reference(), {error, term()}}}, state_data())
                            -> {next_state, ready, state_data(), [?gen_statem:reply_action()]};
                          (info, {'DOWN', reference(), process, pid(), term()}, state_data())
                          -> {keep_state, state_data()};
                          (state_timeout, timeout, state_data())
                            -> {next_state, ready, state_data(), [?gen_statem:reply_action()]}.
%% @private
waiting_stream_start(enter, _PrevState, StateData) ->
    StreamStartTimeout = maps:get(download_start_timeout, StateData),
    {keep_state_and_data, {state_timeout, StreamStartTimeout, timeout}};
waiting_stream_start({call,From}, wait, StateData) ->
    {StateData2, Actions} = maybe_enqueue_waiter(From, StateData),
    {keep_state, StateData2, Actions};
waiting_stream_start(info, {http, {RequestId, stream_start, Headers}},
                     #{ request_id := RequestId } = StateData) ->
    report_event({download_started, Headers}, StateData),
    UpdatedStateData = StateData#{ last_response_headers => Headers,
                                   last_response_body => <<>> },
    {next_state, waiting_stream_end, UpdatedStateData};
waiting_stream_start(info,
                     {http, {RequestId, {{_HttpVersion, StatusCode, StatusDesc}, Headers, Body}}},
                     #{ request_id := RequestId } = StateData)
  when StatusCode =:= 304 ->
    report_event({download_dismissed, {http, {StatusCode, StatusDesc}, Headers, Body}}, StateData),
    UpdatedStateData = maps:remove(request_id, StateData),
    {next_state, ready, UpdatedStateData};
waiting_stream_start(info,
                     {http, {RequestId, {{_HttpVersion, StatusCode, StatusDesc}, Headers, Body}}},
                     #{ request_id := RequestId } = StateData) ->
    report_event({download_failed_to_start, {http, {StatusCode, StatusDesc}, Headers, Body}}, StateData),
    StateData2 = maps:remove(request_id, StateData),
    {StateData3, Replies} = reply_to_waiters({error, {http, StatusCode, StatusDesc}}, StateData2),
    {next_state, ready, StateData3, Replies};
waiting_stream_start(info, {http, {RequestId, {error, Reason}}},
                     #{ request_id := RequestId } = StateData) ->
    report_event({download_failed_to_start, {error, Reason}}, StateData),
    StateData2 = maps:remove(request_id, StateData),
    {StateData3, Replies} = reply_to_waiters({error, {http, Reason}}, StateData2),
    {next_state, ready, StateData3, Replies};
waiting_stream_start(info, {'DOWN', _Ref, process, Pid, _Reason}, StateData) ->
    handle_monitored_process_death(Pid, StateData);
waiting_stream_start(state_timeout, timeout, StateData) ->
    report_event({download_failed_to_start, timeout}, StateData),
    {RequestId, StateData2} = ?maps_take(request_id, StateData),
    {StateData3, Replies} = reply_to_waiters({error, {timeout, waiting_stream_start}}, StateData2),
    ok = httpc:cancel_request(RequestId),
    clear_inbox_of_late_http_messages(RequestId),
    {next_state, ready, StateData3, Replies}.

-spec waiting_stream_end(enter, atom(), state_data())
                        -> {keep_state_and_data, {state_timeout, pos_integer(), timeout}};
                        ({call,from()}, wait, state_data())
                        -> {keep_state, state_data(), [?gen_statem:reply_action()]};
                        (info, {http, {reference(), stream, binary()}}, state_data())
                        -> {keep_state, state_data(), {state_timeout, pos_integer(), timeout}};
                        (info, {http, {reference(), stream_end, headers()}}, state_data())
                        -> {next_state, processing_update, state_data(),
                            {next_event, internal, execute}};
                        (info, {http, {reference(), {error, term()}}}, state_data())
                        -> {next_state, ready, state_data(), [?gen_statem:reply_action()]};
                        (info, {'DOWN', reference(), process, pid(), term()}, state_data())
                        -> {keep_state, state_data()};
                        (state_timeout, timeout, state_data())
                        -> {next_state, ready, state_data(), [?gen_statem:reply_action()]}.
%% @private
waiting_stream_end(enter, _PrevState, StateData) ->
    IdleStreamTimeout = maps:get(idle_download_timeout, StateData),
    {keep_state_and_data, {state_timeout, IdleStreamTimeout, timeout}};
waiting_stream_end({call,From}, wait, StateData) ->
    {StateData2, Actions} = maybe_enqueue_waiter(From, StateData),
    {keep_state, StateData2, Actions};
waiting_stream_end(info, {http, {RequestId, stream, BinBodyPart}},
                   #{ request_id := RequestId } = StateData) ->
    UpdatedStateData =
        ?maps_update_with3(
          last_response_body,
          fun (Body) -> <<Body/binary, BinBodyPart/binary>> end,
          StateData),
    %?log_info("~p database download in progress - ~.3f MiB received so far",
    %          [maps:get(id, StateData),
    %           byte_size(maps:get(last_response_body, UpdatedStateData)) / (1 bsl 20)]),
    IdleStreamTimeout = maps:get(idle_download_timeout, StateData),
    {keep_state, UpdatedStateData, {state_timeout, IdleStreamTimeout, timeout}};
waiting_stream_end(info, {http, {RequestId, stream_end, Headers}}, % no chunked encoding
                   #{ request_id := RequestId } = StateData) ->
    BodySize = byte_size(maps:get(last_response_body, StateData)),
    report_event({download_finished, BodySize, {ok, Headers}}, StateData),
    StateData2 =
        ?maps_update_with4(
          last_response_headers,
          fun (PrevHeaders) -> lists:usort(PrevHeaders ++ Headers) end,
          Headers, StateData),
    StateData3 = maps:remove(request_id, StateData2),
    {next_state, processing_update, StateData3,
     {next_event, internal, execute}};
waiting_stream_end(info, {http, {RequestId, {error, Reason}}},
                   #{ request_id := RequestId } = StateData) ->
    BodySizeSoFar = byte_size(maps:get(last_response_body, StateData)),
    report_event({download_finished, BodySizeSoFar, {error, Reason}}, StateData),
    StateData2 =
        maps:without([request_id, last_response_headers, last_response_body],
                     StateData),
    {StateData3, Replies} = reply_to_waiters({error, {http, Reason}}, StateData2),
    {next_state, ready, {StateData3, Replies}};
waiting_stream_end(info, {'DOWN', _Ref, process, Pid, _Reason}, StateData) ->
    handle_monitored_process_death(Pid, StateData);
waiting_stream_end(state_timeout, timeout, StateData) ->
    BodySizeSoFar = byte_size(maps:get(last_response_body, StateData)),
    report_event({download_finished, BodySizeSoFar, {error, timeout}}, StateData),
    {RequestId, StateData2} = ?maps_take(request_id, StateData),
    StateData3 = maps:without([last_response_headers, last_response_body], StateData2),
    {StateData4, Replies} = reply_to_waiters({error, {timeout, waiting_stream_end}}, StateData3),
    ok = httpc:cancel_request(RequestId),
    clear_inbox_of_late_http_messages(RequestId),
    {next_state, ready, StateData4, Replies}.

-spec processing_update(enter, atom(), state_data())
                        -> keep_state_and_data;
                       (internal, execute, state_data())
                       -> {next_state, ready, state_data(), [?gen_statem:reply_action()]}.
%% @private
processing_update(enter, _PrevState, _StateData) ->
    keep_state_and_data;
processing_update(internal, execute, StateData) ->
    Id = maps:get(id, StateData),
    {Headers, StateData2} = ?maps_take(last_response_headers, StateData),
    {Body, StateData3} = ?maps_take(last_response_body, StateData2),
    URL = maps:get(url, StateData),
    Source = {remote, URL},
    case locus_util:load_database_from_tarball(Id, Body, Source) of
        {ok, Version} ->
            report_event({load_attempt_finished, Source, {ok, Version}}, StateData),
            LastModified = extract_last_modified_datetime_from_response_headers(Headers),
            StateData4 = StateData3#{ last_modified => LastModified,
                                      last_version => Version },
            maybe_try_saving_cached_tarball(Body, LastModified, StateData4),
            {StateData5, Replies} = reply_to_waiters({ok, Version}, StateData4),
            {next_state, ready, StateData5, Replies};
        {error, Error} ->
            report_event({load_attempt_finished, Source, {error, Error}}, StateData),
            {StateData4, Replies} = reply_to_waiters({error, Error}, StateData3),
            {next_state, ready, StateData4, Replies}
    end.

-spec code_change(term(), atom(), state_data(), term()) -> {ok, atom(), state_data()}.
%% @private
code_change(_OldVsn, OldState, OldStateData, _Extra) ->
    case OldStateData of
        #{ id := _, url := _, waiters := _, event_subscribers := _ } ->
            {ok, OldState, OldStateData};
        #{ id := _, url := _, waiters := _ } ->
            % lib was at version 1.0.0; ensure everything works as before
            EventSubscribers = [locus_logger],
            StateData = OldStateData#{ event_subscribers => EventSubscribers,
                                       connect_timeout => ?DEFAULT_HTTP_CONNECT_TIMEOUT,
                                       download_start_timeout => ?DEFAULT_HTTP_DOWNLOAD_START_TIMEOUT,
                                       idle_download_timeout => ?DEFAULT_HTTP_IDLE_DOWNLOAD_TIMEOUT },
            {ok, OldState, StateData}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec server_name(atom()) -> atom().
server_name(Id) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Id)).

-spec init(atom(), string(), [opt()])
        -> ?gen_statem:init_result(initializing) |
           {stop, {invalid_opt, term()}}.
init(Id, URL, Opts) ->
    BaseStateData =
        #{ id => Id,
           url => URL,
           waiters => [],
           event_subscribers => [],
           connect_timeout => ?DEFAULT_HTTP_CONNECT_TIMEOUT,
           download_start_timeout => ?DEFAULT_HTTP_DOWNLOAD_START_TIMEOUT,
           idle_download_timeout => ?DEFAULT_HTTP_IDLE_DOWNLOAD_TIMEOUT
         },
    init_opts(Opts, BaseStateData).

init_opts([{event_subscriber, Module} | Opts], StateData) when is_atom(Module), Module =/= undefined ->
    NewStateData =
        ?maps_update_with3(
          event_subscribers,
          fun (PrevSubscribers) -> [Module | PrevSubscribers] end,
          StateData),
    init_opts(Opts, NewStateData);
init_opts([{event_subscriber, Pid} | Opts], StateData) when is_pid(Pid) ->
    _ = monitor(process, Pid),
    NewStateData =
        ?maps_update_with3(
          event_subscribers,
          fun (PrevSubscribers) -> [Pid | PrevSubscribers] end,
          StateData),
    init_opts(Opts, NewStateData);
init_opts([{connect_timeout, Timeout} | Opts], StateData) when ?is_timeout(Timeout) ->
    NewStateData = StateData#{ connect_timeout := Timeout },
    init_opts(Opts, NewStateData);
init_opts([{download_start_timeout, Timeout} | Opts], StateData) when ?is_timeout(Timeout) ->
    NewStateData = StateData#{ download_start_timeout := Timeout },
    init_opts(Opts, NewStateData);
init_opts([{idle_download_timeout, Timeout} | Opts], StateData) when ?is_timeout(Timeout) ->
    NewStateData = StateData#{ idle_download_timeout := Timeout },
    init_opts(Opts, NewStateData);
init_opts([no_cache | Opts], StateData) ->
    NewStateData = StateData#{ no_cache => true },
    init_opts(Opts, NewStateData);
init_opts([InvalidOpt | _], _StateData) ->
    {stop, {invalid_opt, InvalidOpt}};
init_opts([], StateData) ->
    Actions = [{next_event, internal, maybe_load_from_cache}],
    {ok, initializing, StateData, Actions}.

-spec cached_tarball_name(state_data()) -> nonempty_string().
cached_tarball_name(StateData) ->
    #{ url := URL } = StateData,
    cached_tarball_name_for_url(URL).

-spec cached_tarball_name_for_url(string()) -> nonempty_string().
%% @private
cached_tarball_name_for_url(URL) ->
    Hash = crypto:hash(sha256, URL),
    HexHash = bin_to_hex_str(Hash),
    Filename = HexHash ++ ".tgz",
    UserCachePath = ?filename_basedir(user_cache, "locus_erlang"),
    filename:join(UserCachePath, Filename).

-spec handle_cached_tarball_lookup(LookupResult, nonempty_string(), state_data())
        -> state_data() when LookupResult :: ({ok, binary(), calendar:datetime()} |
                                              {error, term()}).
handle_cached_tarball_lookup({ok, Content, ModificationDate}, CachedTarballName, StateData) ->
    Id = maps:get(id, StateData),
    Source = {cache, CachedTarballName},
    case locus_util:load_database_from_tarball(Id, Content, Source) of
        {ok, Version} ->
            report_event({load_attempt_finished, Source, {ok, Version}}, StateData),
            StateData#{ last_modified => ModificationDate,
                        last_version => Version };
        {error, Error} ->
            report_event({load_attempt_finished, Source, {error, Error}}, StateData),
            StateData
    end;
handle_cached_tarball_lookup({error, Error}, CachedTarballName, StateData) ->
    Source = {cache, CachedTarballName},
    report_event({load_attempt_finished, Source, {error, Error}}, StateData),
    StateData.

-spec maybe_try_saving_cached_tarball(binary(), calendar:datetime(), state_data()) -> ok.
maybe_try_saving_cached_tarball(_Tarball, _LastModified, #{ no_cache := true }) ->
    ok;
maybe_try_saving_cached_tarball(Tarball, LastModified, StateData) ->
    case save_cached_tarball(Tarball, LastModified, StateData) of
        {ok, Filename} ->
            report_event({cache_attempt_finished, Filename, ok}, StateData);
        {{error, Error}, Filename} ->
            report_event({cache_attempt_finished, Filename, {error, Error}}, StateData)
    end.

-spec save_cached_tarball(binary(), calendar:datetime(), state_data())
        -> {Status, Filename} when Status :: ok | {error, {exception, atom(), term()}},
                                   Filename :: nonempty_string().
save_cached_tarball(Tarball, LastModified, StateData) ->
    Filename = cached_tarball_name(StateData),
    TmpSuffix = ".tmp." ++ integer_to_list(locus_rand_compat:uniform(1 bsl 32), 36),
    TmpFilename = Filename ++ TmpSuffix,
    FileInfoMod = #file_info{ mtime = LastModified },
    try
        ok = filelib:ensure_dir(Filename),
        {ok, IoDevice} = file:open(TmpFilename, [write, exclusive, raw]),
        ok = file:write(IoDevice, Tarball),
        ok = file:close(IoDevice),
        ok = file:write_file_info(TmpFilename, FileInfoMod, [{time,universal}]),
        ok = file:rename(TmpFilename, Filename),
        {ok, Filename}
    catch
        Class:Reason ->
            {{error, {exception, Class, Reason}}, Filename}
    end.

-spec bin_to_hex_str(binary()) -> [48..57 | 97..102].
bin_to_hex_str(Bin) ->
    bin_to_hex_str_recur(Bin, []).

-spec bin_to_hex_str_recur(bitstring(), [48..57 | 97..102]) -> [48..57 | 97..102].
bin_to_hex_str_recur(<<Nibble:4, Rest/bits>>, Acc) when Nibble < 10 ->
    bin_to_hex_str_recur(Rest, [$0 + Nibble | Acc]);
bin_to_hex_str_recur(<<Nibble:4, Rest/bits>>, Acc) ->
    bin_to_hex_str_recur(Rest, [$a + Nibble | Acc]);
bin_to_hex_str_recur(<<>>, Acc) ->
    lists:reverse(Acc).

-spec update_period(state_data()) -> pos_integer().
update_period(#{ last_modified := _ } = _StateData) ->
    ?POST_READINESS_UPDATE_PERIOD;
update_period(#{} = _StateData) ->
    ?PRE_READINESS_UPDATE_PERIOD.

request_headers(#{ last_modified := LastModified } = _StateData) ->
    LocalLastModified = calendar:universal_time_to_local_time(LastModified),
    [{"if-modified-since", httpd_util:rfc1123_date(LocalLastModified)}
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

-spec maybe_enqueue_waiter(from(), state_data())
        -> {state_data(), [?gen_statem:reply_action()]}.
maybe_enqueue_waiter(From, #{ last_version := LastVersion } = StateData) ->
    {StateData, [{reply, From, {ok, LastVersion}}]};
maybe_enqueue_waiter(From, StateData) ->
    StateData2 =
        ?maps_update_with3(
          waiters,
          fun (PrevWaiters) -> [From | PrevWaiters] end,
          StateData),
    {StateData2, []}.

-spec reply_to_waiters({ok, calendar:datetime()} | {error, term()}, state_data())
        -> {state_data(), [?gen_statem:reply_action()]}.
reply_to_waiters(Result, StateData) ->
    Waiters = maps:get(waiters, StateData),
    Replies = [{reply, From, Result} || From <- Waiters],
    {StateData#{ waiters := [] }, Replies}.

-spec report_event(event(), state_data()) -> ok.
report_event(Event, #{ id := Id, event_subscribers := Subscribers }) ->
    lists:foreach(
      fun (Module) when is_atom(Module) ->
              Module:report(Id, Event);
          (Pid) ->
              erlang:send(Pid, {locus, Id, Event}, [noconnect])
      end,
      Subscribers).

handle_monitored_process_death(Pid, StateData) ->
    StateData2 =
        ?maps_update_with3(
          event_subscribers,
          fun (Subscribers) -> lists:delete(Pid, Subscribers) end,
          StateData),
    {keep_state, StateData2}.
