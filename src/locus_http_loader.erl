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

-module(locus_http_loader).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
-include("locus_pre_otp19_compat.hrl").

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
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    %%
    code_change/4
   ]).

-ignore_xref(
   [code_change/4
   ]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(DEFAULT_HTTP_CONNECT_TIMEOUT, (timer:seconds(8))).
-define(DEFAULT_HTTP_DOWNLOAD_START_TIMEOUT, (timer:seconds(5))).
-define(DEFAULT_HTTP_IDLE_DOWNLOAD_TIMEOUT, (timer:seconds(5))).
-define(DEFAULT_PRE_READINESS_UPDATE_PERIOD, (timer:minutes(1))).
-define(DEFAULT_POST_READINESS_UPDATE_PERIOD, (timer:hours(6))).

-define(is_timeout(V), ((is_integer((V)) andalso ((V) >= 0)) orelse ((V) =:= infinity))).
-define(is_pos_integer(V), ((is_integer((V)) andalso ((V) >= 1)))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type from() :: {To :: pid(), Tag :: term()}.

-type opt() ::
    {event_subscriber, module() | pid()} |
    {connect_timeout, timeout()} |
    {download_start_timeout, timeout()} |
    {idle_download_timeout, timeout()} |
    {pre_readiness_update_period, pos_integer()} |
    {post_readiness_update_period, pos_integer()} |
    no_cache.
-export_type([opt/0]).

-record(state, {
          id :: atom(),
          url :: url(),
          waiters :: [from()],
          event_subscribers :: [module() | pid()],
          connect_timeout :: timeout(),
          download_start_timeout :: timeout(),
          idle_download_timeout :: timeout(),
          pre_readiness_update_period :: pos_integer(),
          post_readiness_update_period :: pos_integer(),
          no_cache :: boolean(),
          %%,
          load_state :: load_state(),
          last_modified :: undefined | calendar:datetime(),
          last_version :: undefined | calendar:datetime()
         }).
-type state() :: #state{}.

-type load_state() ::
        initializing() |
        idle() |
        waiting_stream_start() |
        waiting_stream_end().

-record(initializing, {
         }).
-type initializing() :: #initializing{}.

-record(idle, {
          timeout_timer :: reference()
         }).
-type idle() :: #idle{}.

-record(waiting_stream_start, {
          request_id :: reference(),
          timeout_timer :: reference()
         }).
-type waiting_stream_start() :: #waiting_stream_start{}.

-record(waiting_stream_end, {
          request_id :: reference(),
          response_headers :: headers(),
          response_body :: binary(),
          timeout_timer :: reference()
         }).
-type waiting_stream_end() :: #waiting_stream_end{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    gen_server:start_link({local, ServerName}, ?MODULE, [Id, URL, Opts], []).

-spec wait(atom(), timeout())
        -> {ok, LoadedVersion :: calendar:datetime()} |
           {error, database_unknown | timeout | {loading, term()}}.
%% @private
wait(Id, Timeout) ->
    ServerName = server_name(Id),
    try gen_server:call(ServerName, wait, Timeout) of
        {ok, LoadedVersion} ->
            {ok, LoadedVersion};
        {error, LoadingError} ->
            {error, {loading, LoadingError}}
    catch
        exit:{timeout, {gen_server,call,[ServerName|_]}} when Timeout =/= infinity ->
            {error, timeout};
        %exit:{{nodedown,_RemoteNode}, {gen_server,call,[ServerName|_}} ->
        %    % Cannot happen (loader is always local)
        %    {error, database_unknown};
        exit:{noproc, {gen_server,call,[ServerName|_]}} ->
            {error, database_unknown};
        exit:{normal, {gen_server,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{shutdown, {gen_server,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{{shutdown,_Reason}, {gen_server,call, [ServerName|_]}} ->
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
    State = sys:get_state(ServerName),
    State#state.event_subscribers.
-endif.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([atom() | string() | opt(), ...])
        -> {ok, state()} |
           {stop, {invalid_opt,term()}}.
%% @private
init([Id, URL, Opts]) ->
    locus_rand_compat:seed(),
    locus_mmdb:create_table(Id),
    init(Id, URL, Opts).

-spec handle_call(term(), {pid(), reference()}, state())
        -> {reply, {ok,calendar:datetime()}, state()} |
           {noreply, state()} |
           {stop, unexpected_call, state()}.
%% @private
handle_call(wait, From, State) ->
    maybe_enqueue_waiter(From, State);
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state()) -> {stop, unexpected_cast, state()}.
%% @private
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state())
        -> {noreply, state()} |
           {stop, unexpected_info, state()}.
%% @private
handle_info(maybe_load_from_cache, State)
  when is_record(State#state.load_state, initializing),
       State#state.no_cache ->
    NewLoadState = #idle{ timeout_timer = start_timer(0, idle_timeout) },
    UpdatedState = State#state{ load_state = NewLoadState },
    {noreply, UpdatedState};
handle_info(maybe_load_from_cache, State)
  when is_record(State#state.load_state, initializing) ->
    NewLoadState = #idle{ timeout_timer = start_timer(0, idle_timeout) },
    State2 = State#state{ load_state = NewLoadState },
    CachedTarballName = cached_tarball_name(State2),
    CachedTarballLookup = locus_util:read_file_and_its_modification_date(CachedTarballName),
    State3 = handle_cached_tarball_lookup(CachedTarballLookup, CachedTarballName, State2),
    {noreply, State3};
%%%%%%%%
handle_info(idle_timeout, State)
  when is_record(State#state.load_state, idle) ->
    URL = State#state.url,
    Headers = request_headers(State),
    Request = {URL, Headers},
    ConnectTimeout = State#state.connect_timeout,
    HTTPOptions = [{connect_timeout, ConnectTimeout}],
    Options = [{sync, false}, {stream, self}],
    {ok, RequestId} = httpc:request(get, Request, HTTPOptions, Options),
    true = is_reference(RequestId),
    NewLoadState =
        #waiting_stream_start{
           request_id = RequestId,
           timeout_timer = start_timer(State#state.download_start_timeout, download_start_timeout)
          },
    UpdatedState = State#state{ load_state = NewLoadState },
    report_event({request_sent, URL, Headers}, State),
    {noreply, UpdatedState};
%%%%%%%%
handle_info({http, {RequestId, stream_start, Headers}}, State)
  when RequestId =:= (State#state.load_state)#waiting_stream_start.request_id ->
    report_event({download_started, Headers}, State),
    LoadState = State#state.load_state,
    true = cancel_timer(LoadState#waiting_stream_start.timeout_timer, stream_start_timeout),
    %%
    NewLoadState =
        #waiting_stream_end{
           request_id = LoadState#waiting_stream_start.request_id,
           response_headers = Headers,
           response_body = <<>>,
           timeout_timer = start_timer(State#state.idle_download_timeout, idle_download_timeout)
          },
    UpdatedState = State#state{ load_state = NewLoadState },
    {noreply, UpdatedState};
handle_info({http, {RequestId, {{_HttpVersion, StatusCode, StatusDesc}, Headers, Body}}}, State)
  when RequestId =:= (State#state.load_state)#waiting_stream_start.request_id,
       StatusCode =:= 304 ->
    report_event({download_dismissed, {http, {StatusCode, StatusDesc}, Headers, Body}}, State),
    LoadState = State#state.load_state,
    true = cancel_timer(LoadState#waiting_stream_start.timeout_timer, download_start_timeout),
    %%
    NewLoadState = #idle{ timeout_timer = start_timer(update_period(State), idle_timeout) },
    UpdatedState = State#state{ load_state = NewLoadState },
    {noreply, UpdatedState};
handle_info({http, {RequestId, {{_HttpVersion, StatusCode, StatusDesc}, Headers, Body}}}, State)
  when RequestId =:= (State#state.load_state)#waiting_stream_start.request_id ->
    report_event({download_failed_to_start, {http, {StatusCode, StatusDesc}, Headers, Body}}, State),
    LoadState = State#state.load_state,
    true = cancel_timer(LoadState#waiting_stream_start.timeout_timer, download_start_timeout),
    %%
    State2 = reply_to_waiters({error, {http, StatusCode, StatusDesc}}, State),
    NewLoadState = #idle{ timeout_timer = start_timer(update_period(State), idle_timeout) },
    State3 = State2#state{ load_state = NewLoadState },
    {noreply, State3};
handle_info({http, {RequestId, {error, Reason}}}, State)
  when RequestId =:= (State#state.load_state)#waiting_stream_start.request_id ->
    report_event({download_failed_to_start, {error, Reason}}, State),
    LoadState = State#state.load_state,
    true = cancel_timer(LoadState#waiting_stream_start.timeout_timer, download_start_timeout),
    %%
    State2 = reply_to_waiters({error, {http, Reason}}, State),
    NewLoadState = #idle{ timeout_timer = start_timer(update_period(State), idle_timeout) },
    State3 = State2#state{ load_state = NewLoadState },
    {noreply, State3};
handle_info(download_start_timeout, State)
  when is_record(State#state.load_state, waiting_stream_start) ->
    report_event({download_failed_to_start, timeout}, State),
    State2 = reply_to_waiters({error, {timeout, waiting_stream_start}}, State),
    LoadState = State#state.load_state,
    false = cancel_timer(LoadState#waiting_stream_start.timeout_timer, download_start_timeout),
    %%
    RequestId = LoadState#waiting_stream_start.request_id,
    ok = httpc:cancel_request(RequestId),
    clear_inbox_of_late_http_messages(RequestId),
    NewLoadState = #idle{ timeout_timer = start_timer(update_period(State), idle_timeout) },
    State3 = State2#state{ load_state = NewLoadState },
    {noreply, State3};
%%%%%%%%
handle_info({http, {RequestId, stream, BinBodyPart}}, State)
  when RequestId =:= (State#state.load_state)#waiting_stream_end.request_id ->
    LoadState = State#state.load_state,
    true = cancel_timer(LoadState#waiting_stream_end.timeout_timer, idle_download_timeout),
    %%
    BodyAcc = LoadState#waiting_stream_end.response_body,
    UpdatedBodyAcc = <<BodyAcc/binary, BinBodyPart/binary>>,
    UpdatedLoadState =
        LoadState#waiting_stream_end{
          response_body = UpdatedBodyAcc,
          timeout_timer = start_timer(State#state.idle_download_timeout, idle_download_timeout)
         },
    UpdatedState = State#state{ load_state = UpdatedLoadState },
    {noreply, UpdatedState};
handle_info({http, {RequestId, stream_end, ExtraHeaders}}, State) % no chunked encoding
  when RequestId =:= (State#state.load_state)#waiting_stream_end.request_id ->
    LoadState = State#state.load_state,
    true = cancel_timer(LoadState#waiting_stream_end.timeout_timer, idle_download_timeout),
    %%
    ResponseHeaders = lists:usort(LoadState#waiting_stream_end.response_headers ++ ExtraHeaders),
    ResponseBody = LoadState#waiting_stream_end.response_body,
    ResponseBodySize = byte_size(ResponseBody),
    report_event({download_finished, ResponseBodySize, {ok, ExtraHeaders}}, State),
    State2 = process_update(ResponseHeaders, ResponseBody, State),
    NewLoadState = #idle{ timeout_timer = start_timer(update_period(State2), idle_timeout) },
    State3 = State2#state{ load_state = NewLoadState },
    {noreply, State3};
handle_info({http, {RequestId, {error, Reason}}}, State)
  when RequestId =:= (State#state.load_state)#waiting_stream_end.request_id ->
    LoadState = State#state.load_state,
    true = cancel_timer(LoadState#waiting_stream_end.timeout_timer, idle_download_timeout),
    ResponseBodySoFar = LoadState#waiting_stream_end.response_body,
    ResponseBodySizeSoFar = byte_size(ResponseBodySoFar),
    report_event({download_finished, ResponseBodySizeSoFar, {error, Reason}}, State),
    %%
    State2 = reply_to_waiters({error, {http, Reason}}, State),
    NewLoadState = #idle{ timeout_timer = start_timer(update_period(State2), idle_timeout) },
    State3 = State2#state{ load_state = NewLoadState },
    {noreply, State3};
handle_info(idle_download_timeout, State)
  when is_record(State#state.load_state, waiting_stream_end) ->
    LoadState = State#state.load_state,
    false = cancel_timer(LoadState#waiting_stream_end.timeout_timer, idle_download_timeout),
    ResponseBodySoFar = LoadState#waiting_stream_end.response_body,
    ResponseBodySizeSoFar = byte_size(ResponseBodySoFar),
    report_event({download_finished, ResponseBodySizeSoFar, {error, timeout}}, State),
    %%
    RequestId = LoadState#waiting_stream_end.request_id,
    ok = httpc:cancel_request(RequestId),
    clear_inbox_of_late_http_messages(RequestId),
    State2 = reply_to_waiters({error, {timeout, waiting_stream_end}}, State),
    NewLoadState = #idle{ timeout_timer = start_timer(update_period(State2), idle_timeout) },
    State3 = State2#state{ load_state = NewLoadState },
    {noreply, State3};
%%%%%%%%
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    handle_monitored_process_death(Pid, State);
%%%%%%%%
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
%% @private
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
%% @private
code_change(_OldVsn, State, _Extra) when is_record(State, state) ->
    {ok, State}.

-spec code_change(term(), atom(), map(), term()) -> no_return().
%% @private
code_change(_OldVsn, OldState, OldStateData, _Extra)
  when is_atom(OldState), is_map(OldStateData) ->
    % old version which used gen_statem with maps for state; force restart
    % (not sure if this will actually work)
    exit({shutdown, upgrading}).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec server_name(atom()) -> atom().
server_name(Id) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Id)).

-spec init(atom(), string(), [opt()])
        -> {ok, state()} |
           {stop, {invalid_opt, term()}}.
init(Id, URL, Opts) ->
    BaseState =
        #state{
           id = Id,
           url = URL,
           waiters = [],
           event_subscribers = [],
           connect_timeout = ?DEFAULT_HTTP_CONNECT_TIMEOUT,
           download_start_timeout = ?DEFAULT_HTTP_DOWNLOAD_START_TIMEOUT,
           idle_download_timeout = ?DEFAULT_HTTP_IDLE_DOWNLOAD_TIMEOUT,
           pre_readiness_update_period = ?DEFAULT_PRE_READINESS_UPDATE_PERIOD,
           post_readiness_update_period = ?DEFAULT_POST_READINESS_UPDATE_PERIOD,
           no_cache = false,
           load_state = #initializing{}
          },
    init_opts(Opts, BaseState).

init_opts([{event_subscriber, Module} | NextOpts], State) when is_atom(Module), Module =/= undefined ->
    EventSubscribers = State#state.event_subscribers,
    UpdatedEventSubscribers = [Module | EventSubscribers],
    UpdatedState = State#state{ event_subscribers = UpdatedEventSubscribers },
    init_opts(NextOpts, UpdatedState);
init_opts([{event_subscriber, Pid} | NextOpts], State) when is_pid(Pid) ->
    _ = monitor(process, Pid),
    EventSubscribers = State#state.event_subscribers,
    UpdatedEventSubscribers = [Pid | EventSubscribers],
    UpdatedState = State#state{ event_subscribers = UpdatedEventSubscribers },
    init_opts(NextOpts, UpdatedState);
init_opts([{connect_timeout, Timeout} | NextOpts], State) when ?is_timeout(Timeout) ->
    UpdatedState = State#state{ connect_timeout = Timeout },
    init_opts(NextOpts, UpdatedState);
init_opts([{download_start_timeout, Timeout} | NextOpts], State) when ?is_timeout(Timeout) ->
    UpdatedState = State#state{ download_start_timeout = Timeout },
    init_opts(NextOpts, UpdatedState);
init_opts([{idle_download_timeout, Timeout} | NextOpts], State) when ?is_timeout(Timeout) ->
    UpdatedState = State#state{ idle_download_timeout = Timeout },
    init_opts(NextOpts, UpdatedState);
init_opts([{pre_readiness_update_period, Interval} | NextOpts], State) when ?is_pos_integer(Interval) ->
    UpdatedState = State#state{ pre_readiness_update_period = Interval },
    init_opts(NextOpts, UpdatedState);
init_opts([{post_readiness_update_period, Interval} | NextOpts], State) when ?is_pos_integer(Interval) ->
    UpdatedState = State#state{ post_readiness_update_period = Interval },
    init_opts(NextOpts, UpdatedState);
init_opts([no_cache | Opts], State) ->
    UpdatedState = State#state{ no_cache = true },
    init_opts(Opts, UpdatedState);
init_opts([InvalidOpt | _], _State) ->
    {stop, {invalid_opt, InvalidOpt}};
init_opts([], State) ->
    self() ! maybe_load_from_cache,
    {ok, State}.

start_timer(Interval, Message) ->
    erlang:send_after(Interval, self(), Message).

cancel_timer(Timer, Message) ->
    is_integer( erlang:cancel_timer(Timer) )
    orelse receive
               Message -> true
           after
               0 -> false
           end.

-spec cached_tarball_name(state()) -> nonempty_string().
cached_tarball_name(State) ->
    URL = State#state.url,
    cached_tarball_name_for_url(URL).

-spec cached_tarball_name_for_url(string()) -> nonempty_string().
%% @private
cached_tarball_name_for_url(URL) ->
    Hash = crypto:hash(sha256, URL),
    HexHash = bin_to_hex_str(Hash),
    Filename = HexHash ++ ".tgz",
    UserCachePath = ?filename_basedir(user_cache, "locus_erlang"),
    filename:join(UserCachePath, Filename).

-spec handle_cached_tarball_lookup(LookupResult, nonempty_string(), state())
        -> state() when LookupResult :: ({ok, binary(), calendar:datetime()} |
                                              {error, term()}).
handle_cached_tarball_lookup({ok, Content, ModificationDate}, CachedTarballName, State) ->
    Id = State#state.id,
    Source = {cache, CachedTarballName},
    case locus_util:load_database_from_tarball(Id, Content, Source) of
        {ok, Version} ->
            report_event({load_attempt_finished, Source, {ok, Version}}, State),
            State#state{ last_modified = ModificationDate,
                         last_version = Version
                       };
        {error, Error} ->
            report_event({load_attempt_finished, Source, {error, Error}}, State),
            State
    end;
handle_cached_tarball_lookup({error, Error}, CachedTarballName, State) ->
    Source = {cache, CachedTarballName},
    report_event({load_attempt_finished, Source, {error, Error}}, State),
    State.

-spec maybe_try_saving_cached_tarball(binary(), calendar:datetime(), state()) -> ok.
maybe_try_saving_cached_tarball(_Tarball, _LastModified, State)
  when State#state.no_cache ->
    ok;
maybe_try_saving_cached_tarball(Tarball, LastModified, State) ->
    case save_cached_tarball(Tarball, LastModified, State) of
        {ok, Filename} ->
            report_event({cache_attempt_finished, Filename, ok}, State);
        {{error, Error}, Filename} ->
            report_event({cache_attempt_finished, Filename, {error, Error}}, State)
    end.

-spec save_cached_tarball(binary(), calendar:datetime(), state())
        -> {Status, Filename} when Status :: ok | {error, {exception, atom(), term()}},
                                   Filename :: nonempty_string().
save_cached_tarball(Tarball, LastModified, State) ->
    Filename = cached_tarball_name(State),
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

update_period(State) ->
    case State#state.last_modified of
        undefined ->
            State#state.pre_readiness_update_period;
        _LastModified ->
            State#state.post_readiness_update_period
    end.

request_headers(State) ->
    case State#state.last_modified of
        undefined ->
            base_request_headers();
        LastModified ->
            LocalLastModified = calendar:universal_time_to_local_time(LastModified),
            [{"if-modified-since", httpd_util:rfc1123_date(LocalLastModified)}
             | base_request_headers()]
    end.

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

process_update(Headers, Body, State) ->
    Id = State#state.id,
    URL = State#state.url,
    Source = {remote, URL},
    case locus_util:load_database_from_tarball(Id, Body, Source) of
        {ok, Version} ->
            report_event({load_attempt_finished, Source, {ok, Version}}, State),
            LastModified = extract_last_modified_datetime_from_response_headers(Headers),
            State2 = State#state{
                       last_modified = LastModified,
                       last_version = Version
                      },
            maybe_try_saving_cached_tarball(Body, LastModified, State2),
            reply_to_waiters({ok, Version}, State2);
        {error, Reason} ->
            report_event({load_attempt_finished, Source, {error, Reason}}, State),
            reply_to_waiters({error, Reason}, State)
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

maybe_enqueue_waiter(From, State) ->
    case State#state.last_version of
        undefined ->
            Waiters = State#state.waiters,
            UpdatedWaiters = [From | Waiters],
            UpdatedState = State#state{ waiters = UpdatedWaiters },
            {noreply, UpdatedState};
        LastVersion ->
            {reply, {ok, LastVersion}, State}
    end.

-spec reply_to_waiters({ok, calendar:datetime()} | {error, term()}, state()) -> state().
reply_to_waiters(Result, State) ->
    Waiters = State#state.waiters,
    lists:foreach(fun (Waiter) -> gen_server:reply(Waiter, Result) end, Waiters),
    State#state{ waiters = [] }.

-spec report_event(event(), state()) -> ok.
report_event(Event, State) ->
    Id = State#state.id,
    lists:foreach(
      fun (Module) when is_atom(Module) ->
              Module:report(Id, Event);
          (Pid) ->
              erlang:send(Pid, {locus, Id, Event}, [noconnect])
      end,
      State#state.event_subscribers).

handle_monitored_process_death(Pid, State) ->
    Subscribers = State#state.event_subscribers,
    UpdatedSubscribers = lists:delete(Pid, Subscribers),
    UpdatedState = State#state{ event_subscribers = UpdatedSubscribers },
    {noreply, UpdatedState}.
