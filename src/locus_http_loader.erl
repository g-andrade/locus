%% Copyright (c) 2017-2019 Guilherme Andrade
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
%%
%% locus includes code extracted from OTP source code, by Ericsson AB,
%% released under the Apache License 2.0.

-include("locus_pre_otp19_compat.hrl").

-module(locus_http_loader).
-behaviour(gen_server).

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
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).

-define(DEFAULT_PRE_READINESS_UPDATE_PERIOD, (timer:minutes(1))).
-define(DEFAULT_POST_READINESS_UPDATE_PERIOD, (timer:hours(6))).

-define(is_pos_integer(V), ((is_integer((V)) andalso ((V) >= 1)))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type from() :: {To :: pid(), Tag :: term()}.

-type opt() ::
    {event_subscriber, module() | pid()} |
    {pre_readiness_update_period, pos_integer()} |
    {post_readiness_update_period, pos_integer()} |
    no_cache |
    locus_http_download:opt() |
    {internal, internal_opt()}.
-export_type([opt/0]).

-opaque internal_opt() ::
    {async_waiter, {pid(),reference()}}.
-export_type([internal_opt/0]).

-record(state, {
          id :: atom(),
          url :: url(),
          waiters :: [from()],
          event_subscribers :: [module() | pid()],
          pre_readiness_update_period :: pos_integer(),
          post_readiness_update_period :: pos_integer(),
          no_cache :: boolean(),
          download_opts :: [locus_http_download:opt()],
          update_timer :: reference() | undefined,
          last_modified :: calendar:datetime() | undefined,
          last_version :: calendar:datetime() | undefined,
          download_pid :: pid() | undefined
         }).
-type state() :: #state{}.

-type filename() :: string().
-export_type([filename/0]).

-type url() :: locus_http_download:url().
-export_type([url/0]).

-type response_status() :: locus_http_download:response_status().
-export_type([response_status/0]).

-type headers() :: locus_http_download:headers().
-export_type([headers/0]).

-type body() :: locus_http_download:body().
-export_type([body/0]).

-type event() ::
        locus_http_download:event() |
        event_load_attempt_finished() |
        event_cache_attempt_finished().
-export_type([event/0]).

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
    gen_server:start_link({local, ServerName}, ?CB_MODULE, [Id, URL, Opts], []).

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

-spec init([atom() | string() | [opt()], ...])
        -> {ok, state()}.
%% @private
init([Id, URL, Opts]) ->
    _ = process_flag(trap_exit, true),
    locus_mmdb:create_table(Id),
    init(Id, URL, Opts).

-spec handle_call(term(), {pid(),reference()}, state())
        -> {noreply, state()} |
           {stop, unexpected_call, state()}.
handle_call(wait, From, State) ->
    State2 = maybe_enqueue_waiter(From, State),
    {noreply, State2};
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state())
        -> {stop, unexpected_cast, state()}.
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state())
        -> {noreply, state()} |
           {stop, unexpected_info, state()}.
handle_info(finish_initialization, State)
  when State#state.update_timer =:= undefined ->
    UpdatedState = finish_initialization(State),
    {noreply, UpdatedState};
handle_info(begin_update, State) ->
    false = erlang:cancel_timer(State#state.update_timer),
    State2 = State#state{ update_timer = undefined },
    State3 = begin_update(State2),
    {noreply, State3};
handle_info({DownloadPid, Msg}, State)
  when DownloadPid =:= State#state.download_pid ->
    handle_download_msg(Msg, State);
handle_info({'DOWN', _, process, Pid, _}, State) ->
    handle_monitored_process_death(Pid, State);
handle_info({'EXIT', Pid, Reason}, State) ->
    handle_linked_process_death(Pid, Reason, State);
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

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
    {DownloadOpts, RemainingOpts} = locus_http_download:sieve_opts(Opts),
    BaseState =
        #state{
           id = Id,
           url = URL,
           waiters = [],
           event_subscribers = [],
           pre_readiness_update_period = ?DEFAULT_PRE_READINESS_UPDATE_PERIOD,
           post_readiness_update_period = ?DEFAULT_POST_READINESS_UPDATE_PERIOD,
           no_cache = false,
           download_opts = DownloadOpts
          },
    init_opts(RemainingOpts, BaseState).

init_opts([{event_subscriber, Module} | Opts], State) when is_atom(Module), Module =/= undefined ->
    #state{ event_subscribers = Subscribers } = State,
    UpdatedSubscribers = [Module | Subscribers],
    UpdatedState = State#state{ event_subscribers = UpdatedSubscribers },
    init_opts(Opts, UpdatedState);
init_opts([{event_subscriber, Pid} | Opts], State) when is_pid(Pid) ->
    _ = monitor(process, Pid),
    #state{ event_subscribers = Subscribers } = State,
    UpdatedSubscribers = [Pid | Subscribers],
    UpdatedState = State#state{ event_subscribers = UpdatedSubscribers },
    init_opts(Opts, UpdatedState);
init_opts([{pre_readiness_update_period, Interval} | Opts], State) when ?is_pos_integer(Interval) ->
    NewState = State#state{ pre_readiness_update_period = Interval },
    init_opts(Opts, NewState);
init_opts([{post_readiness_update_period, Interval} | Opts], State) when ?is_pos_integer(Interval) ->
    NewState = State#state{ post_readiness_update_period = Interval },
    init_opts(Opts, NewState);
init_opts([no_cache | Opts], State) ->
    NewState = State#state{ no_cache = true },
    init_opts(Opts, NewState);
init_opts([{internal, {async_waiter, {Pid,Ref}=From}} | Opts], State) when is_pid(Pid), is_reference(Ref) ->
    NewState = enqueue_waiter(From, State),
    init_opts(Opts, NewState);
init_opts([InvalidOpt | _], _State) ->
    {stop, {invalid_opt, InvalidOpt}};
init_opts([], State) ->
    self() ! finish_initialization,
    {ok, State}.

finish_initialization(State) when State#state.no_cache ->
    schedule_update(0, State);
finish_initialization(State) ->
    CachedTarballName = cached_tarball_name(State),
    CachedTarballLookup = locus_util:read_file_and_its_modification_date(CachedTarballName),
    UpdatedState = handle_cached_tarball_lookup(CachedTarballLookup, CachedTarballName, State),
    schedule_update(0, UpdatedState).

schedule_update(State) ->
    case State#state.last_version of
        undefined ->
            schedule_update(State#state.pre_readiness_update_period, State);
        {{_,_,_}, {_,_,_}} ->
            schedule_update(State#state.post_readiness_update_period, State)
    end.

schedule_update(Interval, State)
  when State#state.update_timer =:= undefined ->
    NewTimer = erlang:send_after(Interval, self(), begin_update),
    State#state{ update_timer = NewTimer }.

begin_update(State)
  when State#state.download_pid =:= undefined ->
    #state{url = URL, download_opts = Opts} = State,
    Headers = request_headers(State),
    {ok, DownloadPid} = locus_http_download:start_link(URL, Headers, Opts),
    State#state{ download_pid = DownloadPid }.

handle_download_msg({event,Event}, State) ->
    report_event(Event, State),
    {noreply, State};
handle_download_msg(dismissed, State)
  when State#state.last_version =/= undefined ->
    stop_and_flush_link(State#state.download_pid),
    State2 = State#state{ download_pid = undefined },
    State3 = schedule_update(State2),
    {noreply, State3};
handle_download_msg({finished, Headers, Body}, State) ->
    stop_and_flush_link(State#state.download_pid),
    State2 = State#state{ download_pid = undefined },
    State3 = try_loading_downloaded_database(Headers, Body, State2),
    State4 = schedule_update(State3),
    {noreply, State4};
handle_download_msg({error, Reason}, State) ->
    stop_and_flush_link(State#state.download_pid),
    State2 = State#state{ download_pid = undefined },
    State3 = reply_to_waiters({error, Reason}, State2),
    State4 = schedule_update(State3),
    {noreply, State4}.

try_loading_downloaded_database(Headers, Body, State) ->
    #state{id = Id, url = URL} = State,
    Source = {remote, URL},
    case locus_util:load_database_from_tarball(Id, Body, Source) of
        {ok, Version} ->
            report_event({load_attempt_finished, Source, {ok, Version}}, State),
            LastModified = extract_last_modified_datetime_from_response_headers(Headers),
            State2 = State#state{ last_modified = LastModified, last_version = Version },
            maybe_try_saving_cached_tarball(Body, LastModified, State2),
            reply_to_waiters({ok, Version}, State2);
        {error, Error} ->
            report_event({load_attempt_finished, Source, {error, Error}}, State),
            reply_to_waiters({error, Error}, State)
    end.

handle_linked_process_death(Pid, Reason, State)
  when Pid =:= State#state.download_pid ->
    {stop, {downloader_stopped, Pid, Reason}}.

-spec cached_tarball_name(state()) -> nonempty_string().
cached_tarball_name(State) ->
    #state{url = URL} = State,
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
        -> state()
             when LookupResult :: ({ok, binary(), calendar:datetime()} |
                                   {error, term()}).
handle_cached_tarball_lookup({ok, Content, ModificationDate}, CachedTarballName, State) ->
    #state{ id = Id } = State,
    Source = {cache, CachedTarballName},
    case locus_util:load_database_from_tarball(Id, Content, Source) of
        {ok, Version} ->
            report_event({load_attempt_finished, Source, {ok, Version}}, State),
            State2 = reply_to_waiters({ok, Version}, State),
            State2#state{ last_modified = ModificationDate,
                          last_version = Version };
        {error, Error} ->
            report_event({load_attempt_finished, Source, {error, Error}}, State),
            State
    end;
handle_cached_tarball_lookup({error, Error}, CachedTarballName, State) ->
    Source = {cache, CachedTarballName},
    report_event({load_attempt_finished, Source, {error, Error}}, State),
    State.

-spec maybe_try_saving_cached_tarball(binary(), calendar:datetime(), state()) -> ok.
maybe_try_saving_cached_tarball(_Tarball, _LastModified, #state{no_cache = true}) ->
    ok;
maybe_try_saving_cached_tarball(Tarball, LastModified, State) ->
    case save_cached_tarball(Tarball, LastModified, State) of
        {ok, Filename} ->
            report_event({cache_attempt_finished, Filename, ok}, State);
        {{error, Error}, Filename} ->
            report_event({cache_attempt_finished, Filename, {error, Error}}, State)
    end.

-spec save_cached_tarball(binary(), calendar:datetime(), state())
        -> {Status, Filename} when Status :: ok | {error, Exception},
                                   Filename :: nonempty_string(),
                                   Exception :: {exception, Class, Reason, Stacktrace},
                                   Class :: atom(),
                                   Reason :: term(),
                                   Stacktrace :: [term()].
save_cached_tarball(Tarball, LastModified, State) ->
    Filename = cached_tarball_name(State),
    TmpSuffix = ".tmp." ++ integer_to_list(rand:uniform(1 bsl 32), 36),
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
            Stacktrace = erlang:get_stacktrace(),
            {{error, {exception, Class, Reason, Stacktrace}},
             Filename}
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

request_headers(#state{last_modified = undefined}) ->
    base_request_headers();
request_headers(#state{last_modified = LastModified}) ->
    LocalLastModified = calendar:universal_time_to_local_time(LastModified),
    [{"if-modified-since", httpd_util:rfc1123_date(LocalLastModified)}
     | base_request_headers()].

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

stop_and_flush_link(Pid) ->
    try gen:stop(Pid, normal, 5000) of
        ok ->
            flush_link(Pid)
    catch
        exit:Reason when Reason =:= normal;
                         Reason =:= shutdown;
                         Reason =:= noproc ->
            flush_link(Pid)
    end.

flush_link(Pid) ->
    receive
        {'EXIT', Pid, _} -> true
    after
        0 -> false
    end.

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

-spec maybe_enqueue_waiter(from(), state()) -> state().
maybe_enqueue_waiter(From, #state{last_version = undefined} = State) ->
    enqueue_waiter(From, State);
maybe_enqueue_waiter(From, #state{last_version = LastVersion} = State) ->
    gen_server:reply(From, {ok,LastVersion}),
    State.

enqueue_waiter(From, State) ->
    #state{waiters = Waiters} = State,
    UpdatedWaiters = [From | Waiters],
    State#state{ waiters = UpdatedWaiters }.

-spec reply_to_waiters({ok, calendar:datetime()} | {error, term()}, state()) -> state().
reply_to_waiters(Result, State) ->
    #state{waiters = Waiters} = State,
    lists:foreach(fun (From) -> gen_server:reply(From, Result) end, Waiters),
    State#state{ waiters = [] }.

-spec report_event(event(), state()) -> ok.
report_event(Event, #state{id = Id, event_subscribers = Subscribers}) ->
    lists:foreach(
      fun (Module) when is_atom(Module) ->
              Module:report(Id, Event);
          (Pid) ->
              erlang:send(Pid, {locus, Id, Event}, [noconnect])
      end,
      Subscribers).

handle_monitored_process_death(Pid, State) ->
    #state{event_subscribers = Subscribers} = State,
    {ok, UpdatedSubscribers} = locus_util:lists_take(Pid, Subscribers),
    UpdatedState = State#state{event_subscribers = UpdatedSubscribers},
    {noreply, UpdatedState}.
