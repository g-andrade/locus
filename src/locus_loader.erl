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

-module(locus_loader).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start/3,
    stop/1,
    wait/2,
    start_link/3,
    dynamic_child_spec/1,
    static_child_spec/4
   ]).

-ignore_xref(
   [start_link/3
   ]).

-ifdef(TEST).
-export([whereis/1]).
-export([list_subscribers/1]).
-export([cached_tarball_path_for_url/1]).
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

-define(DEFAULT_HTTP_UNREADY_UPDATE_PERIOD, (timer:minutes(1))).
-define(DEFAULT_HTTP_READY_UPDATE_PERIOD, (timer:hours(6))).

-define(DEFAULT_FS_UNREADY_UPDATE_PEROID, (timer:seconds(5))).
-define(DEFAULT_FS_READY_UPDATE_PERIOD, (timer:seconds(30))).

-define(is_pos_integer(V), ((is_integer((V)) andalso ((V) >= 1)))).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type opt() ::
    {event_subscriber, module() | pid()} |
    {pre_readiness_update_period, pos_integer()} |
    {post_readiness_update_period, pos_integer()} |
    no_cache |
    fetcher_opt() |
    {internal, internal_opt()}.
-export_type([opt/0]).

-type fetcher_opt() :: locus_http_download:opt().
-export_type([fetcher_opt/0]).

-opaque internal_opt() ::
    {async_waiter, {pid(),reference()}}.
-export_type([internal_opt/0]).

-ifdef(POST_OTP_18).
-type static_child_spec() ::
    #{ id := term(),
       start := {?MODULE, start_link, [atom() | origin() | [opt()], ...]},
       restart := permanent,
       shutdown := non_neg_integer(),
       type := worker,
       modules := [?MODULE, ...]
     }.
-else.
-type static_child_spec() ::
    #{ id => term(),
       start => {?MODULE, start_link, [atom() | origin() | [opt()], ...]},
       restart => permanent,
       shutdown => non_neg_integer(),
       type => worker,
       modules => [?MODULE, ...]
     }.
-endif.
-export_type([static_child_spec/0]).

-record(state, {
          id :: atom(),
          origin :: origin(),
          settings :: settings(),
          fetcher_opts :: [fetcher_opt()],

          update_timer :: reference() | undefined,
          last_modified :: calendar:datetime() | undefined,
          last_version :: calendar:datetime() | undefined,

          fetcher_pid :: pid() | undefined,
          fetcher_source :: source() | undefined,

          cacher_pid :: pid() | undefined,
          cacher_path :: locus_filesystem_store:path() | undefined,
          cacher_source :: source() | undefined,

          subscribers :: [module() | pid()],
          subscriber_mons :: #{monitor() => pid()},
          waiters :: [{reference(),pid()}]
         }).
-type state() :: #state{}.
-type monitor() :: reference().

-record(settings, {
          unready_update_period :: pos_integer(),
          ready_update_period :: pos_integer(),
          use_cache :: boolean()
         }).
-type settings() :: #settings{}.

-type origin() ::
        {http, locus_http_download:url()} |
        {filesystem, locus_filesystem_load:path()}.
-export_type([origin/0]).

-type event() ::
        locus_http_download:event() |
        locus_filesystem_load:event() |
        event_load_attempt_finished() |
        event_cache_attempt_finished().
-export_type([event/0]).

-type event_load_attempt_finished() ::
        {load_attempt_finished, source(), {ok, Version :: calendar:datetime()}} |
        {load_attempt_finished, source(), {error, term()}}.
-export_type([event_load_attempt_finished/0]).

-type event_cache_attempt_finished() ::
% TODO
        {cache_attempt_finished, locus_filesystem_store:path(), ok} |
        {cache_attempt_finished, locus_filesystem_store:path(), {error, term()}}.
-export_type([event_cache_attempt_finished/0]).

-type source() ::
        {remote, locus_http_download:url()} |
        locus_filesystem_load:source().
-export_type([source/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start(atom(), origin(), [opt()])
        -> ok |
           {error, already_started} |
           {error, {invalid_opt,term()}}.
%% @private
start(Id, Origin, Opts) ->
    case locus_loader_sup:start_child([Id, Origin, Opts]) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            {error, already_started};
        {error, {invalid_opt,_} = Reason} ->
            {error, Reason}
    end.

-spec stop(atom()) -> ok | {error, not_found}.
%% @private
stop(Id) ->
    ServerName = server_name(Id),
    try gen:stop(ServerName, normal, 5000) of
        ok -> ok
    catch
        exit:noproc -> {error, not_found};
        exit:normal -> ok;
        exit:shutdown -> ok;
        exit:{shutdown,_} -> ok
    end.

-spec wait(atom(), timeout()) -> {ok, calendar:datetime()} | {error, Reason}
        when Reason :: database_unknown | timeout | {loading, term()}.
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
        exit:{noproc, {gen_server,call,[ServerName|_]}} ->
            {error, database_unknown};
        exit:{normal, {gen_server,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{shutdown, {gen_server,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{{shutdown,_Reason}, {gen_server,call, [ServerName|_]}} ->
            {error, database_unknown}
    end.

-spec start_link(atom(), origin(), [opt()]) -> {ok, pid()}.
%% @private
start_link(Id, Origin, Opts) ->
    ServerName = server_name(Id),
    gen_server:start_link({local,ServerName}, ?MODULE, [Id, Origin, Opts], []).

-spec dynamic_child_spec(term()) -> supervisor:child_spec().
%% @private
dynamic_child_spec(ChildId) ->
    #{ id => ChildId,
       start => {?MODULE, start_link, []},
       restart => transient,
       shutdown => timer:seconds(5),
       type => worker,
       modules => [?MODULE]
     }.

-spec static_child_spec(term(), atom(), origin(), [opt()]) -> static_child_spec().
%% @private
static_child_spec(ChildId, DatabaseId, Origin, Opts) ->
    #{ id => ChildId,
       start => {?MODULE, start_link, [DatabaseId, Origin, Opts]},
       restart => permanent,
       shutdown => timer:seconds(5),
       type => worker,
       modules => [?MODULE]
     }.

-ifdef(TEST).
%% @private
whereis(Id) ->
    ServerName = server_name(Id),
    erlang:whereis(ServerName).

%% @private
list_subscribers(Id) ->
    ServerName = server_name(Id),
    State = sys:get_state(ServerName),
    State#state.subscribers.
-endif.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([atom() | origin() | [opt()], ...])
    -> {ok, state()}.
%% @private
init([Id, Origin, Opts]) ->
    _ = process_flag(trap_exit, true),
    init(Id, Origin, Opts).

-spec handle_call(term(), {pid(),reference()}, state())
        -> {reply, {ok,calendar:datetime()}, state()} |
           {noreply, state()} |
           {stop, unexpected_call, state()}.
%% @private
handle_call(wait, From, State) ->
    State2 = maybe_enqueue_waiter(From, State),
    {noreply, State2};
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state())
        -> {stop, unexpected_cast, state()}.
%% @private
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state())
        -> {noreply, state()} |
           {stop, unexpected_info, state()}.
%% @private
handle_info(finish_initialization, State)
  when State#state.update_timer =:= undefined ->
    UpdatedState = finish_initialization(State),
    {noreply, UpdatedState};
handle_info(begin_update, State) ->
    false = erlang:cancel_timer(State#state.update_timer),
    State2 = State#state{ update_timer = undefined },
    State3 = begin_update(State2),
    {noreply, State3};
handle_info({FetcherPid, Msg}, State)
  when FetcherPid =:= State#state.fetcher_pid ->
    handle_fetcher_msg(Msg, State);
handle_info({CacherPid, Msg}, State)
  when CacherPid =:= State#state.cacher_pid ->
    handle_cacher_msg(Msg, State);
handle_info({'DOWN', Ref, process, _, _}, State) ->
    handle_monitored_process_death(Ref, State);
handle_info({'EXIT', Pid, Reason}, State) ->
    handle_linked_process_death(Pid, Reason, State);
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
%% @private
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
%% @private
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Initialization
%% ------------------------------------------------------------------

-spec server_name(atom()) -> atom().
server_name(Id) ->
    list_to_atom(
      atom_to_list(?MODULE)
      ++ "."
      ++ atom_to_list(Id)
     ).

-spec init(atom(), origin(), [opt()])
        -> {ok, state()} |
           {stop, {invalid_opt, term()}}.
init(Id, Origin, Opts) ->
    {FetcherOpts, RemainingOpts} = sieve_fetcher_opts(Origin, Opts),
    DefaultSettings = default_settings(Origin),
    BaseState =
        #state{
           id = Id,
           origin = Origin,
           settings = DefaultSettings,
           fetcher_opts = FetcherOpts,
           subscribers = [],
           subscriber_mons = #{},
           waiters = []
          },
    init_opts(RemainingOpts, BaseState).

sieve_fetcher_opts({http,_}, Opts) ->
    locus_http_download:sieve_opts(Opts);
sieve_fetcher_opts({filesystem,_}, Opts) ->
    {[], Opts}.

default_settings({http,_}) ->
    #settings{
       unready_update_period = ?DEFAULT_HTTP_UNREADY_UPDATE_PERIOD,
       ready_update_period = ?DEFAULT_HTTP_READY_UPDATE_PERIOD,
       use_cache = true
      };
default_settings({filesystem,_}) ->
    #settings{
       unready_update_period = ?DEFAULT_FS_UNREADY_UPDATE_PEROID,
       ready_update_period = ?DEFAULT_FS_READY_UPDATE_PERIOD,
       use_cache = false
      }.

init_opts([{event_subscriber,Module} | Opts], State)
  when is_atom(Module), Module =/= undefined ->
    #state{subscribers = Subscribers} = State,
    UpdatedSubscribers = [Module | Subscribers],
    UpdatedState = State#state{ subscribers = UpdatedSubscribers },
    init_opts(Opts, UpdatedState);
%
init_opts([{event_subscriber,Pid} | Opts], State)
  when is_pid(Pid) ->
    #state{subscribers = Subscribers, subscriber_mons = SubscriberMons} = State,
    Mon = monitor(process, Pid),
    UpdatedSubscribers = [Pid | Subscribers],
    UpdatedSubscriberMons = SubscriberMons#{Mon => Pid},
    UpdatedState = State#state{ subscribers = UpdatedSubscribers,
                                subscriber_mons = UpdatedSubscriberMons },
    init_opts(Opts, UpdatedState);
%
init_opts([{pre_readiness_update_period,Interval} | Opts], State)
  when ?is_pos_integer(Interval) ->
    #state{settings = Settings} = State,
    UpdatedSettings = Settings#settings{ unready_update_period = Interval },
    UpdatedState = State#state{ settings = UpdatedSettings },
    init_opts(Opts, UpdatedState);
%
init_opts([{post_readiness_update_period,Interval} | Opts], State)
  when ?is_pos_integer(Interval) ->
    #state{settings = Settings} = State,
    UpdatedSettings = Settings#settings{ ready_update_period = Interval },
    UpdatedState = State#state{ settings = UpdatedSettings },
    init_opts(Opts, UpdatedState);
%
init_opts([no_cache | Opts], State) ->
    #state{settings = Settings} = State,
    UpdatedSettings = Settings#settings{ use_cache = false },
    UpdatedState = State#state{ settings = UpdatedSettings },
    init_opts(Opts, UpdatedState);
%
init_opts([{internal, {async_waiter, {Pid,Ref}=From}} | Opts], State)
  when is_pid(Pid), is_reference(Ref) ->
    NewState = enqueue_waiter(From, State),
    init_opts(Opts, NewState);
%
init_opts([InvalidOpt | _], _State) ->
    {stop, {invalid_opt, InvalidOpt}};
%
init_opts([], State) ->
    locus_mmdb:create_table(State#state.id),
    self() ! finish_initialization,
    {ok, State}.

finish_initialization(State)
  when (State#state.settings)#settings.use_cache ->
    CachedTarballPath = cached_tarball_path(State),
    Source = {cache,CachedTarballPath},
    {ok, FetcherPid} = locus_filesystem_load:start_link(Source, undefined),
    State#state{ fetcher_pid = FetcherPid, fetcher_source = Source };
finish_initialization(State) ->
    schedule_update(0, State).

schedule_update(Interval, State)
  when State#state.update_timer =:= undefined ->
    NewTimer = erlang:send_after(Interval, self(), begin_update),
    State#state{ update_timer = NewTimer }.

-spec cached_tarball_path(state()) -> nonempty_string().
cached_tarball_path(State) ->
    #state{origin = Origin} = State,
    {http, URL} = Origin,
    cached_tarball_path_for_url(URL).

-spec cached_tarball_path_for_url(string()) -> nonempty_string().
%% @private
cached_tarball_path_for_url(URL) ->
    Hash = crypto:hash(sha256, URL),
    HexHash = locus_util:bin_to_hex_str(Hash),
    Filename = HexHash ++ ".tgz",
    UserCachePath = ?filename_basedir(user_cache, "locus_erlang"),
    filename:join(UserCachePath, Filename).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Database Updates
%% ------------------------------------------------------------------

begin_update(State)
  when State#state.fetcher_pid =:= undefined ->
    #state{origin = Origin,
           fetcher_opts = FetcherOpts,
           last_modified = LastModified} = State,

    case Origin of
        {http, URL} ->
            Headers = http_request_headers(LastModified),
            {ok, FetcherPid} = locus_http_download:start_link(URL, Headers, FetcherOpts),
            State#state{ fetcher_pid = FetcherPid, fetcher_source = {remote,URL} };
        {filesystem, _} = Source ->
            {ok, FetcherPid} = locus_filesystem_load:start_link(Source, LastModified),
            State#state{ fetcher_pid = FetcherPid, fetcher_source = Source }
    end.

http_request_headers(undefined) ->
    http_base_request_headers();
http_request_headers(LastModified) ->
    LocalLastModified = calendar:universal_time_to_local_time(LastModified),
    [{"if-modified-since", httpd_util:rfc1123_date(LocalLastModified)}
     | http_base_request_headers()].

http_base_request_headers() ->
    [{"accept", join_http_header_values(
                  ["application/gzip",
                   "application/x-gzip",
                   "application/x-gtar",
                   "application/x-tgz"])},
     {"content-encoding", "identity"},
     {"connection", "close"}].

join_http_header_values(Values) ->
    string:join(Values, "; ").

handle_fetcher_msg({event,Event}, State) ->
    #state{fetcher_source = Source} = State,
    case Source of
        {cache,_} ->
            {noreply, State};
        _ ->
            report_event(Event, State),
            {noreply, State}
    end;
handle_fetcher_msg({finished,Status}, State) ->
    #state{fetcher_pid = FetcherPid, fetcher_source = Source} = State,
    stop_and_flush_link(FetcherPid),
    UpdatedState = State#state{ fetcher_pid = undefined, fetcher_source = undefined },
    case Status of
        dismissed ->
            handle_database_fetch_dismissal(Source, UpdatedState);
        {success, Success} ->
            handle_database_fetch_success(Source, Success, UpdatedState);
        {error, Reason} ->
            handle_database_fetch_error(Source, Reason, UpdatedState)
    end.

handle_cacher_msg({finished,Status}, State) ->
    #state{cacher_pid = CacherPid, cacher_path = CacherPath, cacher_source = Source} = State,

    stop_and_flush_link(CacherPid),
    UpdatedState = State#state{ cacher_pid = undefined,
                                cacher_path = undefined,
                                cacher_source = undefined },

    case Status of
        success ->
            report_event({cache_attempt_finished, CacherPath, ok}, UpdatedState),
            handle_update_conclusion(Source, UpdatedState);
        {error, Reason} ->
            report_event({cache_attempt_finished, CacherPath, {error,Reason}}, UpdatedState),
            handle_update_conclusion(Source, UpdatedState)
    end.

handle_database_fetch_dismissal(Source, State)
  when State#state.last_modified =/= undefined -> % sanity check
    handle_update_conclusion(Source, State).

handle_database_fetch_success(Source, Success, State) ->
    #state{id = Id} = State,
    Content = fetched_database_content(Source, Success),
    case load_database_from_tarball(Id, Content, Source) of
        {ok, Version} ->
            LastModified = fetched_database_modification_datetime(Source, Success),
            handle_database_load_success(Source, Version, Content, LastModified, State);
        {error, Reason} ->
            handle_database_load_error(Source, Reason, State)
    end.

handle_database_load_success(Source, Version, Content, LastModified, State) ->
    State2 = State#state{ last_modified = LastModified, last_version = Version },
    report_event({load_attempt_finished, Source, {ok, Version}}, State2),
    State3 = reply_to_waiters({ok, Version}, State2),

    case Source of
        {remote,_} when (State3#state.settings)#settings.use_cache ->
            CachedTarballPath = cached_tarball_path(State3),
            {ok, CacherPid} = locus_filesystem_store:start_link(CachedTarballPath, Content, LastModified),
            State4 = State3#state{ cacher_pid = CacherPid,
                                   cacher_path = CachedTarballPath,
                                   cacher_source = Source },
            {noreply, State4};
        _ ->
            handle_update_conclusion(Source, State3)
    end.

handle_database_load_error(Source, Reason, State) ->
    report_event({load_attempt_finished, Source, {error, Reason}}, State),
    UpdatedState = reply_to_waiters({error, Reason}, State),
    handle_update_conclusion(Source, UpdatedState).

handle_database_fetch_error(Source, Reason, State) ->
    report_event({load_attempt_finished, Source, {error, Reason}}, State),

    case {Source,Reason} of
        {{cache,_}, not_found} ->
            handle_update_conclusion(Source, State);
        _ ->
            UpdatedState = reply_to_waiters({error, Reason}, State),
            handle_update_conclusion(Source, UpdatedState)
    end.

handle_update_conclusion(Source, State) ->
    TimeToNextUpdate = time_to_next_update(Source, State),
    UpdatedState = schedule_update(TimeToNextUpdate, State),
    {noreply, UpdatedState}.

time_to_next_update(LastFetchSource, State) ->
    #state{settings = Settings, last_modified = LastModified} = State,
    #settings{ready_update_period = ReadyPeriod, unready_update_period = UnreadyPeriod} = Settings,
    HasVersionLoaded = (LastModified =/= undefined),

    case LastFetchSource of
        {cache,_} ->
            0; % XXX document this exception
        _ when HasVersionLoaded ->
            ReadyPeriod;
        _ ->
            UnreadyPeriod
    end.

-spec load_database_from_tarball(atom(), binary(), locus_mmdb:source())
        -> {ok, calendar:datetime()} |
           {error, {exception, atom(), term(), [term()]}}.
load_database_from_tarball(Id, Tarball, Source) ->
    try
        BinDatabase = extract_database_from_tarball(Tarball),
        Version = locus_mmdb:decode_and_update(Id, BinDatabase, Source),
        {ok, Version}
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            {error, {exception, Class, Reason, Stacktrace}}
    end.

-spec extract_database_from_tarball(binary()) -> binary().
extract_database_from_tarball(Tarball) ->
    {ok, ContainedPaths} = erl_tar:table({binary, Tarball}, [compressed]),
    {true, DatabasePath} = locus_util:lists_anymap(fun has_mmdb_extension/1, ContainedPaths),
    {ok, [{DatabasePath, BinDatabase}]} =
        erl_tar:extract({binary, Tarball}, [{files, [DatabasePath]}, memory, compressed]),
    BinDatabase.

%-spec has_mmdb_extension(nonempty_string()) -> boolean().
has_mmdb_extension({Filename, _Type, _Size, _MTime, _Mode, _Uid, _Gid}) ->
    % FIXME: this a placeholder for OTP 20; due to the incomplete spec
    % of erl_tar:table/2, Dialyzer comes to believe that no strings
    % can be returned, only the above tuple, which in fact is only returned
    % if the 'verbose' option is picked, something that we are definitely
    % not doing.
    filename:extension(Filename) =:= ".mmdb" andalso {true, Filename};
has_mmdb_extension(Filename) ->
    filename:extension(Filename) =:= ".mmdb".

fetched_database_content({remote,_}, #{body := Body}) ->
    Body;
fetched_database_content({cache,_}, #{content := Content}) ->
    Content;
fetched_database_content({filesystem,_}, #{content := Content}) ->
    Content.

fetched_database_modification_datetime({remote,_}, #{headers := Headers}) ->
    CiHeaders = lists:keymap(fun string:to_lower/1, 1, Headers),
    case lists:keyfind("last-modified", 1, CiHeaders) of
        {"last-modified", LastModified} ->
            ({_,_} = ModificationDate) = httpd_util:convert_request_date(LastModified),
            ModificationDate;
        false ->
            {{1970,1,1}, {0,0,0}}
    end;
fetched_database_modification_datetime({cache,_}, #{modified_on := ModificationDate}) ->
    ModificationDate;
fetched_database_modification_datetime({filesystem,_}, #{modified_on := ModificationDate}) ->
    ModificationDate.

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

%% ------------------------------------------------------------------
%% Internal Function Definitions - Monitoring and Subscriptions
%% ------------------------------------------------------------------

-spec report_event(event(), state()) -> ok.
report_event(Event, #state{id = Id, subscribers = Subscribers}) ->
    lists:foreach(
      fun (Module) when is_atom(Module) ->
              Module:report(Id, Event);
          (Pid) ->
              erlang:send(Pid, {locus, Id, Event}, [noconnect])
      end,
      Subscribers).

handle_monitored_process_death(Ref, State) ->
    #state{subscribers = Subscribers, subscriber_mons = SubscriberMons} = State,
    {Pid, UpdatedSubscriberMons} = locus_util:maps_take(Ref, SubscriberMons),
    {ok, UpdatedSubscribers} = locus_util:lists_take(Pid, Subscribers),
    UpdatedState = State#state{ subscribers = UpdatedSubscribers,
                                subscriber_mons = UpdatedSubscriberMons },
    {noreply, UpdatedState}.

handle_linked_process_death(Pid, Reason, State)
  when Pid =:= State#state.fetcher_pid ->
    {stop, {fetcher_stopped, Pid, Reason}, State};
handle_linked_process_death(Pid, Reason, State)
  when Pid =:= State#state.cacher_pid ->
    {stop, {cacher_stopped, Pid, Reason}, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Waiters
%% ------------------------------------------------------------------

-spec maybe_enqueue_waiter({pid(),reference()}, state()) -> state().
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
