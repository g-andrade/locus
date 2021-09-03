%% Copyright (c) 2017-2021 Guilherme Andrade
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

%% @doc Launches `locus_database_loader' and later shares the unpacked database,
%% as well as any updates to it, through `persistent_term'.
-module(locus_database).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [version/1
   ]).

%% ------------------------------------------------------------------
%% "Private" API Function Exports
%% ------------------------------------------------------------------

-export(
   [start/3,
    stop/2,
    start_link/3,
    dynamic_child_spec/1,
    static_child_spec/4,
    async_get_version_or_subscribe/1,
    find/1
   ]).

-ignore_xref(
   [start_link/3
   ]).

-ifdef(TEST).
-export([whereis/1]).
-export([list_subscribers/1]).
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

-define(HIBERNATE_AFTER, (timer:seconds(5))).

-define(SHARED_STATE_KEY(DatabaseId), {'locus_database.shared_state', DatabaseId}).

%% ------------------------------------------------------------------
%% API Record and Type Definitions
%% ------------------------------------------------------------------

-type opt() ::
    database_opt() |
    locus_loader:opt().
-export_type([opt/0]).

-type database_opt() :: {event_subscriber, atom() | pid()}.
-export_type([database_opt/0]).

-type static_child_spec() ::
    #{ id := term(),
       start := {?MODULE, start_link, [atom() | origin() | [opt()], ...]},
       restart := permanent,
       shutdown := non_neg_integer(),
       type := worker,
       modules := [?MODULE, ...]
     }.
-export_type([static_child_spec/0]).

-type origin() :: locus_loader:origin().
-export_type([origin/0]).

-type event() ::
    locus_loader:event() |
    event_load_attempt_finished().
-export_type([event/0]).

-type event_load_attempt_finished() ::
    {load_attempt_finished, locus_loader:source(), {ok, Version :: calendar:datetime()}} |
    {load_attempt_finished, locus_loader:source(), {error, term()}}.
-export_type([event_load_attempt_finished/0]).

%% ------------------------------------------------------------------
%% Internal Record and Type Definitions
%% ------------------------------------------------------------------

-record(state, {
          id :: atom(),
          loader_pid :: pid(),
          subscribers :: [atom() | pid()],
          subscriber_mons :: #{monitor() => pid()}
         }).
-type state() :: #state{}.
-type monitor() :: reference().

-record(shared_state, {
          database :: locus_mmdb:database(),
          source :: locus_loader:source(),
          version :: calendar:datetime()
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Returns the database version based on its build epoch (UNIX timestamp)
-spec version(BuildEpoch) -> Version
        when BuildEpoch :: non_neg_integer(),
             Version :: calendar:datetime().
version(BuildEpoch) ->
    GregorianEpoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    calendar:gregorian_seconds_to_datetime(GregorianEpoch + BuildEpoch).

%% ------------------------------------------------------------------
%% "Private" API Function Definitions
%% ------------------------------------------------------------------

-spec start(atom(), origin(), [opt()])
        -> ok |
           {error, already_started} |
           {error, {invalid_opt, term()}} |
           {error, application_not_running}.
%% @private
start(Id, Origin, Opts) ->
    case locus_database_sup:start_child([Id, Origin, Opts]) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            {error, already_started};
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(atom(), term()) -> ok | {error, not_found}.
%% @private
stop(Id, Reason) ->
    ServerName = server_name(Id),
    try gen:stop(ServerName, Reason, 5000) of
        ok -> ok
    catch
        exit:noproc -> {error, not_found};
        exit:normal -> ok;
        exit:shutdown -> ok;
        exit:{shutdown, _} -> ok
    end.

-spec start_link(atom(), origin(), [opt()]) -> {ok, pid()}.
%% @private
start_link(Id, Origin, Opts) ->
    ServerName = server_name(Id),
    ServerOpts = [{hibernate_after, ?HIBERNATE_AFTER}],
    gen_server:start_link({local, ServerName}, ?MODULE, [Id, Origin, Opts], ServerOpts).

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

-spec async_get_version_or_subscribe(atom()) -> {await, reference()}.
%% @private
async_get_version_or_subscribe(Id) ->
    ServerName = server_name(Id),
    ServerPid = erlang:whereis(ServerName),
    Ref = monitor(process, ServerPid),
    gen_server:cast(ServerPid, {get_version_or_subscribe, {self(), Ref}}),
    {await, Ref}.

-spec find(atom()) -> {ok, locus_mmdb:database(), locus_loader:source(), calendar:datetime()}
                      | {error, database_not_loaded}
                      | {error, database_unknown}.
%% @private
find(Id) ->
    case find_shared_state(Id) of
        {ok, #shared_state{database = Database, source = Source, version = Version}} ->
            {ok, Database, Source, Version};
        error ->
            ServerName = server_name(Id),
            DoesLoaderExist = (erlang:whereis(ServerName) =/= undefined),
            ErrorReason = maps:get(DoesLoaderExist, #{true => database_not_loaded,
                                                      false => database_unknown}),
            {error, ErrorReason}
    end.

-ifdef(TEST).
whereis(Id) ->
    ServerName = server_name(Id),
    erlang:whereis(ServerName).

list_subscribers(Id) ->
    ServerName = server_name(Id),
    State = sys:get_state(ServerName),
    State#state.subscribers.
-endif.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([atom() | origin() | [opt()], ...])
    -> {ok, state()} |
       {stop, {invalid_opt, term()}}.
%% @private
init([Id, Origin, Opts]) ->
    _ = process_flag(trap_exit, true),
    case validate_opts(Origin, Opts) of
        {ok, {DatabaseOpts, LoaderOpts, FetcherOpts}} ->
            init(Id, Origin, DatabaseOpts, LoaderOpts, FetcherOpts);
        {error, BadOpt} ->
            {stop, {invalid_opt, BadOpt}}
    end.

-spec handle_call(term(), {pid(), reference()}, state())
        -> {stop, unexpected_call, state()}.
%% @private
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state())
        -> {noreply, state()} |
           {stop, unexpected_cast, state()}.
%% @private
handle_cast({get_version_or_subscribe, {Pid, Ref}}, State) ->
    case find_shared_state(State#state.id) of
        error ->
            Mon = monitor(process, Pid),
            UpdatedSubscribers = [Pid | State#state.subscribers],
            UpdatedSubscriberMons = maps:put(Mon, Pid, State#state.subscriber_mons),
            UpdatedState = State#state{ subscribers = UpdatedSubscribers,
                                        subscriber_mons = UpdatedSubscriberMons },
            {noreply, UpdatedState};
        {ok, #shared_state{version = LastVersion}} ->
            _ = Pid ! {Ref, {version, LastVersion}},
            {noreply, State}
    end;
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state())
        -> {noreply, state()} |
           {stop, term(), state()}.
%% @private
handle_info({LoaderPid, Msg}, State)
  when LoaderPid =:= State#state.loader_pid ->
    handle_loader_msg(Msg, State);
handle_info({'DOWN', Ref, process, _, _}, State) ->
    handle_monitored_process_death(Ref, State);
handle_info({'EXIT', Pid, Reason}, State) ->
    handle_linked_process_death(Pid, Reason, State);
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
%% @private
terminate(Reason, State) ->
    _ = (locus_util:is_termination_reason_harmless(Reason)
         % Avoid erasing shared state if we're in a crash cycle
         % (as frequent restarts could put quite a strain on
         %  the GC and/or memory consumption.)
         andalso erase_shared_state(State#state.id)),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
%% @private
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Initialization and Event Handling
%% ------------------------------------------------------------------

-spec server_name(atom()) -> atom().
server_name(Id) ->
    list_to_atom(
      atom_to_list(?MODULE)
      ++ "."
      ++ atom_to_list(Id)
     ).

-spec validate_opts(origin(), list())
        -> {ok, {[database_opt()], [locus_loader:loader_opt()], [locus_loader:fetcher_opt()]}} |
           {error, term()}.
validate_opts(Origin, Opts) ->
    case locus_loader:validate_opts(Origin, Opts) of
        {ok, {LoaderOpts, FetcherOpts, DatabaseOpts}} ->
            validate_database_opts(DatabaseOpts, LoaderOpts, FetcherOpts);
        {error, BadOpt} ->
            {error, BadOpt}
    end.

-spec validate_database_opts(list(), [locus_loader:loader_opt()], [locus_loader:fetcher_opt()])
        -> {ok, {[database_opt()], [locus_loader:loader_opt()], [locus_loader:fetcher_opt()]}} |
           {error, term()}.
validate_database_opts(DatabaseOpts, LoaderOpts, FetcherOpts) ->
    case
        locus_util:lists_anymap(
          fun ({event_subscriber, Module}) when is_atom(Module) ->
                  Module =:= undefined;
              ({event_subscriber, Pid}) ->
                  not is_pid(Pid);
              (_) ->
                  true
          end,
          DatabaseOpts)
    of
        {true, BadOpt} ->
            {error, BadOpt};
        false ->
            {ok, {DatabaseOpts, LoaderOpts, FetcherOpts}}
    end.

-spec init(atom(), origin(), [database_opt()], [locus_loader:loader_opt()],
           [locus_loader:fetcher_opt()]) -> {ok, state()}.
init(Id, Origin, DatabaseOpts, LoaderOpts, FetcherOpts) ->
    {ok, LoaderPid} = locus_loader:start_link(Id, Origin, LoaderOpts, FetcherOpts),
    BaseState =
        #state{
           id = Id,
           loader_pid = LoaderPid,
           subscribers = [],
           subscriber_mons = #{}
          },
    init_opts(DatabaseOpts, BaseState).

-spec init_opts([database_opt()], state()) -> {ok, state()}.
init_opts([{event_subscriber, Module} | Opts], State)
  when is_atom(Module) ->
    #state{subscribers = Subscribers} = State,
    UpdatedSubscribers = [Module | Subscribers],
    UpdatedState = State#state{ subscribers = UpdatedSubscribers },
    init_opts(Opts, UpdatedState);
init_opts([{event_subscriber, Pid} | Opts], State) ->
    #state{subscribers = Subscribers, subscriber_mons = SubscriberMons} = State,
    Mon = monitor(process, Pid),
    UpdatedSubscribers = [Pid | Subscribers],
    UpdatedSubscriberMons = SubscriberMons#{Mon => Pid},
    UpdatedState = State#state{ subscribers = UpdatedSubscribers,
                                subscriber_mons = UpdatedSubscriberMons },
    init_opts(Opts, UpdatedState);
init_opts([], State) ->
    _ = process_flag(trap_exit, true), % ensure `:terminate/2' is called (unless killed)
    {ok, State}.

-spec handle_loader_msg(locus_loader:msg(), state()) -> {noreply, state()}.
handle_loader_msg({event, Event}, State) ->
    report_event(Event, State),
    {noreply, State};
handle_loader_msg({load_success, Source, Version, Database}, State) ->
    update_shared_state(State#state.id, Database, Source, Version),
    report_event({load_attempt_finished, Source, {ok, Version}}, State),
    {noreply, State};
handle_loader_msg({load_failure, Source, Reason}, State) ->
    report_event({load_attempt_finished, Source, {error, Reason}}, State),
    {noreply, State}.

-spec report_event(event(), state()) -> ok.
report_event(Event, #state{id = Id, subscribers = Subscribers}) ->
    lists:foreach(
      fun (Module) when is_atom(Module) ->
              locus_event_subscriber:report(Module, Id, Event);
          (Pid) ->
              erlang:send(Pid, {locus, Id, Event}, [noconnect])
      end,
      Subscribers).

-spec handle_monitored_process_death(monitor(), state()) -> {noreply, state()}.
handle_monitored_process_death(Ref, State) ->
    #state{subscribers = Subscribers, subscriber_mons = SubscriberMons} = State,
    {Pid, UpdatedSubscriberMons} = maps:take(Ref, SubscriberMons),
    {ok, UpdatedSubscribers} = locus_util:lists_take(Pid, Subscribers),
    UpdatedState = State#state{ subscribers = UpdatedSubscribers,
                                subscriber_mons = UpdatedSubscriberMons },
    {noreply, UpdatedState}.

-spec handle_linked_process_death(pid(), term(), state())
        -> {stop, {loader_stopped, pid(), term()}, state()}.
handle_linked_process_death(Pid, Reason, State)
  when Pid =:= State#state.loader_pid ->
    {stop, {loader_stopped, Pid, Reason}, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Shared State Management
%% ------------------------------------------------------------------

erase_shared_state(DatabaseId) ->
    Key = ?SHARED_STATE_KEY(DatabaseId),
    persistent_term:erase(Key).

find_shared_state(DatabaseId) ->
    Key = ?SHARED_STATE_KEY(DatabaseId),
    try persistent_term:get(Key) of
        SharedState ->
            {ok, SharedState}
    catch
        error:badarg ->
            error
    end.

update_shared_state(DatabaseId, Database, Source, Version) ->
    Key = ?SHARED_STATE_KEY(DatabaseId),
    SharedState = #shared_state{database = Database, source = Source, version = Version},
    persistent_term:put(Key, SharedState).

%% ------------------------------------------------------------------
%% Unit Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

large_binaries_in_gcollected_persistent_term_are_copied_by_ref_test() ->
    %
    % Confirm that the VM behaves as we expect it to - otherwise
    % using `persistent_term' to publish `#shared_state{}' would be
    % a giant foot gun.
    %
    Key = {?MODULE, ?FUNCTION_NAME, make_ref()},

    Randomizer = rand:uniform(1024),
    BinarySize = 1024 * 1024, % large enough to be ref counted
    Binary = <<0:BinarySize/integer-unit:8>>,
    SubBinarySize = 1024 + Randomizer,
    <<SubBinary:SubBinarySize/bytes, _/bytes>> = Binary,
    persistent_term:put(Key, SubBinary),

    TestPid = self(),
    HelperPid
        = spawn_link(
            fun () ->
                    RetrievedSubBinary = persistent_term:get(Key),
                    TestPid ! ready_to_check,
                    receive
                        check_it ->
                            erlang:garbage_collect(), % just to be sure
                            ?assertEqual(BinarySize,
                                         binary:referenced_byte_size(RetrievedSubBinary))
                    end
            end),

    receive ready_to_check -> ok end,
    RetrievedSubBinary = persistent_term:get(Key),
    persistent_term:put(Key, updated_value),
    HelperPid ! check_it,
    erlang:garbage_collect(), % just to be sure

    ?assertEqual(BinarySize, binary:referenced_byte_size(RetrievedSubBinary)).

shared_state_is_cleared_when_gracefully_stopped_test() ->
    {ok, _} = application:ensure_all_started(locus),
    PrevTrapExit = process_flag(trap_exit, true),

    try
        Id = ?FUNCTION_NAME,
        launch_database_loader_and_await_it(Id),
        ?assertMatch({ok, #shared_state{}}, find_shared_state(Id)),
        ok = stop(Id, _Reason = normal),
        ?assertMatch(error, find_shared_state(Id))
    after
        true = process_flag(trap_exit, PrevTrapExit)
    end.

shared_state_is_kept_when_crashed_test() ->
    {ok, _} = application:ensure_all_started(locus),
    PrevTrapExit = process_flag(trap_exit, true),

    try
        Id = ?FUNCTION_NAME,
        launch_database_loader_and_await_it(Id),
        ?assertMatch({ok, #shared_state{}}, find_shared_state(Id)),
        ok = stop(Id, _Reason = {shutdown, simulated_crash}),
        ?assertMatch({ok, #shared_state{}}, find_shared_state(Id))
    after
        true = process_flag(trap_exit, PrevTrapExit)
    end.

launch_database_loader_and_await_it(Id) ->
    Origin = {filesystem, "test/priv/GeoLite2-Country.mmdb"},
    {ok, _Pid} = start_link(Id, Origin, _Opts = []),
    {await, Ref} = async_get_version_or_subscribe(Id),
    {ok, _} = wait_for_database_to_load(Ref, Id),
    ok.

wait_for_database_to_load(Ref, Id) ->
    receive
        {Ref, {version, _Version}} ->
            ok;
        {locus, Id, {load_attempt_finished, _, Result}} ->
            {ok, Result};
        {locus, Id, _Event} ->
            % ct:pal("~p", [Event]),
            wait_for_database_to_load(Ref, Id);
        {'DOWN', Ref, process, _, Reason} ->
            exit(Reason)
    end.

-endif. % -ifdef(TEST).
