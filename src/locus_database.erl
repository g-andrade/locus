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

-module(locus_database).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start/3,
    stop/1,
    start_link/3,
    dynamic_child_spec/1,
    static_child_spec/4,
    async_get_version_or_subscribe/1
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

-ifndef(NO_GEN_SERVER_HIBERNATE_AFTER).
-define(HIBERNATE_AFTER, (timer:seconds(5))).
-endif.

-define(DEFAULT_HTTP_UNREADY_UPDATE_PERIOD, (timer:minutes(1))).
-define(DEFAULT_HTTP_READY_UPDATE_PERIOD, (timer:hours(6))).

-define(DEFAULT_FS_UNREADY_UPDATE_PEROID, (timer:seconds(5))).
-define(DEFAULT_FS_READY_UPDATE_PERIOD, (timer:seconds(30))).

-define(is_pos_integer(V), ((is_integer((V)) andalso ((V) >= 1)))).

%% ------------------------------------------------------------------
%% Record and Type Definitions
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

-record(state, {
          id :: atom(),
          loader_pid :: pid(),
          subscribers :: [atom() | pid()],
          subscriber_mons :: #{monitor() => pid()},
          last_version :: calendar:datetime() | undefined
         }).
-type state() :: #state{}.
-type monitor() :: reference().

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
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start(atom(), origin(), [opt()])
        -> ok |
           {error, already_started} |
           {error, {invalid_opt,term()}} |
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

-spec start_link(atom(), origin(), [opt()]) -> {ok, pid()}.
%% @private
start_link(Id, Origin, Opts) ->
    ServerName = server_name(Id),
    ServerOpts = server_opts(),
    gen_server:start_link({local,ServerName}, ?MODULE, [Id, Origin, Opts], ServerOpts).

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
    -> {ok, state()} |
       {stop, {invalid_opt,term()}}.
%% @private
init([Id, Origin, Opts]) ->
    _ = process_flag(trap_exit, true),
    case validate_opts(Origin, Opts) of
        {ok, {DatabaseOpts, LoaderOpts, FetcherOpts}} ->
            init(Id, Origin, DatabaseOpts, LoaderOpts, FetcherOpts);
        {error, BadOpt} ->
            {stop, {invalid_opt,BadOpt}}
    end.

-spec handle_call(term(), {pid(),reference()}, state())
        -> {stop, unexpected_call, state()}.
%% @private
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state())
        -> {noreply, state()} |
           {stop, unexpected_cast, state()}.
%% @private
handle_cast({get_version_or_subscribe, {Pid, Ref}}, State) ->
    case State#state.last_version of
        undefined ->
            Mon = monitor(process, Pid),
            UpdatedSubscribers = [Pid | State#state.subscribers],
            UpdatedSubscriberMons = maps:put(Mon, Pid, State#state.subscriber_mons),
            UpdatedState = State#state{ subscribers = UpdatedSubscribers,
                                        subscriber_mons = UpdatedSubscriberMons },
            {noreply, UpdatedState};
        LastVersion ->
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
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
%% @private
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec server_name(atom()) -> atom().
server_name(Id) ->
    list_to_atom(
      atom_to_list(?MODULE)
      ++ "."
      ++ atom_to_list(Id)
     ).

-ifndef(NO_GEN_SERVER_HIBERNATE_AFTER).
server_opts() -> [{locus_util:dialyzer_opaque_atom(hibernate_after), ?HIBERNATE_AFTER}].
-else.
server_opts() -> [].
-endif.

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
init_opts([{event_subscriber,Module} | Opts], State)
  when is_atom(Module) ->
    #state{subscribers = Subscribers} = State,
    UpdatedSubscribers = [Module | Subscribers],
    UpdatedState = State#state{ subscribers = UpdatedSubscribers },
    init_opts(Opts, UpdatedState);
init_opts([{event_subscriber,Pid} | Opts], State) ->
    #state{subscribers = Subscribers, subscriber_mons = SubscriberMons} = State,
    Mon = monitor(process, Pid),
    UpdatedSubscribers = [Pid | Subscribers],
    UpdatedSubscriberMons = SubscriberMons#{Mon => Pid},
    UpdatedState = State#state{ subscribers = UpdatedSubscribers,
                                subscriber_mons = UpdatedSubscriberMons },
    init_opts(Opts, UpdatedState);
init_opts([], State) ->
    locus_mmdb:create_table(State#state.id),
    {ok, State}.

-spec handle_loader_msg(locus_loader:msg(), state()) -> {noreply, state()}.
handle_loader_msg({event, Event}, State) ->
    report_event(Event, State),
    {noreply, State};
handle_loader_msg({load_success, Source, Version, Parts}, State) ->
    #state{id = Id} = State,
    locus_mmdb:update(Id, Parts),
    State2 = State#state{ last_version = Version },
    report_event({load_attempt_finished, Source, {ok,Version}}, State),
    {noreply, State2};
handle_loader_msg({load_failure, Source, Reason}, State) ->
    report_event({load_attempt_finished, Source, {error,Reason}}, State),
    {noreply, State}.

-spec report_event(event(), state()) -> ok.
report_event(Event, #state{id = Id, subscribers = Subscribers}) ->
    lists:foreach(
      fun (Module) when is_atom(Module) ->
              Module:report(Id, Event);
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
