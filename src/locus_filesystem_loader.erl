%% Copyright (c) 2018 Guilherme Andrade
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

-module(locus_filesystem_loader).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).                    -ignore_xref({start_link, 3}).
-export([wait/2]).

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

-define(CB_MODULE, ?MODULE).

-define(PRE_READINESS_UPDATE_PERIOD, (timer:seconds(5))).
-define(POST_READINESS_UPDATE_PERIOD, (timer:seconds(30))).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type from() :: {To :: pid(), Tag :: term()}.

-type opt() ::
    {event_subscriber, module() | pid()} |
    {async_waiter, {pid(),reference()}}.
-export_type([opt/0]).

-record(state, {
          id :: atom(),
          path :: filename(),
          waiters :: [from()],
          event_subscribers :: [module() | pid()],
          last_modified :: undefined | calendar:datetime(),
          last_version :: undefined | calendar:datetime()
         }).
-type state() :: #state{}.

-type filename() :: string().
-export_type([filename/0]).

-type event() ::
        event_load_attempt_finished().
-export_type([event/0]).

-type event_load_attempt_finished() ::
        {load_attempt_started, locus_mmdb:filesystem_loader_source()} |
        {load_attempt_dismissed, locus_mmdb:filesystem_loader_source()} |
        {load_attempt_finished, locus_mmdb:filesystem_loader_source(), {ok, Version :: calendar:datetime()}} |
        {load_attempt_finished, locus_mmdb:filesystem_loader_source(), {error, term()}}.
-export_type([event_load_attempt_finished/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(atom(), string(), [opt()]) -> {ok, pid()}.
%% @private
start_link(Id, Path, Opts) ->
    ServerName = server_name(Id),
    gen_server:start_link({local, ServerName}, ?CB_MODULE, [Id, Path, Opts], []).

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

-spec init([atom() | string() | opt(), ...]) -> {ok, state()}.
%% @private
init([Id, Path, Opts]) ->
    locus_rand_compat:seed(),
    locus_mmdb:create_table(Id),
    init(Id, Path, Opts).

-spec handle_call(term(), from(), state())
        -> {reply, {ok, calendar:datetime()}, state()} |
           {noreply, state()} |
           {stop, unexpected_call, state()}.
%% @private
handle_call(wait, From, State) ->
    maybe_enqueue_waiter(From, State);
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
handle_info(attempt_load, State) ->
    attempt_load(State);
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    handle_monitored_process_death(Pid, State);
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
%% @private
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
%% @private
code_change(_OldVsn, OldState, _Extra) ->
    {ok, OldState}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec server_name(atom()) -> atom().
server_name(Id) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Id)).

-spec init(atom(), string(), [opt()]) -> {ok, state()}.
init(Id, Path, Opts) ->
    BaseState =
        #state{
           id = Id,
           path = Path,
           waiters = [],
           event_subscribers = []
          },
    init_opts(Opts, BaseState).

init_opts([{event_subscriber, Module} | Opts], State) when is_atom(Module), Module =/= undefined ->
    EventSubscribers = State#state.event_subscribers,
    UpdatedEventSubscribers = [Module | EventSubscribers],
    UpdatedState = State#state{ event_subscribers = UpdatedEventSubscribers },
    init_opts(Opts, UpdatedState);
init_opts([{event_subscriber, Pid} | Opts], State) when is_pid(Pid) ->
    _ = monitor(process, Pid),
    EventSubscribers = State#state.event_subscribers,
    UpdatedEventSubscribers = [Pid | EventSubscribers],
    UpdatedState = State#state{ event_subscribers = UpdatedEventSubscribers },
    init_opts(Opts, UpdatedState);
init_opts([{async_waiter, {Pid,Ref}=From} | Opts], StateData) when is_pid(Pid), is_reference(Ref) ->
    {noreply, NewStateData} = enqueue_waiter(From, StateData),
    init_opts(Opts, NewStateData);
init_opts([InvalidOpt | _], _StateData) ->
    {stop, {invalid_opt, InvalidOpt}};
init_opts([], State) ->
    self() ! attempt_load,
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Subscribers and Waiters
%% ------------------------------------------------------------------

-spec handle_monitored_process_death(pid(), state()) -> {noreply, state()}.
handle_monitored_process_death(Pid, State) ->
    EventSubscribers = State#state.event_subscribers,
    UpdatedEventSubscribers = lists:delete(Pid, EventSubscribers),
    UpdatedState = State#state{ event_subscribers = UpdatedEventSubscribers },
    {noreply, UpdatedState}.

-spec maybe_enqueue_waiter(from(), state())
        -> {reply, {ok, calendar:datetime()}, state()} |
           {noreply, state()}.
maybe_enqueue_waiter(_From, State) when State#state.last_version =/= undefined ->
    {reply, {ok, State#state.last_version}, State};
maybe_enqueue_waiter(From, State) ->
    enqueue_waiter(From, State).

enqueue_waiter(From, State) ->
    Waiters = State#state.waiters,
    UpdatedWaiters = [From | Waiters],
    UpdatedState = State#state{ waiters = UpdatedWaiters },
    {noreply, UpdatedState}.

-spec report_event(event(), state()) -> ok.
report_event(Event, #state{ id = Id, event_subscribers = Subscribers }) ->
    lists:foreach(
      fun (Module) when is_atom(Module) ->
              Module:report(Id, Event);
          (Pid) ->
              erlang:send(Pid, {locus, Id, Event}, [noconnect])
      end,
      Subscribers).

-spec reply_to_waiters({ok, calendar:datetime()} | {error, term()}, state()) -> state().
reply_to_waiters(Result, State) ->
    lists:foreach(
      fun (From) ->
          gen_server:reply(From, Result)
      end,
      State#state.waiters),
    State#state{ waiters = [] }.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Loading
%% ------------------------------------------------------------------

-spec attempt_load(state()) -> {noreply, state()}.
attempt_load(State) ->
    report_event({load_attempt_started, db_source(State)}, State),
    FileLookup = locus_util:maybe_read_file_and_its_modification_date(
                   State#state.path, State#state.last_modified),
    State2 = handle_file_lookup(FileLookup, State),
    _ = schedule_next_load_attempt(State2),
    {noreply, State2}.

-spec handle_file_lookup({ok, unchanged} |
                         {ok, binary(), calendar:datetime()} |
                         {error, term()},
                         state()) -> state().
handle_file_lookup({ok, unchanged}, State) when State#state.last_version =/= undefined ->
    % ignore waiters, as there can be none
    report_event({load_attempt_dismissed, db_source(State)}, State),
    State;
handle_file_lookup({ok, Content, ModificationDate}, State) ->
    Id = State#state.id,
    Source = db_source(State),
    case locus_util:load_database_from_tarball(Id, Content, Source) of
        {ok, Version} ->
            report_event({load_attempt_finished, Source, {ok, Version}}, State),
            State2 = reply_to_waiters({ok, Version}, State),
            State2#state{ last_modified = ModificationDate,
                          last_version = Version };
        {error, Error} ->
            report_event({load_attempt_finished, Source, {error, Error}}, State),
            reply_to_waiters({error, Error}, State)
    end;
handle_file_lookup({error, Error}, State) ->
    report_event({load_attempt_finished, db_source(State), {error, Error}}, State),
    reply_to_waiters({error, Error}, State).

-spec schedule_next_load_attempt(state()) -> reference().
schedule_next_load_attempt(State) when State#state.last_version =:= undefined ->
    erlang:send_after(?PRE_READINESS_UPDATE_PERIOD, self(), attempt_load);
schedule_next_load_attempt(_State) ->
    erlang:send_after(?POST_READINESS_UPDATE_PERIOD, self(), attempt_load).

-spec db_source(state()) -> locus_mmdb:filesystem_loader_source().
db_source(State) ->
    {filesystem, State#state.path}.
