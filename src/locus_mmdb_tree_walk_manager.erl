%% Copyright (c) 2021-2022 Guilherme Andrade
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
-module(locus_mmdb_tree_walk_manager).
-behaviour(gen_server).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [new_counters/0,
    start_link/2,
    new_handle/3,
    take_index/1,
    maybe_give_index/3,
    stop/1
   ]).

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

-define(FREE_WORKERS_COUNTER, 1).
-define(NUMBER_OF_COUNTERS, 1).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-opaque counters() :: atomics:atomics_ref().
-export_type([counters/0]).

-record(handle, {
          pid :: pid(),
          counters :: counters(),
          concurrent :: boolean()
         }).
-opaque handle() :: #handle{}.
-export_type([handle/0]).

-record(state, {
          walk_started :: boolean(),
          counters :: counters(),
          concurrency :: pos_integer(),
          free_workers :: [{pid(), reply_tag()}]
         }).
-type state() :: #state{}.

-if(?OTP_RELEASE < 25).
-type reply_tag() :: reference().
-else.
-type reply_tag() :: gen_server:reply_tag().
-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec new_counters() -> counters().
new_counters() ->
    atomics:new(?NUMBER_OF_COUNTERS, [{signed, true}]).

-spec start_link(counters(), pos_integer()) -> {ok, pid()}.
start_link(Counters, Concurrency) ->
    gen_server:start_link(?MODULE, [Counters, Concurrency], []).

-spec new_handle(pid(), counters(), pos_integer()) -> handle().
new_handle(Pid, Counters, Concurrency) ->
    #handle{pid = Pid,
            counters = Counters,
            concurrent = Concurrency > 1}.

-spec take_index(handle()) -> {ok, non_neg_integer(), pos_integer()} | stop.
take_index(#handle{pid = Pid}) ->
    try
        gen_server:call(Pid, take_index, _Timeout = infinity)
    catch
        exit:{Reason, {gen_server, call, [Pid | _]}}
          when Reason =:= normal; Reason =:= noproc ->
            stop
    end.

-spec maybe_give_index(handle(), non_neg_integer(), pos_integer()) -> boolean().
maybe_give_index(#handle{concurrent = true, counters = Counters} = Handle, NodeIndex, Path) ->
    atomics:get(Counters, ?FREE_WORKERS_COUNTER) > 0
    andalso maybe_give_index_(Handle, NodeIndex, Path);
maybe_give_index(#handle{concurrent = false}, _NodeIndex, _Path) ->
    false.

-spec stop(pid()) -> ok.
stop(Pid) ->
    try
        gen_server:stop(Pid)
    catch
        exit:noproc -> ok
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([counters() | pos_integer(), ...]) -> {ok, state()}.
init([Counters, Concurrency]) ->
    _ = process_flag(trap_exit, true), % always call `:terminate/2' unless killed
    {ok, #state{walk_started = false,
                counters = Counters,
                concurrency = Concurrency,
                free_workers = []}}.

-spec handle_call(term(), {pid(), reference()}, state())
        -> {noreply, state()} |
           {reply, {ok, 0}, state()} |
           {stop, {unexpected_call, #{request := _, from := {pid(), reference()}}}, state()}.
handle_call(take_index, From, State) ->
    case State#state.walk_started of
        true
          when length(State#state.free_workers) < (State#state.concurrency - 1) ->
            atomics:add(State#state.counters, ?FREE_WORKERS_COUNTER, +1),
            UpdatedFreeWorkers = [From | State#state.free_workers],
            UpdatedState = State#state{free_workers = UpdatedFreeWorkers},
            {noreply, UpdatedState};
        true ->
            % All workers are waiting now, which means there's no work left.
            Reply = stop,
            {stop, normal, Reply, State};
        false ->
            RootIndex = 0, % give root index to whomever calls us first
            RootPath = 1, % start at one so we can always infer its length
            UpdatedState = State#state{walk_started = true},
            {reply, {ok, RootIndex, RootPath}, UpdatedState}
    end;
handle_call(Request, From, State) ->
    ErrorDetails = #{request => Request, from => From},
    {stop, {unexpected_call, ErrorDetails}, State}.

-spec handle_cast(term(), state())
        -> {noreply, state()} |
           {stop, {unexpected_cast, term()}, state()}.
handle_cast({give_index, NodeIndex, Path}, State) ->
    % ct:pal("???? ~p", [State#state.free_workers]),
    [ReplyTo | RemainingFreeworkers] = State#state.free_workers,
    UpdatedState = State#state{free_workers = RemainingFreeworkers},
    gen_server:reply(ReplyTo, {ok, NodeIndex, Path}),
    {noreply, UpdatedState};
handle_cast(Request, State) ->
    {stop, {unexpected_cast, Request}, State}.

-spec handle_info(term(), state())
        -> {stop, {unexpected_info, term()}, state()}.
handle_info(Info, State) ->
    {stop, {unexpected_info, Info}, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    lists:foreach(
      fun (ReplyTo) ->
              gen_server:reply(ReplyTo, stop)
      end,
      State#state.free_workers).

-spec code_change(term(), state() | term(), term())
        -> {ok, state()} | {error, {cannot_convert_state, term()}}.
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State};
code_change(_OldVsn, State, _Extra) ->
    {error, {cannot_convert_state, State}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

maybe_give_index_(Handle, NodeIndex, Path) ->
    case atomics:sub_get(Handle#handle.counters, ?FREE_WORKERS_COUNTER, 1) of
        UpdatedNumberOfFreeWorkers when UpdatedNumberOfFreeWorkers >= 0 ->
            gen_server:cast(Handle#handle.pid, {give_index, NodeIndex, Path}),
            true;
        _ ->
            atomics:add(Handle#handle.counters, ?FREE_WORKERS_COUNTER, +1), % too late
            false
    end.
