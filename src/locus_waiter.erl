%% Copyright (c) 2017-2020 Guilherme Andrade
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
-module(locus_waiter).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start([atom()], timeout()) -> {pid(), reference()}.
start(DatabaseIds, Timeout) ->
    OwnerPid = self(),
    spawn_monitor(
      fun () ->
              run_waiter(OwnerPid, DatabaseIds, Timeout)
      end).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

run_waiter(OwnerPid, DatabaseIds, Timeout) ->
    _ = maybe_schedule_timeout(Timeout),
    OwnerMon = monitor(process, OwnerPid),
    WaitRefs = lists:map(fun locus_database:enqueue_waiter/1, DatabaseIds),
    WaitList = lists:zip(WaitRefs, DatabaseIds),
    run_waiter_loop(OwnerPid, OwnerMon, WaitList, #{}).

run_waiter_loop(OwnerPid, _OwnerMon, [], Successes) ->
    _ = OwnerPid ! {self(), {ok, Successes}},
    exit(normal);
run_waiter_loop(OwnerPid, OwnerMon, WaitList, Successes) ->
    receive
        Msg ->
            handle_msg(Msg, OwnerPid, OwnerMon, WaitList, Successes)
    end.

handle_msg(Msg, OwnerPid, OwnerMon, WaitList, Successes) ->
    case Msg of
        timeout ->
            _ = OwnerPid ! {self(), {error, timeout}},
            exit(normal);
        {'DOWN', OwnerMon, _, _, _} ->
            exit(normal);
        {'DOWN', Ref, _, _, Reason} ->
            {_,DatabaseId} = lists:keyfind(Ref, 1, WaitList),
            _ = OwnerPid ! {self(), {error, {stopped, DatabaseId, Reason}}},
            exit(normal);
        {Ref, {error,Reason}}
          when is_reference(Ref) ->
            {_,DatabaseId} = lists:keyfind(Ref, 1, WaitList),
            _ = OwnerPid ! {self(), {error, {DatabaseId, Reason}}},
            exit(normal);
        {Ref, {ok,Version}}
          when is_reference(Ref) ->
            {value, {_,DatabaseId}, UpdatedWaitList} = lists:keytake(Ref, 1, WaitList),
            demonitor(Ref, [flush]),
            UpdatedSuccesses = Successes#{ DatabaseId => Version },
            run_waiter_loop(OwnerPid, OwnerMon, UpdatedWaitList, UpdatedSuccesses)
    end.

maybe_schedule_timeout(infinity) ->
    no;
maybe_schedule_timeout(0) ->
    _ = self() ! timeout,
    now;
maybe_schedule_timeout(Timeout) ->
    erlang:send_after(Timeout, self(), timeout).
