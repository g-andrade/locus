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

%% @private
-module(locus_waiter).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start(reference(), [atom()], timeout()) -> pid().
start(ReplyRef, DatabaseId, Timeout) ->
    OwnerPid = self(),
    spawn_link(
      fun () ->
              run_waiter(OwnerPid, ReplyRef, DatabaseId, Timeout)
      end).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

run_waiter(OwnerPid, ReplyRef, DatabaseId, Timeout) ->
    _ = process_flag(trap_exit, true),
    _ = maybe_schedule_timeout(Timeout),
    {await, SubscriptionRef} = locus_database:async_get_version_or_subscribe(DatabaseId),
    wait_for_success(OwnerPid, SubscriptionRef, ReplyRef, DatabaseId, Timeout, []).

-spec reply_to_owner(pid(), reference(), atom(), tuple()) -> no_return().
reply_to_owner(OwnerPid, ReplyRef, DatabaseId, Reply) ->
    _ = OwnerPid ! {ReplyRef, DatabaseId, Reply},
    unlink(OwnerPid),
    exit(normal).

wait_for_success(OwnerPid, SubscriptionRef, ReplyRef, DatabaseId,
                 Timeout, LoadAttemptFailures) ->

    case receive_message(OwnerPid, SubscriptionRef, DatabaseId) of
        {already_loaded, Version} ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {ok, Version});
        database_unknown ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, database_unknown});

        {event, {load_attempt_finished, {cache, _}, {error, not_found}}} ->
            wait_for_success(OwnerPid, SubscriptionRef, ReplyRef, DatabaseId,
                             Timeout, LoadAttemptFailures);
        {event, {load_attempt_finished, _, {ok, Version}}} ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {ok, Version});
        {event, {load_attempt_finished, _, {error, Reason}}} ->
            UpdatedLoadAttemptFailures = [Reason | LoadAttemptFailures],
            wait_for_success(OwnerPid, SubscriptionRef, ReplyRef, DatabaseId,
                             Timeout, UpdatedLoadAttemptFailures);
        {event, _} ->
            wait_for_success(OwnerPid, SubscriptionRef, ReplyRef, DatabaseId,
                             Timeout, LoadAttemptFailures);

        timeout ->
            Reason = {timeout, lists:reverse(LoadAttemptFailures)},
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, Reason});

        {database_stopped, Reason} ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, {stopped, Reason}});
        owner_stopped ->
            exit(normal)
    end.

receive_message(OwnerPid, SubscriptionRef, DatabaseId) ->
    receive
        timeout ->
            timeout;
        {SubscriptionRef, {version, Version}} ->
            demonitor(SubscriptionRef, [flush]),
            {already_loaded, Version};
        {'DOWN', SubscriptionRef, _, _, Reason}
          when Reason =:= noproc;
               Reason =:= normal;
               Reason =:= shutdown;
               element(1, Reason) =:= shutdown, tuple_size(Reason) =:= 2 ->
            database_unknown;

        {locus, DatabaseId, Event} ->
            {event, Event};
        {'DOWN', SubscriptionRef, _, _, Reason} ->
            {database_stopped, Reason};
        {'EXIT', OwnerPid, _} ->
            owner_stopped
    end.

maybe_schedule_timeout(infinity) ->
    no;
maybe_schedule_timeout(0) ->
    _ = self() ! timeout,
    now;
maybe_schedule_timeout(Timeout) ->
    erlang:send_after(Timeout, self(), timeout).
