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

-export([start/4]).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type opt() :: {emulate_legacy_behaviour, boolean()}.
-export_type([opt/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start(reference(), [atom()], timeout(), [opt()]) -> pid().
start(ReplyRef, DatabaseId, Timeout, Opts) ->
    OwnerPid = self(),
    spawn_link(
      fun () ->
              run_waiter(OwnerPid, ReplyRef, DatabaseId, Timeout, Opts)
      end).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

run_waiter(OwnerPid, ReplyRef, DatabaseId, Timeout, Opts) ->
    _ = process_flag(trap_exit, true),
    _ = maybe_schedule_timeout(Timeout),
    EmulateLegacyBehaviour = proplists:get_value(emulate_legacy_behaviour, Opts),

    case locus_database:get_version_or_subscribe(DatabaseId) of
        {version, Version} ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {ok, Version});
        {subscribed, DatabasePid} ->
            DatabaseMon = monitor(process, DatabasePid),
            wait_for_success(OwnerPid, DatabaseMon, ReplyRef, DatabaseId, Opts, []);
        database_unknown 
          when EmulateLegacyBehaviour, Timeout =:= 0 ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, timeout});
        database_unknown ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, database_unknown})
    end.

-spec reply_to_owner(pid(), reference(), atom(), tuple()) -> no_return().
reply_to_owner(OwnerPid, ReplyRef, DatabaseId, Reply) ->
    _ = OwnerPid ! {ReplyRef, DatabaseId, Reply},
    unlink(OwnerPid),
    exit(normal).

wait_for_success(OwnerPid, DatabaseMon, ReplyRef, DatabaseId, Opts, LoadAttemptFailures) ->
    EmulateLegacyBehaviour = proplists:get_value(emulate_legacy_behaviour, Opts),

    case receive_message(OwnerPid, DatabaseMon, DatabaseId) of
        {event, {load_attempt_finished, {cache, _}, {error, not_found}}} ->
            wait_for_success(OwnerPid, DatabaseMon, ReplyRef, DatabaseId, Opts, LoadAttemptFailures);
        {event, {load_attempt_finished, _, {ok, Version}}} ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {ok, Version});
        {event, {load_attempt_finished, _, {error, Reason}}}
          when EmulateLegacyBehaviour ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, {loading, Reason}});
        {event, {load_attempt_finished, _, {error, Reason}}} ->
            UpdatedLoadAttemptFailures = [Reason | LoadAttemptFailures],
            wait_for_success(OwnerPid, DatabaseMon, ReplyRef, DatabaseId, Opts, UpdatedLoadAttemptFailures);
        {event, _} ->
            wait_for_success(OwnerPid, DatabaseMon, ReplyRef, DatabaseId, Opts, LoadAttemptFailures);

        timeout when LoadAttemptFailures =:= [] ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, timeout});
        timeout ->
            Reason = {load_attempts, lists:reverse(LoadAttemptFailures)},
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, Reason});

        {database_stopped, _Reason}
          when EmulateLegacyBehaviour ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, database_unknown});
        {database_stopped, Reason} ->
            reply_to_owner(OwnerPid, ReplyRef, DatabaseId, {error, {stopped, Reason}});
        owner_stopped ->
            exit(normal)
    end.

receive_message(OwnerPid, DatabaseMon, DatabaseId) ->
    receive
        {locus, DatabaseId, Event} ->
            {event, Event};
        timeout ->
            timeout;
        {'DOWN', DatabaseMon, _, _, Reason} ->
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
