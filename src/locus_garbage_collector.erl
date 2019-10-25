%% Copyright (c) 2019 Guilherme Andrade
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

-module(locus_garbage_collector).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [validate_opts/1,
    start_link/1,
    dispatch_binaries/3
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

-define(is_non_neg_integer(V), ((is_integer((V)) andalso ((V) >= 0)))).

-define(DEFAULT_DELAY, (timer:seconds(10))).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type opt() ::
    no_garbage_collection |
    {garbage_collection_delay, non_neg_integer()}.
-export_type([opt/0]).

-type msg() :: {event,event()}.
-export_type([msg/0]).

-type event() ::
    garbage_collection_started_event() |
    garbage_collection_finished_event().
-export_type([event/0]).

-type garbage_collection_started_event() ::
    {garbage_collection_started,
     #{ target := garbage_collection_target(),
        context := term()
      }}.
-export_type([garbage_collection_started_event/0]).

-type garbage_collection_finished_event() ::
    {garbage_collection_finished,
     #{ target := garbage_collection_target(),
        context := term(),
        duration := non_neg_integer(), % in native time units (monotonic clock)
        processes_affected := non_neg_integer()
      }}.
-export_type([garbage_collection_finished_event/0]).

-type garbage_collection_target() :: shared_binaries.
-export_type([garbage_collection_target/0]).

-record(state, {
          owner_pid :: pid(),
          active :: boolean(),
          delay :: non_neg_integer()
         }).
-type state() :: #state{}.

-type binary_id() :: term().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec validate_opts(list())
        -> {ok, {[opt()], list()}} |
           {error, term()}.
%% @private
validate_opts(MixedOpts) ->
    try
        lists:partition(
          fun (no_garbage_collection) ->
                  true;
              ({garbage_collection_delay, Delay} = Opt) ->
                  ?is_non_neg_integer(Delay) orelse error({badopt,Opt});
              (_) ->
                  false
          end,
          MixedOpts)
    of
        {Opts, OtherOpts} ->
            {ok, {Opts, OtherOpts}}
    catch
        error:{badopt,BadOpt} ->
            {error, BadOpt}
    end.

-spec start_link([opt()]) -> {ok, pid()}.
%% @private
start_link(Opts) ->
    gen_server:start_link(?MODULE, [self(), Opts], []).

-spec dispatch_binaries(pid(), [binary()], term()) -> ok.
%% @private
dispatch_binaries(Pid, Binaries, Context) ->
    MaybeBinaryIds = [ref_counted_binary_id(Binary) || Binary <- Binaries],
    BinaryIds = [BinaryId || {id,BinaryId} <- MaybeBinaryIds],
    UniqueBinaryIds = lists:usort(BinaryIds),
    gen_server:cast(Pid, {schedule_binary_collection,UniqueBinaryIds,Context}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([InitArg, ...]) -> {ok, state()}
        when InitArg :: OwnerPid | Opts,
             OwnerPid :: pid(),
             Opts :: [opt()].
%% @private
init([OwnerPid, Opts]) ->
    _ = process_flag(trap_exit, true),
    {ok, #state{
            owner_pid = OwnerPid,
            active = not proplists:get_value(no_garbage_collection, Opts, false),
            delay = proplists:get_value(garbage_collection_delay, Opts, ?DEFAULT_DELAY)
           }}.

-spec handle_call(term(), {pid(),reference()}, state())
        -> {stop, unexpected_call, state()}.
%% @private
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state())
        -> {noreply, state()} |
           {stop, unexpected_cast, state()}.
%% @private
handle_cast({schedule_binary_collection,BinaryIds,Context}, State) ->
    handle_binary_collection_schedule(BinaryIds, Context, State);
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state())
        -> {stop, normal, state()} |
           {stop, unexpected_info, state()}.
%% @private
handle_info({collect_binaries,BinaryIds,Context}, State) ->
    StartTs = erlang:monotonic_time(),
    StartEvent = {garbage_collection_started, #{target => shared_binaries, context => Context}},
    report_event(StartEvent, State),
    ProcessesAffected = collect_binaries(BinaryIds),
    Duration = erlang:monotonic_time() - StartTs,
    handle_binary_collection_result(Context, Duration, ProcessesAffected, State);
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
%% Internal Function Definitions - Binary Collection
%% ------------------------------------------------------------------

-spec ref_counted_binary_id(binary()) -> {id, binary_id()} | undefined.
ref_counted_binary_id(Binary) ->
    CallerPid = self(),
    HelperRef = make_ref(),
    {HelperPid, HelperMon} =
        spawn_monitor(
          fun () ->
                  receive
                      {HelperRef, MaybeCopiedBinary} ->
                          {binary, CopiedBinaryInfos} = process_info(self(), binary),
                          _ = binary:referenced_byte_size(MaybeCopiedBinary),
                          _ = CallerPid ! {HelperRef, CopiedBinaryInfos}
                  after
                      5000 ->
                          exit(timeout)
                  end
          end),

    _ = HelperPid ! {HelperRef, Binary},
    receive
        {HelperRef, CopiedBinaryInfos} ->
            demonitor(HelperMon, [flush]),
            {binary, OwnBinaryInfos} = process_info(self(), binary),
            case lists:filter(
                   fun ({BinaryId, _, _}) ->
                           lists:keymember(BinaryId, 1, OwnBinaryInfos)
                   end,
                   CopiedBinaryInfos)
            of
                [{BinaryId, _, _}] ->
                    {id, BinaryId};
                [] ->
                    undefined
            end;

        {'DOWN', HelperMon, _, _, Reason} ->
            exit({helper_proc, Reason})
    after
        5000 ->
            demonitor(HelperMon, [flush]),
            exit(HelperPid, kill),
            exit(timeout)
    end.

handle_binary_collection_schedule(BinaryIds, _Context, State)
  when BinaryIds =:= [];
       not State#state.active ->
    {noreply, State};
handle_binary_collection_schedule(BinaryIds, Context, State) ->
    _ = erlang:send_after(State#state.delay, self(), {collect_binaries,BinaryIds,Context}),
    {noreply, State}.

collect_binaries(BinaryIds) ->
    lists:foldl(
      fun (Pid, Count) ->
              MaybeBinaryInfos = process_info(Pid, binary),
              case maybe_collect_pid(Pid, MaybeBinaryInfos, BinaryIds) of
                  true -> Count + 1;
                  _    -> Count
              end
      end,
      0, processes()).

maybe_collect_pid(_, undefined, _) ->
    false;
maybe_collect_pid(Pid, {binary,BinaryInfos}, BinaryIds) ->
    binary_infos_intersect_ids(BinaryInfos, BinaryIds)
    andalso erlang:garbage_collect(Pid). % FIXME major?

binary_infos_intersect_ids(BinaryInfos, BinaryIds) ->
    lists:any(
      fun (BinaryId) ->
              lists:keymember(BinaryId, 1, BinaryInfos)
      end,
      BinaryIds).

handle_binary_collection_result(Context, Duration, NumberOfProcesses, State) ->
    Details =
        #{ target => shared_binaries,
           context => Context,
           duration => Duration,
           processes_affected => NumberOfProcesses
         },
    Event = {garbage_collection_finished, Details},
    report_event(Event, State),
    erlang:garbage_collect(),
    {noreply, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Utils
%% ------------------------------------------------------------------

-spec report_event(event(), state()) -> ok.
report_event(Event, State) ->
    notify_owner({event,Event}, State).

-spec notify_owner(msg(), state()) -> ok.
notify_owner(Msg, State) ->
    #state{owner_pid = OwnerPid} = State,
    _ = erlang:send(OwnerPid, {self(),Msg}, [noconnect]),
    ok.
