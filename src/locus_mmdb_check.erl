%% Copyright (c) 2019-2022 Guilherme Andrade
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

%% @doc API for working with MMDB - wholesomeness check
-module(locus_mmdb_check).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([run/1]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type bad_node_index_in_tree()
    :: {bad_node_index_in_tree, non_neg_integer(),
        #{under := #{prefix := string(),
                     path := [non_neg_integer()]}}
       }.
-export_type([bad_node_index_in_tree/0]).


-type bad_data_index_in_tree()
    :: {bad_data_index_in_tree, non_neg_integer(),
        #{why := term(),
          under := #{prefix := string(),
                     path := [non_neg_integer()]}}
       }.
-export_type([bad_data_index_in_tree/0]).

-type excessively_long_path_in_tree()
    :: {excessively_long_path_in_tree, non_neg_integer(),
        #{under := #{prefix := string(),
                     path := [non_neg_integer()]}}
       }.
-export_type([excessively_long_path_in_tree/0]).

-type loop_in_data_section()
    :: {loop_in_data_section,
        #{path => [{non_neg_integer(), term()}, ...]}
       }.
-export_type([loop_in_data_section/0]).

-type invalid_position_in_data_section()
    :: {invalid_position_in_data_section,
        #{which => non_neg_integer(),
          under := #{path := [{non_neg_integer(), term()}]}}
       }.
-export_type([invalid_position_in_data_section/0]).

-type bad_chunk_in_data_section()
    :: {bad_chunk_in_data_section,
        #{position := non_neg_integer(),
          why := term(),
          under := #{path := [non_neg_integer()]}}
       }.
-export_type([bad_chunk_in_data_section/0]).

-type map_key_of_wrong_type_in_data_section()
    :: {map_key_of_wrong_type_in_data_section,
        #{position := non_neg_integer(),
          key := term(),
          under := #{path := [non_neg_integer()]}}
       }.
-export_type([map_key_of_wrong_type_in_data_section/0]).

-type invalid_utf8_string_in_data_section()
    :: {invalid_utf8_string_in_data_section,
        #{position := non_neg_integer(),
          original_data := binary(),
          error := term(),
          under := #{path := [non_neg_integer()]}}
       }.
-export_type([invalid_utf8_string_in_data_section/0]).

-type error()
    :: bad_node_index_in_tree()
    |  bad_data_index_in_tree()
    |  excessively_long_path_in_tree()
    |  loop_in_data_section()
    |  invalid_position_in_data_section()
    |  bad_chunk_in_data_section()
    |  map_key_of_wrong_type_in_data_section()
    |  invalid_utf8_string_in_data_section()
    .
-export_type([error/0]).

-type unprintable_utf8_string_in_data_section()
    :: {unprintable_utf8_string_in_data_section,
        #{position := non_neg_integer(),
          value := unicode:unicode_binary(),
          under := #{path := [non_neg_integer()]}}
        }.
-export_type([unprintable_utf8_string_in_data_section/0]).

-type warning()
    :: unprintable_utf8_string_in_data_section()
    .
-export_type([warning/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Checks for errors and quirks within `Database'
-spec run(locus_mmdb:database())
        -> ok
           | {warnings, [warning(), ...]}
           | {errors, [error(), ...], [warning()]}.
run(Database) ->
    #{tree := Tree, data_section := DataSection} = Database,
    Journal = locus_mmdb_check_journal:new(),

    try
        {ok, DataIndicesBitArray} = collect_data_indices_in_tree(Tree, DataSection, Journal),
        ok = validate_data_indices(DataIndicesBitArray, DataSection, Journal),
        check_journal(Journal)
    after
        locus_mmdb_check_journal:delete(Journal)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

collect_data_indices_in_tree(Tree, DataSection, Journal) ->
    Concurrency = erlang:system_info(schedulers_online),
    BitArray = locus_shared_bitarray:new(_Length = byte_size(DataSection)),
    WalkManagerCounters = locus_mmdb_tree_walk_manager:new_counters(),
    {ok, WalkManagerPid} = locus_mmdb_tree_walk_manager:start_link(WalkManagerCounters,
                                                                   Concurrency),
    WalkManager = locus_mmdb_tree_walk_manager:new_handle(WalkManagerPid,
                                                          WalkManagerCounters,
                                                          Concurrency),

    try run_concurrent_tasks(
           Concurrency,
           fun (_TaskIndex, _Concurrency) ->
                   collect_data_indices_batches_in_tree(BitArray, DataSection,
                                                        WalkManager, Journal, Tree)
           end)
    of
        {ok, _} ->
            {ok, BitArray}
    after
        locus_mmdb_tree_walk_manager:stop(WalkManagerPid)
    end.

collect_data_indices_batches_in_tree(BitArray, DataSection, WalkManager, Journal, Tree) ->
    DataSectionSize = byte_size(DataSection),
    locus_mmdb_tree:validate(
      fun (DataIndex) ->
              case DataIndex < DataSectionSize of
                  true ->
                      locus_shared_bitarray:set(BitArray, DataIndex);
                  false ->
                      {error, {beyond_data_section_size, DataSectionSize}}
              end
      end,
      WalkManager, Journal, Tree).

validate_data_indices(BitArray, DataSection, Journal) ->
    VisitedBitArray = locus_shared_bitarray:new(byte_size(DataSection)),
    MapKeysBitArray = locus_shared_bitarray:new(byte_size(DataSection)),

    case run_concurrent_tasks(
           fun (TaskIndex, Concurrency) ->
                   BatchSize = Concurrency,
                   BatchOffset = TaskIndex,
                   validate_data_indices_batch(BitArray, VisitedBitArray, MapKeysBitArray,
                                               BatchSize, BatchOffset, DataSection,
                                               Journal)
           end)
    of
        {ok, _} ->
            ok;
        {error, _} = Error ->
            Error
    end.

validate_data_indices_batch(BitArray, VisitedBitArray, MapKeysBitArray,
                            BatchSize, BatchOffset, DataSection,
                            Journal) ->
    locus_mmdb_data_codec:validate_indices_in_tree(
      BitArray, VisitedBitArray, MapKeysBitArray,
      BatchSize, BatchOffset, DataSection,
      Journal).

run_concurrent_tasks(TaskFun) ->
    Concurrency = erlang:system_info(schedulers_online),
    run_concurrent_tasks(Concurrency, TaskFun).

run_concurrent_tasks(Concurrency, TaskFun) ->
    Monitors = spawn_concurrent_tasks(Concurrency, TaskFun),
    await_concurrent_tasks(Monitors).

spawn_concurrent_tasks(Concurrency, TaskFun) ->
    PidMonitors = [spawn_task(TaskFun, TaskIndex, Concurrency)
                   || TaskIndex <- lists:seq(0, Concurrency - 1)],

    {PidsList, MonitorsList} = lists:unzip(PidMonitors),
    maps:from_list( lists:zip(MonitorsList, PidsList) ).

spawn_task(Fun, TaskIndex, Concurrency) ->
    spawn_monitor(
      fun () ->
              try Fun(TaskIndex, Concurrency) of
                  Reply ->
                      exit({shutdown, Reply})
              catch
                  Class:Reason:Stacktrace ->
                      SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
                      SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
                      erlang:raise(Class, SaferReason, SaferStacktrace)
              end
      end).

await_concurrent_tasks(Monitors) ->
    await_concurrent_tasks_recur(Monitors, []).

await_concurrent_tasks_recur(Monitors, SuccessesAcc)
  when map_size(Monitors) > 0 ->
    receive
        {'DOWN', Ref, process, Pid, Reason}
          when is_map_key(Ref, Monitors) ->
            handle_task_shutdown(Monitors, Ref, Pid, Reason, SuccessesAcc)
    end;
await_concurrent_tasks_recur(#{} = _Monitors, SuccessesAcc) ->
    {ok, SuccessesAcc}.

handle_task_shutdown(Monitors, Ref, Pid, Reason, SuccessesAcc) ->
    {Pid, RemainingMonitors} = maps:take(Ref, Monitors),
    case Reason of
        {shutdown, ok} ->
            await_concurrent_tasks_recur(RemainingMonitors, SuccessesAcc);
        {shutdown, {ok, Success}} ->
            UpdatedSuccessesAcc = [Success | SuccessesAcc],
            await_concurrent_tasks_recur(RemainingMonitors, UpdatedSuccessesAcc);
        {shutdown, {error, Reason}} ->
            {error, Reason};
        UnexpectedReason ->
            error({concurrent_task_terminated_unexpectedly,
                   #{pid => Pid,
                     why => UnexpectedReason}})
    end.

check_journal(Journal) ->
    case locus_mmdb_check_journal:check(Journal) of
        #{errors := [], warnings := []} ->
            ok;
        #{errors := [], warnings := Warnings} ->
            {warnings, Warnings};
        #{errors := Errors, warnings := Warnings} ->
            {errors, lists:usort(Errors), lists:usort(Warnings)}
    end.
