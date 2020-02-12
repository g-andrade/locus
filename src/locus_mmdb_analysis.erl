%% Copyright (c) 2019-2020 Guilherme Andrade
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

%% @reference <a href="https://maxmind.github.io/MaxMind-DB/">MaxMind DB File Format Specification</a>

-module(locus_mmdb_analysis).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([run/1]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type flaw() ::
    max_depth_exceeded() |
    invalid_tree_data_section_pointer() |
    data_record_decoding_failed().
-export_type([flaw/0]).

-type max_depth_exceeded() ::
    {max_depth_exceeded, #{ tree_prefix := {inet:ip_address(), 0..128},
                            node_index := non_neg_integer(),
                            depth := 33 | 129
                          }}.
-export_type([max_depth_exceeded/0]).

-type invalid_tree_data_section_pointer() ::
    {invalid_tree_data_section_pointer, #{ tree_prefix := {inet:ip_address(), 0..128},
                                           node_index := non_neg_integer(),
                                           value := neg_integer()
                               }}.
-export_type([invalid_tree_data_section_pointer/0]).

-type data_record_decoding_failed() ::
    {data_record_decoding_failed, #{ data_index := non_neg_integer(),
                                     class := error | throw | exit,
                                     reason := term(),
                                     tree_prefixes := [{inet:ip_address(), 0..128}, ...]
                                   }}.
-export_type([data_record_decoding_failed/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec run(locus_mmdb:parts()) -> ok | {error, {flawed, [flaw(), ...]}}.
%% @private
run(DatabaseParts) ->
    ParentPid = self(),
    PrevTrapExit = process_flag(trap_exit, true),
    CoordinatorSpawnOpts = [link, {priority,low}],
    try
        CoordinatorPid =
            spawn_opt(
              fun () -> run_analysis_coordinator(ParentPid, DatabaseParts) end,
              CoordinatorSpawnOpts),

        receive
            {CoordinatorPid, {analysis_result, TreeFlaws, DataRecordFlaws}} ->
                process_flag(trap_exit, PrevTrapExit),
                receive {'EXIT', CoordinatorPid, _} -> ok after 0 -> ok end,
                case {TreeFlaws,DataRecordFlaws} of
                    {[],[]} ->
                        ok;
                    _ ->
                        {error, {flawed, TreeFlaws ++ DataRecordFlaws}}
                end;
            {'EXIT', CoordinatorPid, Reason} ->
                process_flag(trap_exit, PrevTrapExit),
                {error, {coordinator_stopped, CoordinatorPid, Reason}}
        end
    catch
        ExcClass:ExcReason ->
            Stacktrace = erlang:get_stacktrace(),
            SaferReason = locus_util:purge_term_of_very_large_binaries(ExcReason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            true = process_flag(trap_exit, PrevTrapExit),
            erlang:raise(ExcClass, SaferReason, SaferStacktrace)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

run_analysis_coordinator(ParentPid, DatabaseParts) ->
    #{metadata := Metadata, tree := Tree, data_section := DataSection} = DatabaseParts,
    MaxDepth =
        case maps:get(<<"ip_version">>, Metadata) of
            4 -> 32;
            6 -> 128
        end,

    CoordinatorPid = self(),
    DataAnalyzerSpawnOpts = [link, {priority,normal}],
    DataAnalysisConcurrency = erlang:system_info(schedulers_online),
    DataAnalyzers =
        lists:foldl(
          fun (DataAnalyzerNr, Acc) ->
                  Pid = spawn_opt(
                          fun () -> run_data_analyzer(CoordinatorPid, DataSection) end,
                          DataAnalyzerSpawnOpts),
                  maps:put(DataAnalyzerNr - 1, Pid, Acc)
          end,
          #{}, lists:seq(1, DataAnalysisConcurrency)),

    RevTreeFlaws = analyze_tree(DataAnalyzers, MaxDepth, Metadata, Tree),
    TreeFlaws = lists:reverse(RevTreeFlaws),

    BadDataRecordResults =
        maps:fold(
          fun (_, DataAnalyzerPid, Acc) ->
                  _ = DataAnalyzerPid ! {self(), collect_bad_results},
                  receive
                      {DataAnalyzerPid, {bad_results, Bad}} ->
                          maps:merge(Acc, Bad)
                  end
          end,
          #{}, DataAnalyzers),

    DataRecordFlaws =
        maps:fold(
          fun (DataIndex, {{data_record_decoding_failed,Class,Reason}, TreeRefs}, Acc) ->
                  [{data_record_decoding_failed,
                    #{ data_index => DataIndex,
                       class => Class,
                       reason => Reason,
                       tree_prefixes => data_analysis_bad_tree_prefixes(MaxDepth, TreeRefs)
                     }} | Acc]
          end,
          [], BadDataRecordResults),

    _ = ParentPid ! {self(), {analysis_result, TreeFlaws, DataRecordFlaws}},
    ok.

analyze_tree(DataAnalyzers, MaxDepth, Metadata, Tree) ->
    locus_mmdb_tree:foldl(
      fun (Prefix, Depth, NodeIndex, _DataIndex, Acc) when Depth > MaxDepth ->
              [{max_depth_exceeded,
                #{ tree_prefix => analysis_flaw_prefix(MaxDepth, Depth, Prefix),
                   node_index => NodeIndex
                 }} | Acc];
          (Prefix, Depth, NodeIndex, DataIndex, Acc) when DataIndex < 0 ->
              [{invalid_tree_data_section_pointer,
                #{ tree_prefix => analysis_flaw_prefix(MaxDepth, Depth, Prefix),
                   node_index => NodeIndex,
                   value => DataIndex
                 }} | Acc];
          (Prefix, Depth, _NodeIndex, DataIndex, Acc) ->
              DataAnalyzerNr = erlang:phash2(DataIndex, map_size(DataAnalyzers)),
              DataAnalyzerPid = maps:get(DataAnalyzerNr, DataAnalyzers),
              _ = DataAnalyzerPid ! {self(), {analyze, DataIndex, Depth, Prefix}},
              Acc
      end,
      [], Metadata, Tree).

analysis_flaw_prefix(MaxDepth, Depth, Prefix) ->
    ShiftAmount = MaxDepth - Depth,
    ShiftedPrefix = Prefix bsl ShiftAmount,
    BitAddress = <<ShiftedPrefix:MaxDepth>>,
    locus_mmdb_tree:bitstring_ip_address_prefix(BitAddress, ShiftAmount).

run_data_analyzer(CoordinatorPid, DataSection) ->
    State = #{ coordinator_pid => CoordinatorPid,
               data_section => DataSection,
               good => gb_sets:empty(),
               bad => #{}
             },
    run_data_analyzer_loop(State).

run_data_analyzer_loop(State) ->
    receive
        Msg ->
            UpdatedState = handle_data_analyzer_msg(Msg, State),
            run_data_analyzer_loop(UpdatedState)
    end.

handle_data_analyzer_msg({CoordinatorPid, {analyze, DataIndex, Depth, Prefix}},
                         #{coordinator_pid := CoordinatorPid} = State) ->
    #{good := Good} = State,
    case gb_sets:is_element(DataIndex, Good) of
        true ->
            % already analyzed and classified as good data record
            run_data_analyzer_loop(State);
        false ->
            #{bad := Bad} = State,
            case maps:find(DataIndex, Bad) of
                {ok, {FlawInfo, BadReferences}} ->
                    % already analyzed and classified as flawed data record
                    UpdatedBadRefereces = [{Depth,Prefix} | BadReferences],
                    UpdatedBad = maps:update(DataIndex, {FlawInfo, UpdatedBadRefereces}, Bad),
                    UpdatedState = maps:update(bad, UpdatedBad, State),
                    run_data_analyzer_loop(UpdatedState);
                error ->
                    % analyzing for the first time
                    handle_data_record_analysis(DataIndex, Depth, Prefix, State)
            end
    end;
handle_data_analyzer_msg({CoordinatorPid, collect_bad_results},
                         #{coordinator_pid := CoordinatorPid} = State) ->
    #{bad := Bad} = State,
    _ = CoordinatorPid ! {self(), {bad_results,Bad}},
    State.

handle_data_record_analysis(DataIndex, Depth, Prefix, State) ->
    #{data_section := DataSection} = State,
    try
        begin
            locus_mmdb_data:decode_on_index(DataIndex, DataSection)
        end
    of
        {_, _} ->
            #{good := Good} = State,
            UpdatedGood = gb_sets:insert(DataIndex, Good),
            maps:update(good, UpdatedGood, State)
    catch
        Class:Reason ->
            #{bad := Bad} = State,
            FlawInfo = {data_record_decoding_failed, Class, Reason},
            UpdatedBad = maps:put(DataIndex, {FlawInfo,[{Depth,Prefix}]}, Bad),
            maps:update(bad, UpdatedBad, State)
    end.

data_analysis_bad_tree_prefixes(MaxDepth, BadReferences) ->
    lists:map(
      fun ({Depth, Prefix}) ->
              analysis_flaw_prefix(MaxDepth, Depth, Prefix)
      end,
      BadReferences).
