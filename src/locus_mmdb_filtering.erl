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

%% @reference <a href="https://maxmind.github.io/MaxMind-DB/">MaxMind DB File Format Specification</a>

%% @private
-module(locus_mmdb_filtering).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([run/2]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec run(locus_mmdb:parts(), string()) -> ok.
run(DatabaseParts, _OutputFilename) ->
    #{metadata := Metadata, tree := Tree} = DatabaseParts,
    io:format("loading data record indices from tree...~n"),
    TreeDataIndices = locus_mmdb_tree:unique_data_indices(Metadata, Tree),
    io:format("nr of tree data indices: ~p~n", [length(TreeDataIndices)]),

    io:format("loading data section dependencies...~n"),
    Whitelist = #{ <<"location">> => everything },
    ProcDictRestore = create_dependencies_table(Whitelist),
    try
        collect_data_section_dependencies(DatabaseParts, TreeDataIndices),
        io:format("total nr of dependencies: ~p~n", [dependencies_size()]),

        %%filter_data_section(DatabaseParts, AllDependencies),
        ok
    after
        destroy_dependencies_table(ProcDictRestore)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Collecting Dependencies
%% ------------------------------------------------------------------

create_dependencies_table(Whitelist) ->
    Table = ets:new(dependencies, [ordered_set]),
    #{ dependencies_table => erlang:put(dependencies_table, Table),
       whitelist => erlang:put(whitelist, Whitelist)
     }.

add_dependency(Index) ->
    Table = erlang:get(dependencies_table),
    ets:insert(Table, {Index}).

is_dependency(Index) ->
    Table = erlang:get(dependencies_table),
    ets:member(Table, Index).

dependencies_size() ->
    Table = erlang:get(dependencies_table),
    ets:info(Table, memory).

current_whitelist() ->
    erlang:get(whitelist).

destroy_dependencies_table(ProcDictRestore) ->
    Table = erlang:get(dependencies_table),
    ets:delete(Table),
    maps:fold(
      fun (Key, Value, _) ->
              _ = erlang:put(Key, Value),
              ok
      end,
      ok, ProcDictRestore).

collect_data_section_dependencies(DatabaseParts, TreeDependencies) ->
    #{data_section := DataSection} = DatabaseParts,
    lists:foreach(
      fun (Index) ->
              _ = maybe_collect_data_chunk_dependencies(Index, DataSection, [])
      end,
      TreeDependencies).

maybe_collect_data_chunk_dependencies(Index, DataSection, Path) ->
    case is_dependency(Index) of
        true ->
            skipped;
        _ ->
            collect_data_chunk_dependencies(Index, DataSection, Path)
    end.

collect_data_chunk_dependencies(Index, DataSection, Path) ->
    add_dependency(Index),
    <<_:Index/bytes, Chunk/bytes>> = DataSection,
    {Size, Type, Value, _} = locus_mmdb_data:read_chunk(Chunk),
    case Type of
        pointer ->
            _ = maybe_collect_data_chunk_dependencies(Value, DataSection, Path),
            Index + Size;
        map ->
            collect_data_map_dependencies(Index + Size, Value, DataSection, Path);
        array ->
            collect_data_array_dependencies(Index + Size, Value, DataSection);
        _ ->
            Index + Size
    end.


collect_data_map_dependencies(KeyIndex, 0, _DataSection) ->
    KeyIndex;
collect_data_map_dependencies(KeyIndex, NrOfPairsLeft, DataSection) ->
    ValueIndex = collect_data_map_key_dependencies(KeyIndex, DataSection),
    NextKeyIndex = collect_data_chunk_dependencies(ValueIndex, DataSection),
    collect_data_map_dependencies(NextKeyIndex, NrOfPairsLeft - 1, DataSection).

collect_data_map_key_dependencies(KeyIndex, DataSection) ->
    add_dependency(KeyIndex),
    <<_:KeyIndex/bytes, Chunk/bytes>> = DataSection,
    {KeySize, KeyType, KeyValue, _} = locus_mmdb_data:read_chunk(Chunk),
    case KeyType of
        pointer ->
            _ = maybe_collect_data_chunk_dependencies(KeyValue, DataSection),
            KeyIndex + KeySize;
        utf8_string ->
            KeyIndex + KeySize
    end.

collect_data_array_dependencies(ValueIndex, 0, _DataSection) ->
    ValueIndex;
collect_data_array_dependencies(ValueIndex, NrOfValuesLeft, DataSection) ->
    NextValueIndex = collect_data_chunk_dependencies(ValueIndex, DataSection),
    collect_data_array_dependencies(NextValueIndex, NrOfValuesLeft - 1, DataSection).
