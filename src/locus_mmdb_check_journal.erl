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
-module(locus_mmdb_check_journal).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [new/0,
    bad_node_index_in_tree/4,
    bad_data_index_in_tree/5,
    excessively_long_path_in_tree/4,
    loop_in_data_section/2,
    invalid_position_in_data_section/3,
    bad_chunk_in_data_section/4,
    map_key_of_wrong_type_in_data_section/4,
    invalid_utf8_string_in_data_section/5,
    unprintable_utf8_string_in_data_section/4,
    check/1,
    delete/1
   ]).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(journal, {
          error_tables :: tuple(),
          warning_tables :: tuple()
         }).
-opaque t() :: #journal{}.
-export_type([t/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec new() -> t().
new() ->
    #journal{
       error_tables = new_tables(),
       warning_tables = new_tables()
      }.

-spec bad_node_index_in_tree(t(), non_neg_integer(), string(), [non_neg_integer()])
        -> true.
bad_node_index_in_tree(Journal, NodeIndex, Prefix, Path) ->
    Error = bad_node_index_in_tree(NodeIndex, Prefix, Path),
    register_error(Journal, Error).

-spec bad_data_index_in_tree(t(), non_neg_integer(), term(),
                             string(), [non_neg_integer()])
        -> true.
bad_data_index_in_tree(Journal, DataIndex, Why, Prefix, Path) ->
    Error = bad_data_index_in_tree(DataIndex, Why, Prefix, Path),
    register_error(Journal, Error).

-spec excessively_long_path_in_tree(t(), non_neg_integer(), string(), [non_neg_integer()])
        -> true.
excessively_long_path_in_tree(Journal, NodeIndex, Prefix, Path) ->
    Error = excessively_long_path_in_tree(NodeIndex, Prefix, Path),
    register_error(Journal, Error).

-spec loop_in_data_section(t(), [{non_neg_integer(), term()}, ...]) -> true.
loop_in_data_section(Journal, Path) ->
    Error = loop_in_data_section(Path),
    register_error(Journal, Error).

-spec invalid_position_in_data_section(t(), non_neg_integer(),
                                       [{non_neg_integer(), term()}, ...])
        -> true.
invalid_position_in_data_section(Journal, Position, Path) ->
    Error = invalid_position_in_data_section(Position, Path),
    register_error(Journal, Error).

-spec bad_chunk_in_data_section(t(), non_neg_integer(), term(),
                                [{non_neg_integer(), term()}, ...])
        -> true.
bad_chunk_in_data_section(Journal, Position, Why, Path) ->
    Error = bad_chunk_in_data_section(Position, Why, Path),
    register_error(Journal, Error).

-spec map_key_of_wrong_type_in_data_section(t(), non_neg_integer(), term(),
                                            [{non_neg_integer(), term()}])
        -> true.
map_key_of_wrong_type_in_data_section(Journal, Position, Key, Path) ->
    Error = map_key_of_wrong_type_in_data_section(Position, Key, Path),
    register_error(Journal, Error).

-spec invalid_utf8_string_in_data_section(t(), non_neg_integer(), binary(),
                                          term(), [non_neg_integer()])
        -> true.
invalid_utf8_string_in_data_section(Journal, Position, OriginalData, ValidationError, Path) ->
    Error = invalid_utf8_string_in_data_section(Position, OriginalData, ValidationError, Path),
    register_error(Journal, Error).

-spec unprintable_utf8_string_in_data_section(t(), non_neg_integer(), unicode:unicode_binary(),
                                              [non_neg_integer()])
        -> true.
unprintable_utf8_string_in_data_section(Journal, Position, Value, Path) ->
    Warning = unprintable_utf8_string_in_data_section(Position, Value, Path),
    register_warning(Journal, Warning).

-spec check(t()) -> #{errors := [locus_mmdb_check:error()],
                      warnings := [locus_mmdb_check:warning()]}.
check(Journal) ->
    #{errors => tables_to_list(Journal#journal.error_tables),
      warnings => tables_to_list(Journal#journal.warning_tables)}.

-spec delete(t()) -> ok.
delete(Journal) ->
    delete_tables(Journal#journal.error_tables),
    delete_tables(Journal#journal.warning_tables).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec bad_node_index_in_tree(non_neg_integer(), string(), [non_neg_integer()])
        -> locus_mmdb_check:bad_node_index_in_tree().
bad_node_index_in_tree(NodeIndex, Prefix, Path) ->
    {bad_node_index_in_tree, NodeIndex, #{under => #{prefix => Prefix,
                                                     path => Path}}}.

-spec bad_data_index_in_tree(non_neg_integer(), term(), string(), [non_neg_integer()])
        -> locus_mmdb_check:bad_data_index_in_tree().
bad_data_index_in_tree(DataIndex, Why, Prefix, Path) ->
    {bad_data_index_in_tree, DataIndex, #{why => Why,
                                          under => #{prefix => Prefix,
                                                     path => Path}}}.

-spec excessively_long_path_in_tree(non_neg_integer(), string(), [non_neg_integer()])
        -> locus_mmdb_check:excessively_long_path_in_tree().
excessively_long_path_in_tree(NodeIndex, Prefix, Path) ->
    {excessively_long_path_in_tree, NodeIndex, #{under => #{prefix => Prefix,
                                                            path => Path}}}.

-spec loop_in_data_section([{non_neg_integer(), term()}, ...])
        -> locus_mmdb_check:loop_in_data_section().
loop_in_data_section(Path) ->
    {loop_in_data_section, #{path => Path}}.

-spec invalid_position_in_data_section(non_neg_integer(), [{non_neg_integer(), term()}, ...])
        -> locus_mmdb_check:invalid_position_in_data_section().
invalid_position_in_data_section(Position, Path) ->
    {invalid_position_in_data_section, #{which => Position,
                                         under => #{path => Path}}}.

-spec bad_chunk_in_data_section(non_neg_integer(), term(), [{non_neg_integer(), term()}, ...])
        -> locus_mmdb_check:bad_chunk_in_data_section().
bad_chunk_in_data_section(Position, Why, Path) ->
    {bad_chunk_in_data_section, #{position => Position,
                                  why => Why,
                                  under => #{path => Path}}}.

-spec map_key_of_wrong_type_in_data_section(non_neg_integer(), term(),
                                            [{non_neg_integer(), term()}])
        -> locus_mmdb_check:map_key_of_wrong_type_in_data_section().
map_key_of_wrong_type_in_data_section(Position, Key, Path) ->
    {map_key_of_wrong_type_in_data_section, #{position => Position,
                                              key => Key,
                                              under => #{path => Path}}}.

-spec invalid_utf8_string_in_data_section(non_neg_integer(), binary(),
                                          term(), [{non_neg_integer(), term()}])
        -> locus_mmdb_check:invalid_utf8_string_in_data_section().
invalid_utf8_string_in_data_section(Position, OriginalData, ValidationError, Path) ->
    {invalid_utf8_string_in_data_section, #{position => Position,
                                            original_data => OriginalData,
                                            error => ValidationError,
                                            under => #{path => Path}}}.

-spec unprintable_utf8_string_in_data_section(non_neg_integer(),
                                              unicode:unicode_binary(),
                                              [{non_neg_integer(), term()}])
        -> locus_mmdb_check:unprintable_utf8_string_in_data_section().
unprintable_utf8_string_in_data_section(Position, Value, Path) ->
    {unprintable_utf8_string_in_data_section, #{position => Position,
                                                value => Value,
                                                under => #{path => Path}}}.

register_error(#journal{error_tables = Tables}, Error) ->
    Table = pick_table(Tables),
    ets:insert(Table, {make_ref(), Error}).

register_warning(#journal{warning_tables = Tables}, Error) ->
    Table = pick_table(Tables),
    ets:insert(Table, {make_ref(), Error}).

new_tables() ->
    Amount = erlang:system_info(schedulers),
    list_to_tuple([new_table() || _ <- lists:seq(1, Amount)]).

new_table() ->
    ets:new(?MODULE, [public]).

pick_table(Tables) ->
    TableIndex = erlang:system_info(scheduler_id),
    element(TableIndex, Tables).

tables_to_list(Tables) ->
    lists:foldl(fun prepend_table_to_list/2, [], tuple_to_list(Tables)).

prepend_table_to_list(Table, Acc) ->
    ets:foldl(fun prepend_element_to_list/2, Acc, Table).

prepend_element_to_list({Ref, Element}, Acc)
  when is_reference(Ref) ->
    [Element | Acc].

delete_tables(Tables) ->
    lists:foreach(fun ets:delete/1, tuple_to_list(Tables)).
