%% Copyright (c) 2018-2023 Guilherme Andrade
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

-module(maxmind_main_mmdb_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(PROJECT_ROOT, "../../../../").
-define(TEST_SOURCES_REL_PATH, "_build/test/lib/maxmind_test_data/source-data/").
-define(TEST_DBS_REL_PATH, "_build/test/lib/maxmind_test_data/test-data/").

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    DatabasePathsPattern = filename:join([?PROJECT_ROOT, ?TEST_DBS_REL_PATH, "*.mmdb"]),
    DatabasePaths = filelib:wildcard(DatabasePathsPattern),
    lists:filtermap(
      fun (DatabasePath) ->
              DatabaseFilename = filename:basename(DatabasePath),
              GroupName = filename:rootname(DatabaseFilename),
              Group = list_to_atom(GroupName),
              {true, {Group, [parallel], test_cases()}}
      end,
      DatabasePaths).

test_cases() ->
    Exports = ?MODULE:module_info(exports),
    [Function || {Function, 1} <- Exports,
                 lists:suffix("_test", atom_to_list(Function))].

%%%%%%%%%%%%%%%
init_per_group(Group, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    {ok, _} = application:ensure_all_started(jsx),
    GroupName = atom_to_list(Group),
    IsBroken = guess_brokenness(GroupName),
    SourceFilename = GroupName ++ ".json",
    SourcePath = filename:join([?PROJECT_ROOT, ?TEST_SOURCES_REL_PATH, SourceFilename]),
    DatabaseFilename = GroupName ++ ".mmdb",
    DatabasePath = filename:join([?PROJECT_ROOT, ?TEST_DBS_REL_PATH, DatabaseFilename]),

    {ok, BinDatabase} = file:read_file(DatabasePath),
    case file:read_file(SourcePath) of
        {ok, BinGroupDef} when not IsBroken ->
            JsonGroupDef = jsx:decode(BinGroupDef),
            [{json_group_def, JsonGroupDef},
             {json_group_def_filename, SourceFilename},
             {bin_database, BinDatabase}
             | Config];
        {error, enoent} ->
            [{bin_database, BinDatabase},
             {json_group_def_filename, SourceFilename},
             {is_broken, IsBroken}
             | Config]
    end.

end_per_group(_Group, _Config) ->
    ok = application:stop(jsx),
    ok = application:stop(locus).

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

unpack_and_check_database_test(Config) ->
    case lists:member({is_broken, true}, Config) of
        false ->
            ?assertMatch(ok, unpack_and_check_database(Config));
        true ->
            ?assertMatch(
               {errors, [_|_], _},
               unpack_and_check_database(Config))
    end.


expected_lookup_results_test(Config) ->
    case lists:keymember(json_group_def, 1, Config) of
        true ->
            {json_group_def_filename, SourceFilename} = lists:keyfind(json_group_def_filename,
                                                                      1, Config),
            {ok, Database} = unpack_database(Config),
            lists:foreach(
              fun ({TestCaseIndex, Address, Expectation}) ->
                      Reality = determine_lookup_reality(Database, Address),
                      ?assertEqual(Expectation, Reality,
                                   unicode:characters_to_list(
                                     io_lib:format("Source filename: '~ts'"
                                                   ", test case index: ~b"
                                                   ", address ~p",
                                                   [SourceFilename, TestCaseIndex,
                                                    Address])
                                    ))
              end,
              expected_lookup_results(Config));
        false ->
            {skip, "No source available for comparison."}
    end.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

guess_brokenness(GroupName) ->
    LowerCase = string:to_lower(GroupName),
    lists:any(
      fun (Pattern) ->
              string:str(LowerCase, Pattern) > 0
      end,
      ["-broken-", "-invalid-"]).

unpack_and_check_database(Config) ->
    case unpack_database(Config) of
        {ok, Database} ->
            locus_mmdb_check:run(Database);
        {error, Reason} ->
            {errors, [Reason], []} % Dirty hack
    end.

unpack_database(Config) ->
    BinDatabase = proplists:get_value(bin_database, Config),
    locus_mmdb:unpack_database(BinDatabase).

expected_lookup_results(Config) ->
    JsonGroupDef = proplists:get_value(json_group_def, Config),
    JsonGroupDefIndices = lists:seq(1, length(JsonGroupDef)),
    EnumeratedJsonGroupDef = lists:zip(JsonGroupDefIndices, JsonGroupDef),
    lists:foldr(
      fun ({TestCaseIndex, JsonDict}, Acc) ->
              maps:fold(
                fun (Address, UncomparableSuccess, SubAcc) ->
                        expected_lookup_results(TestCaseIndex, Address,
                                                UncomparableSuccess,
                                                SubAcc)
                end,
                Acc, JsonDict)
      end,
      [], EnumeratedJsonGroupDef).

expected_lookup_results(TestCaseIndex, Address, UncomparableSuccess, Acc) ->
    ComparableSuccess = comparable_lookup_success(UncomparableSuccess),
    [{TestCaseIndex, Address, {ok, ComparableSuccess}} | Acc].

determine_lookup_reality(Database, Address) ->
    case locus_mmdb:lookup_address(Address, Database) of
        {ok, UncomparableSuccess} ->
            % UncomparableSuccess = maps:without([prefix], UncomparableSuccessWithExtras),
            Success = comparable_lookup_success(UncomparableSuccess),
            {ok, Success};
        {error, Reason} ->
            {error, Reason}
    end.

comparable_lookup_success(Map) when is_map(Map) ->
    maps:map(
      fun (_Key, Value) ->
              comparable_lookup_success(Value)
      end,
      Map);
comparable_lookup_success(List) when is_list(List) ->
    lists:map(
      fun (Value) ->
              comparable_lookup_success(Value)
      end,
      List);
comparable_lookup_success(Binary) when is_binary(Binary) ->
    try binary_to_float(Binary) of
        Float ->
            % No float geographical data in JSON test group
            Float
    catch
        error:badarg ->
            try binary_to_integer(Binary) of
                Integer ->
                    % No integer geographical data in JSON test group
                    Integer
            catch
                error:badarg ->
                    Binary
            end
    end;
comparable_lookup_success(Integer) when is_integer(Integer) ->
    Integer;
comparable_lookup_success(Float) when is_float(Float) ->
    case trunc(Float) == Float of
        true ->
            % ughhh
            trunc(Float);
        false ->
            Float
    end;
comparable_lookup_success(true) ->
    % No boolean flags in JSON test group
    1;
comparable_lookup_success(false) ->
    % No boolean flags in JSON test group
    0.
