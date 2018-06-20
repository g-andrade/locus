%% Copyright (c) 2018 Guilherme Andrade
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

-module(good_mmdb_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(PROJECT_ROOT, "../../../../").
-define(TESTS_GROUPS_REL_PATH, "test/MaxMind-DB/source-data/").
-define(TEST_DBS_REL_PATH, "test/MaxMind-DB/test-data/").

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

-ifdef(RUNNING_ON_TRAVIS).
groups() ->
    % This suite is too heavy for Travis. It runs ok but screws up
    % locus_SUITE which runs right after, e.g. with unexpected timeouts.
    % Probably CPU throttling of some sort.
    [].
-else.
groups() ->
    GroupPathsPattern = filename:join([?PROJECT_ROOT, ?TESTS_GROUPS_REL_PATH, "*.json"]),
    GroupPaths = filelib:wildcard(GroupPathsPattern),
    lists:map(
      fun (GroupPath) ->
              GroupFilename = filename:basename(GroupPath),
              GroupName = filename:rootname(GroupFilename),
              Group = list_to_atom(GroupName),
              {Group, [parallel], test_cases()}
      end,
      GroupPaths).
-endif.

test_cases() ->
    Exports = ?MODULE:module_info(exports),
    [Function || {Function,1} <- Exports,
                 lists:suffix("_test", atom_to_list(Function))].

%%%%%%%%%%%%%%%
init_per_group(Group, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    {ok, _} = application:ensure_all_started(jsx),
    GroupName = atom_to_list(Group),
    GroupFilename = GroupName ++ ".json",
    GroupPath = filename:join([?PROJECT_ROOT, ?TESTS_GROUPS_REL_PATH, GroupFilename]),
    DatabaseFilename = GroupName ++ ".mmdb",
    DatabasePath = filename:join([?PROJECT_ROOT, ?TEST_DBS_REL_PATH, DatabaseFilename]),
    %%
    {ok, BinGroupDef} = file:read_file(GroupPath),
    JsonGroupDef = jsx:decode(BinGroupDef, [return_maps]),
    %%
    {ok, BinDatabase} = file:read_file(DatabasePath),
    %%
    [{json_group_def, JsonGroupDef},
     {bin_database, BinDatabase}
     | Config].

end_per_group(_Group, Config) ->
    Config.

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

load_database_test(Config) ->
    ?assertMatch(
       {#{tree := _} = _DatabaseParts, {{_,_,_},{_,_,_}} = _DatabaseVersion},
       decode_database_parts(Config)).

expected_lookup_results_test(Config) ->
    {DatabaseParts, DatabaseVersion} = decode_database_parts(Config),
    ct:pal("Database version: ~p", [DatabaseVersion]),
    lists:foreach(
      fun ({Address, Expectation}) ->
              Reality = determine_lookup_reality(DatabaseParts, Address),
              ?assertEqual(Expectation, Reality)
      end,
      expected_lookup_results(Config)).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

decode_database_parts(Config) ->
    BinDatabase = proplists:get_value(bin_database, Config),
    Source = {filesystem, ""},
    locus_mmdb:decode_database_parts(BinDatabase, Source).

expected_lookup_results(Config) ->
    JsonGroupDef = proplists:get_value(json_group_def, Config),
    lists:foldr(
      fun (JsonDict, Acc) ->
              maps:fold(fun expected_lookup_results/3, Acc, JsonDict)
      end,
      [], JsonGroupDef).

expected_lookup_results(Address, UncomparableSuccess, Acc) ->
    ComparableSuccess = comparable_lookup_success(UncomparableSuccess),
    [{Address, {ok, ComparableSuccess}} | Acc].

determine_lookup_reality(DatabaseParts, Address) ->
    ct:pal("Looking up ~s", [Address]),
    {ok, ParsedAddress} = locus_util:parse_ip_address(Address),
    case locus_mmdb:lookup_([{database, DatabaseParts}], ParsedAddress) of
        {ok, UncomparableSuccessWithExtras} ->
            UncomparableSuccess = maps:without([prefix], UncomparableSuccessWithExtras),
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
