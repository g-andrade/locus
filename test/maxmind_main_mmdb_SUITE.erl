%% Copyright (c) 2018-2020 Guilherme Andrade
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
-define(TEST_SOURCES_REL_PATH, "test/MaxMind-DB/source-data/").
-define(TEST_DBS_REL_PATH, "test/MaxMind-DB/test-data/").

-define(BLACKLIST, ["MaxMind-DB-no-ipv4-search-tree",
                    "MaxMind-DB-test-metadata-pointers"]).

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

-ifdef(RUNNING_ON_CI).
groups() ->
    % This suite is too heavy for Travis. It runs ok but screws up
    % locus_SUITE which runs right after, e.g. with unexpected timeouts.
    % Probably CPU throttling of some sort.
    [].
-else.
groups() ->
    DatabasePathsPattern = filename:join([?PROJECT_ROOT, ?TEST_DBS_REL_PATH, "*.mmdb"]),
    DatabasePaths = filelib:wildcard(DatabasePathsPattern),
    lists:filtermap(
      fun (DatabasePath) ->
              DatabaseFilename = filename:basename(DatabasePath),
              GroupName = filename:rootname(DatabaseFilename),
              (not lists:member(GroupName, ?BLACKLIST) andalso
               begin
                   Group = list_to_atom(GroupName),
                   {true, {Group, [parallel], test_cases()}}
               end)
      end,
      DatabasePaths).
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
             {bin_database, BinDatabase}
             | Config];
        {error, enoent} ->
            [{bin_database, BinDatabase},
             {is_broken, IsBroken}
             | Config]
    end.

end_per_group(_Group, _Config) ->
    ok = application:stop(locus).

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

load_database_test(Config) ->
    case lists:member({is_broken,true}, Config) of
        false ->
            ?assertMatch(
               {{{_,_,_},{_,_,_}} = _DatabaseVersion,
                #{tree := _} = _DatabaseParts},
               decode_database_parts(Config));
        true ->
            ?assertError(
               _,
               begin
                   {DatabaseVersion, DatabaseParts} = decode_database_parts(Config),
                   ct:pal("Loaded version ~p", [DatabaseVersion]),
                   ok = locus_mmdb_analysis:run_(DatabaseParts)
               end)
    end.

expected_lookup_results_test(Config) ->
    case lists:keymember(json_group_def, 1, Config) of
        true ->
            {DatabaseVersion, DatabaseParts} = decode_database_parts(Config),
            ct:pal("Database version: ~p", [DatabaseVersion]),
            lists:foreach(
              fun ({Address, Expectation}) ->
                      Reality = determine_lookup_reality(DatabaseParts, Address),
                      ?assertEqual(Expectation, Reality)
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

decode_database_parts(Config) ->
    BinDatabase = proplists:get_value(bin_database, Config),
    Source = {filesystem, ""},
    locus_mmdb:decode_database_parts(Source, BinDatabase).

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
    case locus_mmdb:lookup_(ParsedAddress, DatabaseParts) of
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
