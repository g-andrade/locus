%% Copyright (c) 2019-2024 Guilherme Andrade
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

-module(maxmind_bad_mmdb_SUITE).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

-define(PROJECT_ROOT, "../../../../").
-define(DATABASES_ROOT_DIR, "_build/test/lib/maxmind_test_data/bad-data").

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    BaseDatabasePath = filename:join(?PROJECT_ROOT, ?DATABASES_ROOT_DIR),
    DatabasePathsPattern = filename:join([BaseDatabasePath, "**", "*.mmdb"]),
    DatabasePaths = filelib:wildcard(DatabasePathsPattern),
    lists:filtermap(
      fun (DatabasePath) ->
              true = lists:prefix(BaseDatabasePath, DatabasePath),
              DatabasePathTailLength = length(DatabasePath) - length(BaseDatabasePath),
              DatabasePathTail = lists:sublist(DatabasePath, length(BaseDatabasePath) + 1,
                                               DatabasePathTailLength),
              Group = list_to_atom(DatabasePathTail),
              {true, {Group, [parallel], test_cases()}}
      end,
      DatabasePaths).

test_cases() ->
    Exports = ?MODULE:module_info(exports),
    [Function || {Function, 1} <- Exports,
                 lists:suffix("_test", atom_to_list(Function))].

%%%%%%%%%%%%%%%
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(locus),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(locus).

init_per_group(Group, Config) ->
    BaseDatabasePath = filename:join(?PROJECT_ROOT, ?DATABASES_ROOT_DIR),
    DatabasePathTail = atom_to_list(Group),
    DatabasePath = BaseDatabasePath ++ DatabasePathTail,
    [{group, Group},
     {database_path, DatabasePath}
     | Config].

end_per_group(_Group, _Config) ->
    ok.

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

fail_to_unpack_or_check_test(Config) ->
    {_, DatabasePath} = lists:keyfind(database_path, 1, Config),
    {ok, EncodedDatabase} = file:read_file(DatabasePath),
    case locus_mmdb:unpack_database(EncodedDatabase) of
        {ok, Database} ->
            ?assertMatch({errors, [_ | _], _},
                         locus_mmdb_check:run(Database));
        {error, _} ->
            ok
    end.
