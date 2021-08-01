%% Copyright (c) 2017-2021 Guilherme Andrade
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

-module(locus_local_sources_SUITE).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

-define(assertRecv(Pattern),
        ((fun () -> receive Msg -> ?assertMatch((Pattern), Msg)
                    after 30000 -> error(timeout) end end)())).

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    LocalExtensions = ["tar.gz", "tgz", "tar", "mmdb", "mmdb.gz"],
    FileSystemTests
        = [{list_to_atom("from_filesystem_" ++ FileExtension),
            _Opts = [parallel],
            locus_test_utils:test_cases(?MODULE)}
           || FileExtension <- LocalExtensions],
    CustomFetcherTests
        = [{list_to_atom("from_custom_" ++ FileExtension),
            _Opts = [parallel],
            locus_test_utils:test_cases(?MODULE)}
           || FileExtension <- LocalExtensions],

    FileSystemTests ++ CustomFetcherTests.

%%%%%%%%%%%%%%%
init_per_group(GroupName, Config) ->
    ok = application:set_env(kernel, logger_level, debug),
    ok = locus_logger:set_loglevel(debug),
    {ok, _} = application:ensure_all_started(locus),

    case atom_to_list(GroupName) of
        "from_filesystem_" ++ FileExtension ->
            PathWithTestTarballs = locus_test_utils:path_with_test_tarballs(),
            DatabasePath = filename:join(PathWithTestTarballs, "GeoLite2-Country." ++ FileExtension),
            WrongPath = filename:join(PathWithTestTarballs, "foobarbarfoofoobar"),
            CorruptPath = filename:join(PathWithTestTarballs, "corruption." ++ FileExtension),
            [{group_type, filesystem},
             {load_from, DatabasePath},
             {wrong_load_from, WrongPath},
             {corrupt_load_from, CorruptPath}
             | Config];

        "from_custom_" ++ FileExtension ->
            ModifiedOn = calendar:universal_time(),

            {ok, GoodPid} = locus_test_custom_fetcher:start("GeoLite2-Country", FileExtension,
                                                            ModifiedOn),
            GoodFetcherArgs = #{locality => local, pid => GoodPid},
            GoodFetcher = {custom_fetcher, locus_test_custom_fetcher, GoodFetcherArgs},

            {ok, BadPid} = locus_test_custom_fetcher:start("foobarbarfoofoobar", FileExtension,
                                                           ModifiedOn),
            BadFetcherArgs = #{locality => local, pid => BadPid},
            BadFetcher = {custom_fetcher, locus_test_custom_fetcher, BadFetcherArgs},

            {ok, UglyPid} = locus_test_custom_fetcher:start("corruption", FileExtension,
                                                           ModifiedOn),
            UglyFetcherArgs = #{locality => local, pid => UglyPid},
            UglyFetcher = {custom_fetcher, locus_test_custom_fetcher, UglyFetcherArgs},

            [{group_type, custom_fetcher},
             {load_from, GoodFetcher},
             {wrong_load_from, BadFetcher},
             {corrupt_load_from, UglyFetcher},
             {custom_fetcher_pids, [GoodPid, BadPid, UglyPid]}
             | Config]
    end.

end_per_group(GroupName, Config) ->
    case atom_to_list(GroupName) of
        "from_filesystem_" ++ _FileExtension ->
            ok = application:stop(locus),
            Config;

        "from_custom_" ++ _FileExtension ->
            [GoodPid, BadPid, UglyPid] = proplists:get_value(custom_fetcher_pids, Config),
            ok = locus_test_custom_fetcher:stop(GoodPid),
            ok = locus_test_custom_fetcher:stop(BadPid),
            ok = locus_test_custom_fetcher:stop(UglyPid)
    end.

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

ipv4_country_lookup_test(Config) ->
    locus_common_tests:ipv4_country_lookup_test(Config).

ipv4_invalid_addr_test(Config) ->
    locus_common_tests:ipv4_invalid_addr_test(Config).

ipv6_country_lookup_test(Config) ->
    locus_common_tests:ipv6_country_lookup_test(Config).

ipv6_invalid_addr_test(Config) ->
    locus_common_tests:ipv6_invalid_addr_test(Config).

wrong_source_test(Config) ->
    WrongLoadFrom = proplists:get_value(wrong_load_from, Config),
    Loader = wrong_source_test,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, WrongLoadFrom, LoaderOpts),
    case proplists:get_value(group_type, Config) of
        filesystem ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {filesystem, _}}}),
            ?assertRecv({locus, Loader, {load_attempt_finished,
                                         {filesystem, _},
                                         {error, not_found}}});
        custom_fetcher ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {local, {custom, _}}}}),
            ?assertRecv({locus, Loader, {load_attempt_finished,
                                         {local, {custom, _}},
                                         {error, not_found}}})
    end,
    ok = locus:stop_loader(Loader).

corrupt_database_test(Config) ->
    CorruptLoadFrom = proplists:get_value(corrupt_load_from, Config),
    Loader = corrupt_database_test,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, CorruptLoadFrom, LoaderOpts),
    case proplists:get_value(group_type, Config) of
        filesystem ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {filesystem, _}}}),
            ?assertRecv({locus, Loader, {load_attempt_finished,
                                         {filesystem, _},
                                         {error, {decode_database_from, _, _}}}});
        custom_fetcher ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {local, {custom, _}}}}),
            ?assertRecv({locus, Loader, {load_attempt_finished,
                                         {local, {custom,  _}},
                                         {error, {decode_database_from, _, _}}}})
    end,
    ok = locus:stop_loader(Loader).

database_unknown_test(_Config) ->
    locus_common_tests:database_unknown_test().

invalid_args_test(Config) ->
    locus_common_tests:invalid_args_test(Config).

subscriber_death_test(Config) ->
    locus_common_tests:subscriber_death_test(Config).

loader_child_spec_test(Config) ->
    locus_common_tests:loader_child_spec_test(Config).

await_loader_failures_test(_Config) ->
    locus_common_tests:await_loader_failures_test().

wait_for_loader_failures_test(_Config) ->
    locus_common_tests:wait_for_loader_failures_test().

%%%

test_successful_loader_await(Loader) ->
    case rand:uniform(2) of
        1 ->
            {ok, LoadedVersion} = locus:await_loader(Loader),
            {ok, #{Loader := LoadedVersion}} = locus:await_loaders([Loader], 500),
            {ok, LoadedVersion} = locus:wait_for_loader(Loader),
            {ok, #{Loader := LoadedVersion}} = locus:wait_for_loaders([Loader], 500),
            LoadedVersion;
        2 ->
            {ok, LoadedVersion} = locus:wait_for_loader(Loader),
            {ok, #{Loader := LoadedVersion}} = locus:wait_for_loaders([Loader], 500),
            {ok, LoadedVersion} = locus:await_loader(Loader),
            {ok, #{Loader := LoadedVersion}} = locus:await_loaders([Loader], 500),
            LoadedVersion
    end.
