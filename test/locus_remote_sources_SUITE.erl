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

-module(locus_remote_sources_SUITE).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-define(assertRecv(Pattern),
        ((fun () -> receive Msg -> ?assertMatch((Pattern), Msg)
                    after 30000 -> error(timeout) end end)())).

-define(VERSION1_TIMESTAMP, {{2018, 01, 01}, {00, 00, 00}}).
-define(VERSION2_TIMESTAMP, {{2018, 02, 01}, {00, 00, 00}}).

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    LocalExtensions = ["tar.gz", "tgz", "tar", "mmdb", "mmdb.gz"],
    LocalTests =
        [{list_to_atom("local_http_tests_" ++ FileExtension), [],
          locus_test_utils:test_cases(?MODULE, ["_test", "_localtest",
                                                "_httptest", "_localhttptest"])}
         || FileExtension <- LocalExtensions],

    CustomFetcherTests
        = [{list_to_atom("custom_fetcher_tests_" ++ FileExtension),
            _Opts = [],
            locus_test_utils:test_cases(?MODULE, ["_test", "_localtest",
                                                  "_customtest"])}
           || FileExtension <- LocalExtensions],


    case should_run_remote_http_tests() of
        false ->
            LocalTests
            ++ CustomFetcherTests;
        true ->
            LocalTests
            ++ CustomFetcherTests
            ++ [{remote_http_tests,
                 _Opts = [],
                 locus_test_utils:test_cases(?MODULE, ["_test", "_httptest"])}]
    end.

should_run_remote_http_tests() ->
    currently_checkedout_commit_is_likely_tagged()
    andalso license_key_from_environment_is_defined().

currently_checkedout_commit_is_likely_tagged() ->
    {ok, _} = application:ensure_all_started(locus),
    Apps = application:which_applications(),
    {locus, _Descr, Vsn} = lists:keyfind(locus, 1, Apps),
    case string:tokens(Vsn, ".") of
        [_, _, _] -> true; % likely a tagged commit
        _ -> false
    end.

license_key_from_environment_is_defined() ->
    LicenseKey = license_key_from_environment(),
    LicenseKey =/= false andalso length(LicenseKey) > 0.

license_key_from_environment() ->
    os:getenv("MAXMIND_LICENSE_KEY").

%%%%%%%%%%%%%%%
init_per_group(GroupName, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    ok = application:set_env(kernel, logger_level, debug),
    ok = locus_logger:set_loglevel(debug),

    case atom_to_list(GroupName) of
        "local_http_tests_" ++ FileExtension ->
            PathWithTestTarballs = locus_test_utils:path_with_test_tarballs(),
            {ok, HttpdPid, BaseURL} = locus_httpd:start(PathWithTestTarballs),
            DatabasePath = filename:join(PathWithTestTarballs,
                                         "GeoLite2-Country." ++ FileExtension),
            RandomAnchor = integer_to_list(rand:uniform(1 bsl 64), 36),
            DatabaseURL = BaseURL ++ "/GeoLite2-Country." ++ FileExtension ++ "#" ++ RandomAnchor,
            WrongURL = BaseURL ++ "/foobarbarfoofoobar",
            CorruptURL = BaseURL ++ "/corruption." ++ FileExtension,
            ok = set_file_mtime(DatabasePath, ?VERSION1_TIMESTAMP),
            [{group_type, local_http},
             {httpd_pid, HttpdPid},
             {load_from, DatabaseURL},
             {wrong_load_from, WrongURL},
             {corrupt_load_from, CorruptURL},
             {local_path, DatabasePath}
             | Config];

        "custom_fetcher_tests_" ++ FileExtension ->
            ModifiedOn = calendar:universal_time(),

            {ok, GoodPid} = locus_test_custom_fetcher:start("GeoLite2-Country", FileExtension,
                                                            ModifiedOn),
            GoodFetcherArgs = #{locality => remote, pid => GoodPid},
            GoodFetcher = {custom_fetcher, locus_test_custom_fetcher, GoodFetcherArgs},

            {ok, BadPid} = locus_test_custom_fetcher:start("foobarbarfoofoobar", FileExtension,
                                                           ModifiedOn),
            BadFetcherArgs = #{locality => remote, pid => BadPid},
            BadFetcher = {custom_fetcher, locus_test_custom_fetcher, BadFetcherArgs},

            {ok, UglyPid} = locus_test_custom_fetcher:start("corruption", FileExtension,
                                                           ModifiedOn),
            UglyFetcherArgs = #{locality => remote, pid => UglyPid},
            UglyFetcher = {custom_fetcher, locus_test_custom_fetcher, UglyFetcherArgs},

            [{group_type, custom_fetcher},
             {load_from, GoodFetcher},
             {wrong_load_from, BadFetcher},
             {corrupt_load_from, UglyFetcher},
             {custom_fetcher_pids, [GoodPid, BadPid, UglyPid]}
             | Config];

        "remote_http_tests" ->
            ok = application:set_env(locus, license_key, license_key_from_environment()),
            Edition = {maxmind, 'GeoLite2-Country'},
            [{group_type, remote_http},
             {load_from, Edition}
             | Config]
    end.

end_per_group(GroupName, Config) ->
    case atom_to_list(GroupName) of
        "local_http_tests_" ++ _FileExtension ->
            URL = proplists:get_value(load_from, Config),
            CacheFilename = locus_loader:cached_database_path_for_url(URL),
            HttpdPid = proplists:get_value(httpd_pid, Config),

            ok = application:stop(locus),
            ok = locus_httpd:stop(HttpdPid),
            _ = file:delete(CacheFilename),
            Config;

        "custom_fetcher_tests_" ++ _FileExtension ->
            {custom_fetcher, Module, _Args} = proplists:get_value(load_from, Config),
            [GoodPid, BadPid, UglyPid] = proplists:get_value(custom_fetcher_pids, Config),
            FetchedFrom = locus_test_custom_fetcher:get_database_is_fetched_from(GoodPid),
            CacheFilename = locus_loader:cached_database_path_for_custom_fetcher(Module,
                                                                                 FetchedFrom),

            ok = application:stop(locus),
            _ = file:delete(CacheFilename),
            ok = locus_test_custom_fetcher:stop(GoodPid),
            ok = locus_test_custom_fetcher:stop(BadPid),
            ok = locus_test_custom_fetcher:stop(UglyPid),
            Config;

        "remote_http_tests" ->
            {maxmind, MaxMindEditionName}
                = proplists:get_value(load_from, Config),
            Date = undefined,
            CacheFilename = locus_loader:cached_database_path_for_maxmind_edition_name(
                              MaxMindEditionName, Date),

            ok = application:stop(locus),
            _ = file:delete(CacheFilename),
            Config
    end.

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

cacheless_loading_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = cacheless_loading_test,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
    LoadedVersion = locus_common_tests:test_successful_loader_await(Loader),

    case LoadFrom of
        "http" ++ _ ->
            ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
            ?assertRecv({locus, Loader, {download_started, _Headers}}),
            ?assertRecv({locus, Loader, {download_finished, _BytesReceived,
                                         {ok, _TrailingHeaders}}});

        {maxmind, _} ->
            ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
            ?assertRecv({locus, Loader, {download_started, _Headers}}),
            ?assertRecv({locus, Loader, {download_finished, _BytesReceived,
                                         {ok, _TrailingHeaders}}}),

            ?assertRecv({locus, Loader, {checksum, {request_sent, _ChecksumURL,
                                                    _ChecksumReqHeaders}}}),
            ?assertRecv({locus, Loader, {checksum, {download_started,
                                                    _ChecksumRespHeaders}}}),
            ?assertRecv({locus, Loader, {checksum, {download_finished,
                                                    _BytesReceived,
                                                    {ok, _TrailingChecksumRespHeaders}}}});

        {custom_fetcher, _, _} ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {remote, {custom, _}}}})
    end,

    ?assertRecv({locus, Loader, {load_attempt_finished, {remote, _}, {ok, LoadedVersion}}}),
    ?assertMatch({ok, #{ metadata := #{}, source := {remote, _}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertMatch({ok, {remote, _}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).

cold_remote_loading_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = cold_regular_loading_test,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
    LoadedVersion = locus_common_tests:test_successful_loader_await(Loader),
    ?assertRecv({locus, Loader, {load_attempt_finished, {cache, _}, {error, _}}}),

    case LoadFrom of
        "http" ++ _ ->
            ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
            ?assertRecv({locus, Loader, {download_started, _Headers}}),
            ?assertRecv({locus, Loader, {download_finished, _BytesReceived,
                                         {ok, _TrailingHeaders}}});

        {maxmind, _} ->
            ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
            ?assertRecv({locus, Loader, {download_started, _Headers}}),
            ?assertRecv({locus, Loader, {download_finished, _BytesReceived,
                                         {ok, _TrailingHeaders}}}),

            ?assertRecv({locus, Loader, {checksum, {request_sent, _ChecksumURL,
                                                    _ChecksumReqHeaders}}}),
            ?assertRecv({locus, Loader, {checksum, {download_started,
                                                    _ChecksumRespHeaders}}}),
            ?assertRecv({locus, Loader, {checksum, {download_finished,
                                                    _BytesReceived,
                                                    {ok, _TrailingChecksumRespHeaders}}}});

        {custom_fetcher, _, _} ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {remote, {custom, _}}}})
    end,

    ?assertRecv({locus, Loader, {load_attempt_finished, {remote, _}, {ok, LoadedVersion}}}),
    ?assertRecv({locus, Loader, {cache_attempt_finished, _CacheFilename, ok}}),
    ?assertMatch({ok, #{ metadata := #{}, source := {remote, _}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertMatch({ok, {remote, _}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).

warm_remote_loading_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = warm_regular_loading_test,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
    LoadedVersion = locus_common_tests:test_successful_loader_await(Loader),
    CacheFilename = cached_database_path(LoadFrom, Config),
    ?assertRecv({locus, Loader, {load_attempt_finished, {cache, _}, {ok, LoadedVersion}}}),

    case LoadFrom of
        {custom_fetcher, _, _} ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {remote, {custom, _}}}}),
            ?assertRecv({locus, Loader, {load_attempt_dismissed,
                                         {remote, {custom, _}}}});
        _HttpOrMaxMind ->
            ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
            ?assertRecv({locus, Loader, {download_dismissed, {http, {304, _}, _Headers, _Body}}})
    end,

    ?assertMatch({ok, #{ metadata := #{},
                         source := {cache, CacheFilename},
                         version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertEqual({ok, {cache, CacheFilename}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).

update_works_localhttptest(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = update_works_test,
    UpdatePeriod = 200,
    LoaderOpts = [no_cache,
                  {update_period, UpdatePeriod},
                  {event_subscriber, self()}],

    %% First load
    Path = proplists:get_value(local_path, Config),
    ok = set_file_mtime(Path, ?VERSION1_TIMESTAMP),
    ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote, _}, {ok, _}}}),

    %% Update
    ok = set_file_mtime(Path, ?VERSION2_TIMESTAMP),
    {TimeElapsedA, _}
    = timer:tc(
        fun () ->
                ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}})
        end),
    MillisecondsElapsedA = TimeElapsedA / 1000,
    ct:pal("MillsecondsElapsed: ~p", [MillisecondsElapsedA]),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote, _}, {ok, _}}}),
    ?assert(MillisecondsElapsedA / UpdatePeriod >= 0.90),

    %% Dismissal
    {TimeElapsedB, _}
    = timer:tc(
        fun () ->
                ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}})
        end),
    MillisecondsElapsedB = TimeElapsedB / 1000,
    ct:pal("MillsecondsElapsed: ~p", [MillisecondsElapsedB]),
    ?assertRecv({locus, Loader, {download_dismissed, {http, {304, _}, _Headers, _Body}}}),
    ?assert(MillisecondsElapsedB / UpdatePeriod >= 0.90),

    ok = locus:stop_loader(Loader).

update_works_customtest(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = update_works_test,
    UpdatePeriod = 200,
    LoaderOpts = [no_cache,
                  {update_period, UpdatePeriod},
                  {event_subscriber, self()}],


    %% First load
    [GoodPid, _BadPid, _UglyPid] = proplists:get_value(custom_fetcher_pids, Config),
    locus_test_custom_fetcher:set_modification_datetime(GoodPid, ?VERSION1_TIMESTAMP),
    ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
    ?assertRecv({locus, Loader, {load_attempt_started,
                                 {remote, {custom, _}}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished,
                                 {remote, {custom, _}},
                                 {ok, _}}}),

    %% Update
    locus_test_custom_fetcher:set_modification_datetime(GoodPid, ?VERSION2_TIMESTAMP),
    {TimeElapsedA, _}
    = timer:tc(
        fun () ->
                ?assertRecv({locus, Loader, {load_attempt_started,
                                             {remote, {custom, _}}}})
        end),
    MillisecondsElapsedA = TimeElapsedA / 1000,
    ct:pal("MillsecondsElapsed: ~p", [MillisecondsElapsedA]),
    ?assertRecv({locus, Loader, {load_attempt_finished,
                                 {remote, {custom, _}},
                                 {ok, _}}}),

    ?assert(MillisecondsElapsedA / UpdatePeriod >= 0.90),

    %% Dismissal
    {TimeElapsedB, _}
    = timer:tc(
        fun () ->
                ?assertRecv({locus, Loader, {load_attempt_started,
                                             {remote, {custom, _}}}})
        end),
    MillisecondsElapsedB = TimeElapsedB / 1000,
    ct:pal("MillsecondsElapsed: ~p", [MillisecondsElapsedB]),
    ?assertRecv({locus, Loader, {load_attempt_dismissed,
                                 {remote, {custom, _}}}}),
    ?assert(MillisecondsElapsedB / UpdatePeriod >= 0.90),

    ok = locus:stop_loader(Loader).

ipv4_country_lookup_test(Config) ->
    locus_common_tests:ipv4_country_lookup_test(Config).

ipv4_invalid_addr_test(Config) ->
    locus_common_tests:ipv4_invalid_addr_test(Config).

ipv6_country_lookup_test(Config) ->
    locus_common_tests:ipv6_country_lookup_test(Config).

ipv6_invalid_addr_test(Config) ->
    locus_common_tests:ipv6_invalid_addr_test(Config).

connect_timeout_httptest(Config) ->
    % Undeterministic test case
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = connect_timeout_test,
    LoaderOpts = [no_cache, {connect_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
             ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
             try ?assertRecv({locus, Loader, {download_failed_to_start,
                                              {error, {failed_connect, _}}}})
             of
                 _ ->
                     ok = locus:stop_loader(Loader)
             catch
                 _Class:_Reason when AttemptsLeft >= 1 ->
                     ct:pal("~p re-attempts left...", [AttemptsLeft - 1]),
                     ok = locus:stop_loader(Loader),
                     clear_proc_inbox_of_events(Loader),
                     F(AttemptsLeft - 1)
             end
     end(MaxAttempts)).

download_start_timeout_httptest(Config) ->
    %% Undeterministic test case
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = download_start_timeout_test,
    LoaderOpts = [no_cache, {download_start_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
             ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
             try ?assertRecv({locus, Loader, {download_failed_to_start, timeout}}) of
                 _ ->
                     ok = locus:stop_loader(Loader)
             catch
                 _Class:_Reason when AttemptsLeft >= 1 ->
                     ct:pal("~p re-attempts left...", [AttemptsLeft - 1]),
                     ok = locus:stop_loader(Loader),
                     clear_proc_inbox_of_events(Loader),
                     F(AttemptsLeft - 1)
             end
     end)(MaxAttempts).

idle_download_timeout_httptest(Config) ->
    % Undeterministic test case
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = idle_download_timeout_test,
    LoaderOpts = [no_cache, {idle_download_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
             ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
             ?assertRecv({locus, Loader, {download_started, _Headers}}),
             try ?assertRecv({locus, Loader, {download_finished, _BytesReceived,
                                              {error, timeout}}})
             of
                 _ ->
                     ok = locus:stop_loader(Loader)
             catch
                 _Class:_Reason when AttemptsLeft >= 1 ->
                     ct:pal("~p re-attempts left...", [AttemptsLeft - 1]),
                     ok = locus:stop_loader(Loader),
                     clear_proc_inbox_of_events(Loader),
                     F(AttemptsLeft - 1)
             end
     end(MaxAttempts)).

wrong_source_localtest(Config) ->
    WrongLoadFrom = proplists:get_value(wrong_load_from, Config),
    Loader = wrong_source_test,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, WrongLoadFrom, LoaderOpts),

    case WrongLoadFrom of
        "http" ++ _ = URL ->
            ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
            ?assertRecv({locus, Loader, {download_failed_to_start,
                                         {http, {404 = _StatusCode, _StatusDesc},
                                          _Headers, _Body}}});
        {custom_fetcher, _, _} ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {remote, {custom, _}}}}),
            ?assertRecv({locus, Loader, {load_attempt_finished,
                                         {remote, {custom, _}},
                                         {error, not_found}}})
    end,
    ok = locus:stop_loader(Loader).

corrupt_database_localtest(Config) ->
    CorruptLoadFrom = proplists:get_value(corrupt_load_from, Config),
    Loader = corrupt_database_test,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, CorruptLoadFrom, LoaderOpts),

    case CorruptLoadFrom of
        "http" ++ _ = URL ->
            ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
            ?assertRecv({locus, Loader, {download_started, _Headers}}),
            ?assertRecv({locus, Loader, {download_finished, _BytesReceived,
                                         {ok, _TrailingHeaders}}}),
            ?assertRecv({locus, Loader, {load_attempt_finished,
                                         {remote, URL},
                                         {error, {unpack_database_from, _, _}}}});
        {custom_fetcher, _, _} ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {remote, {custom, _}}}}),
            ?assertRecv({locus, Loader, {load_attempt_finished,
                                         {remote, {custom, _}},
                                         {error, {unpack_database_from, _, _}}}})
    end,
    ok = locus:stop_loader(Loader).

database_unknown_test(_Config) ->
    locus_common_tests:database_unknown_test().

database_still_loading_localtest(Config) ->
    WrongLoadFrom = proplists:get_value(wrong_load_from, Config),
    Loader = database_still_loading_test,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, WrongLoadFrom, LoaderOpts),

    case WrongLoadFrom of
        "http" ++ _ ->
            ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
            ?assertRecv({locus, Loader, {download_failed_to_start,
                                         {http, {404 = _StatusCode, _StatusDesc},
                                          _Headers, _Body}}});
        {custom_fetcher, _, _} ->
            ?assertRecv({locus, Loader, {load_attempt_started,
                                         {remote, {custom, _}}}}),
            ?assertRecv({locus, Loader, {load_attempt_finished,
                                         {remote, {custom, _}},
                                         {error, not_found}}})
    end,

    ?assertEqual({error, database_not_loaded}, locus:lookup(Loader, "127.0.0.1")),
    ?assertEqual({error, database_not_loaded}, locus:get_info(Loader)),
    ?assertEqual({error, database_not_loaded}, locus:get_info(Loader, metadata)),
    ?assertEqual({error, database_not_loaded}, locus:get_info(Loader, source)),
    ?assertEqual({error, database_not_loaded}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).

invalid_args_test(Config) ->
    locus_common_tests:invalid_args_test(Config).

subscriber_death_test(Config) ->
    locus_common_tests:subscriber_death_test(Config).

loader_child_spec_test(Config) ->
    locus_common_tests:loader_child_spec_test(Config).

await_loader_failures_test(_Config) ->
    locus_common_tests:await_loader_failures_test().

%%%

max_undeterministic_attempts(Config) ->
    % Doesn't apply to `custom_fetcher' tests
    case proplists:get_value(group_type, Config) of
        local_http -> 1000;
        remote_http -> 10
    end.

set_file_mtime(Path, DateTime) ->
    FileInfoMod = #file_info{ mtime = DateTime },
    file:write_file_info(Path, FileInfoMod, [{time, universal}]).

clear_proc_inbox_of_events(Loader) ->
    receive
        {locus, Loader, _Event} ->
            clear_proc_inbox_of_events(Loader)
    after
        0 ->
            ok
    end.

cached_database_path({maxmind, _} = Edition, _Config) ->
    {maxmind, ParsedEditionName} = locus:parse_database_edition(Edition),
    locus_loader:cached_database_path_for_maxmind_edition_name(ParsedEditionName, undefined);
cached_database_path("http" ++ _ = URL, _Config) ->
    locus_loader:cached_database_path_for_url(URL);
cached_database_path({custom_fetcher, Module, _Args}, Config) ->
    [GoodPid, _BadPid, _UglyPid] = proplists:get_value(custom_fetcher_pids, Config),
    FetchedFrom = locus_test_custom_fetcher:get_database_is_fetched_from(GoodPid),
    locus_loader:cached_database_path_for_custom_fetcher(Module, FetchedFrom).
