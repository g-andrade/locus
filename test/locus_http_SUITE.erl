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

-module(locus_http_SUITE).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-define(assertRecv(Pattern),
        ((fun () -> receive Msg -> ?assertMatch((Pattern), Msg)
                    after 30000 -> error(timeout) end end)())).

-define(VERSION1_TIMESTAMP, {{2018,01,01}, {00,00,00}}).
-define(VERSION2_TIMESTAMP, {{2018,02,01}, {00,00,00}}).

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    LocalExtensions = ["tar.gz", "tgz", "tar", "mmdb", "mmdb.gz"],
    LocalTests =
        [{list_to_atom("local_http_tests_" ++ FileExtension), [],
          locus_test_utils:test_cases(?MODULE, ["_test", "_localtest"])}
         || FileExtension <- LocalExtensions],

    case should_run_remote_http_tests() of
        false ->
            LocalTests;
        true ->
            LocalTests
            ++ [{remote_http_tests, [], locus_test_utils:test_cases(?MODULE, "_test")}]
    end.

should_run_remote_http_tests() ->
    currently_checkedout_commit_is_likely_tagged()
    andalso license_key_from_environment_is_defined().

currently_checkedout_commit_is_likely_tagged() ->
    {ok, _} = application:ensure_all_started(locus),
    Apps = application:which_applications(),
    {locus, _Descr, Vsn} = lists:keyfind(locus, 1, Apps),
    case string:tokens(Vsn, ".") of
        [_,_,_] -> true; % likely a tagged commit
        _ -> false
    end.

license_key_from_environment_is_defined() ->
    LicenseKey = license_key_from_environment(),
    LicenseKey =/= false andalso length(LicenseKey) > 0.

license_key_from_environment() ->
    os:getenv("MAXMIND_LICENSE_KEY").

%%%%%%%%%%%%%%%
init_per_group(GroupName, Config) ->
    case atom_to_list(GroupName) of
        "local_http_tests_" ++ FileExtension ->
            {ok, _} = application:ensure_all_started(locus),
            ok = application:set_env(kernel, logger_level, debug),
            ok = locus_logger:set_loglevel(debug),
            PathWithTestTarballs = locus_test_utils:path_with_test_tarballs(),
            {ok, HttpdPid, BaseURL} = locus_httpd:start(PathWithTestTarballs),
            DatabasePath = filename:join(PathWithTestTarballs, "GeoLite2-Country." ++ FileExtension),
            RandomAnchor = integer_to_list(rand:uniform(1 bsl 64), 36),
            DatabaseURL = BaseURL ++ "/GeoLite2-Country." ++ FileExtension ++ "#" ++ RandomAnchor,
            CorruptURL = BaseURL ++ "/corruption." ++ FileExtension,
            ok = set_file_mtime(DatabasePath, ?VERSION1_TIMESTAMP),
            [{is_http, true},
             {is_remote, false},
             {httpd_pid, HttpdPid},
             {load_from, DatabaseURL},
             {path, DatabasePath},
             {corrupt_url, CorruptURL},
             {base_url, BaseURL}
             | Config];
        "remote_http_tests" ->
            {ok, _} = application:ensure_all_started(locus),
            ok = application:set_env(kernel, logger_level, debug),
            ok = locus_logger:set_loglevel(debug),
            ok = application:set_env(locus, license_key, license_key_from_environment()),
            Edition = {maxmind, 'GeoLite2-Country'},
            [{is_http, true},
             {is_remote, true},
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
    % check events
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),

    _ = case LoadFrom of
            {maxmind, _} ->
                ?assertRecv({locus, Loader, {checksum, {request_sent, _ChecksumURL,
                                                        _ChecksumReqHeaders}}}),
                ?assertRecv({locus, Loader, {checksum, {download_started,
                                                        _ChecksumRespHeaders}}}),
                ?assertRecv({locus, Loader, {checksum, {download_finished,
                                                        _BytesReceived,
                                                        {ok, _TrailingChecksumRespHeaders}}}});
            _ ->
                ok
        end,

    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, LoadedVersion}}}),
    % check info
    ?assertMatch({ok, #{ metadata := #{}, source := {remote,_}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertMatch({ok, {remote,_}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).

cold_remote_loading_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = cold_regular_loading_test,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
    LoadedVersion = locus_common_tests:test_successful_loader_await(Loader),
    % check events
    ?assertRecv({locus, Loader, {load_attempt_finished, {cache,_}, {error,_}}}),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),

    _ = case LoadFrom of
            {maxmind, _} ->
                ?assertRecv({locus, Loader, {checksum, {request_sent, _ChecksumURL,
                                                        _ChecksumReqHeaders}}}),
                ?assertRecv({locus, Loader, {checksum, {download_started,
                                                        _ChecksumRespHeaders}}}),
                ?assertRecv({locus, Loader, {checksum, {download_finished,
                                                        _BytesReceived,
                                                        {ok, _TrailingChecksumRespHeaders}}}});
            _ ->
                ok
        end,

    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, LoadedVersion}}}),
    ?assertRecv({locus, Loader, {cache_attempt_finished, _CacheFilename, ok}}),
    % check info
    ?assertMatch({ok, #{ metadata := #{}, source := {remote,_}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertMatch({ok, {remote,_}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).

warm_remote_loading_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = warm_regular_loading_test,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
    LoadedVersion = locus_common_tests:test_successful_loader_await(Loader),
    CacheFilename = cached_database_path(LoadFrom),
    % check events
    ?assertRecv({locus, Loader, {load_attempt_finished, {cache,_}, {ok,LoadedVersion}}}),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_dismissed, {http, {304,_}, _Headers, _Body}}}),
    % check info
    ?assertMatch({ok, #{ metadata := #{}, source := {cache,CacheFilename}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertEqual({ok, {cache,CacheFilename}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).

update_works_test(Config) ->
    IsRemote = proplists:get_value(is_remote, Config),
    update_works_test(IsRemote, Config).

update_works_test(IsRemote, _Config) when IsRemote ->
    {skip, "Unable to tweak modification time of remote files"};
update_works_test(_IsRemote, Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Path = proplists:get_value(path, Config),
    Loader = update_works_test,
    UpdatePeriod = 200,
    LoaderOpts = [no_cache,
                  {update_period, UpdatePeriod},
                  {event_subscriber, self()}],
    %%
    ok = set_file_mtime(Path, ?VERSION1_TIMESTAMP),
    ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, _}}}),
    %%
    ok = set_file_mtime(Path, ?VERSION2_TIMESTAMP),
    {TimeElapsedA, _} = timer:tc(fun () -> ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}) end),
    MillisecondsElapsedA = TimeElapsedA / 1000,
    ct:pal("MillsecondsElapsed: ~p", [MillisecondsElapsedA]),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, _}}}),
    ?assert(MillisecondsElapsedA / UpdatePeriod >= 0.90),
    %%
    {TimeElapsedB, _} = timer:tc(fun () -> ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}) end),
    MillisecondsElapsedB = TimeElapsedB / 1000,
    ct:pal("MillsecondsElapsed: ~p", [MillisecondsElapsedB]),
    ?assertRecv({locus, Loader, {download_dismissed, {http, {304,_}, _Headers, _Body}}}),
    ?assert(MillisecondsElapsedB / UpdatePeriod >= 0.90),
    %%
    ok = locus:stop_loader(Loader).

ipv4_country_lookup_test(Config) ->
    locus_common_tests:ipv4_country_lookup_test(Config).

ipv4_invalid_addr_test(Config) ->
    locus_common_tests:ipv4_invalid_addr_test(Config).

ipv6_country_lookup_test(Config) ->
    locus_common_tests:ipv6_country_lookup_test(Config).

ipv6_invalid_addr_test(Config) ->
    locus_common_tests:ipv6_invalid_addr_test(Config).

connect_timeout_test(Config) ->
    % Undeterministic test case
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = connect_timeout_test,
    LoaderOpts = [no_cache, {connect_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
             ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
             try ?assertRecv({locus, Loader, {download_failed_to_start, {error, {failed_connect, _}}}}) of
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

download_start_timeout_test(Config) ->
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

idle_download_timeout_test(Config) ->
    % Undeterministic test case
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = idle_download_timeout_test,
    LoaderOpts = [no_cache, {idle_download_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
             ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
             ?assertRecv({locus, Loader, {download_started, _Headers}}),
             try ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {error, timeout}}}) of
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

wrong_url_localtest(Config) ->
    BaseURL = proplists:get_value(base_url, Config),
    URL = BaseURL ++ "/foobarbarfoofoobar",
    Loader = wrong_url_test,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_failed_to_start,
                                 {http, {404 = _StatusCode, _StatusDesc}, _Headers, _Body}}}),
    ok = locus:stop_loader(Loader).

corrupt_database_localtest(Config) ->
    CorruptURL = proplists:get_value(corrupt_url, Config),
    Loader = corrupt_database_test,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, CorruptURL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, CorruptURL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {error, _}}}),
    ok = locus:stop_loader(Loader).

database_unknown_test(_Config) ->
    locus_common_tests:database_unknown_test().

database_still_loading_localtest(Config) ->
    BaseURL = proplists:get_value(base_url, Config),
    URL = BaseURL ++ "/foobarbarfoofoobar",
    Loader = database_still_loading_test,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_failed_to_start,
                                 {http, {404 = _StatusCode, _StatusDesc}, _Headers, _Body}}}),
    ?assertEqual({error, database_not_loaded}, locus:lookup(Loader, "127.0.0.1")),
    ?assertEqual({error, database_not_loaded}, locus:get_version(Loader)),
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

wait_for_loader_failures_test(_Config) ->
    locus_common_tests:wait_for_loader_failures_test().

%%%

max_undeterministic_attempts(Config) ->
    case proplists:get_value(is_remote, Config) of
        true -> 10;
        false -> 1000
    end.

set_file_mtime(Path, DateTime) ->
    FileInfoMod = #file_info{ mtime = DateTime },
    file:write_file_info(Path, FileInfoMod, [{time,universal}]).

clear_proc_inbox_of_events(Loader) ->
    receive
        {locus, Loader, _Event} ->
            clear_proc_inbox_of_events(Loader)
    after
        0 ->
            ok
    end.

cached_database_path({maxmind, _} = Edition) ->
    {maxmind, ParsedEditionName} = locus:parse_database_edition(Edition),
    locus_loader:cached_database_path_for_maxmind_edition_name(ParsedEditionName, undefined);
cached_database_path(URL) ->
    locus_loader:cached_database_path_for_url(URL).
