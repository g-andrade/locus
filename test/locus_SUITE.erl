%% Copyright (c) 2017-2020 Guilherme Andrade
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

-module(locus_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(PATH_WITH_TEST_TARBALLS, "../../../../test/priv").

-define(IPV4_STR_ADDR, "93.184.216.34"). % example.com
-define(IPV6_STR_ADDR, "2606:2800:220:1:248:1893:25c8:1946"). % example.com

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
    FilesystemTests =
        [{list_to_atom("filesystem_tests_" ++ FileExtension), [], test_cases("_fstest")}
         || FileExtension <- LocalExtensions],
    LocalHttpTests =
        [{list_to_atom("local_http_tests_" ++ FileExtension), [], (test_cases("_httptest") ++
                                                                   test_cases("_localhttptest"))}
         || FileExtension <- LocalExtensions],

    case should_run_remote_http_tests() of
        false -> FilesystemTests ++ LocalHttpTests;
        true ->
            FilesystemTests ++ LocalHttpTests
            ++ [{remote_http_tests, [], test_cases("_httptest")}]
    end.

-ifdef(RUNNING_ON_CI).
should_run_remote_http_tests() ->
    false.
-else.
should_run_remote_http_tests() ->
    currently_checkedout_commit_is_likely_tagged()
    andalso (license_key_from_environment() =/= undefined).

currently_checkedout_commit_is_likely_tagged() ->
    {ok, _} = application:ensure_all_started(locus),
    Apps = application:which_applications(),
    {locus, _Descr, Vsn} = lists:keyfind(locus, 1, Apps),
    case string:tokens(Vsn, ".") of
        [_,_,_] -> true; % likely a tagged commit
        _ -> false
    end.
-endif.

license_key_from_environment() ->
    os:getenv("MAXMIND_LICENSE_KEY").

test_cases(Suffix) ->
    exported_functions_with_suffixes(["_test", Suffix]).

exported_functions_with_suffixes(Suffixes) ->
    [Name || {Name, 1} <- exported_functions(),
             lists:any(fun (Suffix) -> lists:suffix(Suffix, atom_to_list(Name)) end,
                       Suffixes)].

exported_functions() ->
    ModuleInfo = ?MODULE:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    Exports.

%%%%%%%%%%%%%%%
init_per_group(GroupName, Config) ->
    case atom_to_list(GroupName) of
        "filesystem_tests_" ++ FileExtension ->
            {ok, _} = application:ensure_all_started(locus),
            ok = locus_logger:set_loglevel(debug),
            BaseURL = ?PATH_WITH_TEST_TARBALLS,
            DatabasePath = filename:join(?PATH_WITH_TEST_TARBALLS, "GeoLite2-Country." ++ FileExtension),
            CorruptPath = filename:join(?PATH_WITH_TEST_TARBALLS, "corruption." ++ FileExtension),
            [{is_http, false},
             {url_or_edition, DatabasePath},
             {path, DatabasePath},
             {corrupt_url, CorruptPath},
             {base_url, BaseURL}
             | Config];
        "local_http_tests_" ++ FileExtension ->
            {ok, _} = application:ensure_all_started(locus),
            ok = locus_logger:set_loglevel(debug),
            {ok, HttpdPid, BaseURL} = locus_httpd:start(?PATH_WITH_TEST_TARBALLS),
            DatabasePath = filename:join(?PATH_WITH_TEST_TARBALLS, "GeoLite2-Country." ++ FileExtension),
            RandomAnchor = integer_to_list(rand:uniform(1 bsl 64), 36),
            DatabaseURL = BaseURL ++ "/GeoLite2-Country." ++ FileExtension ++ "#" ++ RandomAnchor,
            CorruptURL = BaseURL ++ "/corruption." ++ FileExtension,
            ok = set_file_mtime(DatabasePath, ?VERSION1_TIMESTAMP),
            [{is_http, true},
             {is_remote, false},
             {httpd_pid, HttpdPid},
             {url_or_edition, DatabaseURL},
             {path, DatabasePath},
             {corrupt_url, CorruptURL},
             {base_url, BaseURL}
             | Config];
        "remote_http_tests" ->
            {ok, _} = application:ensure_all_started(locus),
            ok = locus_logger:set_loglevel(debug),
            ok = application:set_env(locus, license_key, license_key_from_environment()),
            % RandomAnchor = integer_to_list(rand:uniform(1 bsl 64), 36), % FIXME
            Edition = 'GeoLite2-Country',
            [{is_http, true},
             {is_remote, true},
             {url_or_edition, Edition}
             | Config]
    end.

end_per_group(GroupName, Config) ->
    case atom_to_list(GroupName) of
        "filesystem_tests_" ++ _FileExtension ->
            ok = application:stop(locus),
            Config;
        "local_http_tests_" ++ _FileExtension ->
            URL = proplists:get_value(url_or_edition, Config),
            CacheFilename = locus_loader:cached_database_path_for_url(URL),
            HttpdPid = proplists:get_value(httpd_pid, Config),

            ok = application:stop(locus),
            ok = locus_httpd:stop(HttpdPid),
            _ = file:delete(CacheFilename),
            Config;
        "remote_http_tests" ->
            Edition = proplists:get_value(url_or_edition, Config),
            Date = undefined,
            CacheFilename = locus_loader:cached_database_path_for_maxmind_edition(Edition, Date),

            ok = application:stop(locus),
            _ = file:delete(CacheFilename),
            Config
    end.

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

cacheless_loading_httptest(Config) ->
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = cacheless_loading_httptest,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URLOrEdition, LoaderOpts),
    LoadedVersion = test_successful_loader_await(Loader),
    % check events
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, LoadedVersion}}}),
    % check info
    ?assertMatch({ok, #{ metadata := #{}, source := {remote,_}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertMatch({ok, {remote,_}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).

cold_remote_loading_httptest(Config) ->
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = cold_regular_loading_httptest,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, URLOrEdition, LoaderOpts),
    LoadedVersion = test_successful_loader_await(Loader),
    % check events
    ?assertRecv({locus, Loader, {load_attempt_finished, {cache,_}, {error,_}}}),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, LoadedVersion}}}),
    ?assertRecv({locus, Loader, {cache_attempt_finished, _CacheFilename, ok}}),
    % check info
    ?assertMatch({ok, #{ metadata := #{}, source := {remote,_}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertMatch({ok, {remote,_}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).

warm_remote_loading_httptest(Config) ->
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = warm_regular_loading_httptest,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, URLOrEdition, LoaderOpts),
    LoadedVersion = test_successful_loader_await(Loader),
    CacheFilename = cached_database_path(URLOrEdition),
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

update_works_httptest(Config) ->
    IsRemote = proplists:get_value(is_remote, Config),
    update_works_httptest(IsRemote, Config).

update_works_httptest(IsRemote, _Config) when IsRemote ->
    {skip, "Unable to tweak modification time of remote files"};
update_works_httptest(_IsRemote, Config) ->
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Path = proplists:get_value(path, Config),
    Loader = update_works_httptest,
    UpdatePeriod = 200,
    LoaderOpts = [no_cache,
                  {update_period, UpdatePeriod},
                  {event_subscriber, self()}],
    %%
    ok = set_file_mtime(Path, ?VERSION1_TIMESTAMP),
    ok = locus:start_loader(Loader, URLOrEdition, LoaderOpts),
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
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = ipv4_country_lookup_test,
    ok = locus:start_loader(Loader, URLOrEdition),
    test_successful_loader_await(Loader),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV4_STR_ADDR),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).

ipv4_invalid_addr_test(Config) ->
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = ipv4_invalid_addr_test,
    ok = locus:start_loader(Loader, URLOrEdition),
    test_successful_loader_await(Loader),
    ?assertEqual({error, invalid_address}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).

ipv6_country_lookup_test(Config) ->
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = ipv6_country_lookup_test,
    ok = locus:start_loader(Loader, URLOrEdition),
    test_successful_loader_await(Loader),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV6_STR_ADDR),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).

ipv6_invalid_addr_test(Config) ->
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = ipv6_invalid_addr_test,
    ok = locus:start_loader(Loader, URLOrEdition),
    test_successful_loader_await(Loader),
    ?assertEqual({error, invalid_address}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).

connect_timeout_httptest(Config) ->
    % Undeterministic test case
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = connect_timeout_httptest,
    LoaderOpts = [no_cache, {connect_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, URLOrEdition, LoaderOpts),
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

download_start_timeout_httptest(Config) ->
    %% Undeterministic test case
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = download_start_timeout_httptest,
    LoaderOpts = [no_cache, {download_start_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, URLOrEdition, LoaderOpts),
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
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = idle_download_timeout_httptest,
    LoaderOpts = [no_cache, {idle_download_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, URLOrEdition, LoaderOpts),
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

wrong_url_fstest(Config) ->
    BaseURL = proplists:get_value(base_url, Config),
    URL = filename:join(BaseURL, "foobarbarfoofoobar"),
    Loader = wrong_url_fstest,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {load_attempt_started, {filesystem,_}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {filesystem,_}, {error,_}}}),
    ok = locus:stop_loader(Loader).

wrong_url_localhttptest(Config) ->
    BaseURL = proplists:get_value(base_url, Config),
    URL = BaseURL ++ "/foobarbarfoofoobar",
    Loader = wrong_url_httptest,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_failed_to_start,
                                 {http, {404 = _StatusCode, _StatusDesc}, _Headers, _Body}}}),
    ok = locus:stop_loader(Loader).

corrupt_database_fstest(Config) ->
    CorruptURL = proplists:get_value(corrupt_url, Config),
    Loader = corrupt_database_fstest,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, CorruptURL, LoaderOpts),
    ?assertRecv({locus, Loader, {load_attempt_started, {filesystem,_}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {filesystem,_}, {error, _}}}),
    ok = locus:stop_loader(Loader).

corrupt_database_localhttptest(Config) ->
    CorruptURL = proplists:get_value(corrupt_url, Config),
    Loader = corrupt_database_httptest,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, CorruptURL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, CorruptURL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {error, _}}}),
    ok = locus:stop_loader(Loader).

database_unknown_test(_Config) ->
    Loader = database_unknown_test,
    ?assertEqual({error, database_unknown}, locus:lookup(Loader, "127.0.0.1")),
    ?assertEqual({error, database_unknown}, locus:get_version(Loader)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader, metadata)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader, source)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader, version)).

database_still_loading_localhttptest(Config) ->
    BaseURL = proplists:get_value(base_url, Config),
    URL = BaseURL ++ "/foobarbarfoofoobar",
    Loader = database_still_loading_httptest,
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
    _ = process_flag(trap_exit, true),
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    NotAnURLNorAnEdition = [{}],
    Loader = invalid_args_test,
    NotAnOpt = not_an_opt,
    InvalidOpts = [NotAnOpt],

    % regular loaders
    ?assertEqual({error, invalid_url}, locus:start_loader(Loader, NotAnURLNorAnEdition)),
    ?assertEqual({error, {invalid_opt, NotAnOpt}}, locus:start_loader(Loader, URLOrEdition, InvalidOpts)),

    % child spec'd loaders
    ?assertMatch({error, {invalid_url,_}},
                 custom_loader_sup:start_link(Loader, NotAnURLNorAnEdition)),
    ?assertMatch({error, {shutdown, {failed_to_start_child,_,{invalid_opt,NotAnOpt}}}},
                 custom_loader_sup:start_link(Loader, URLOrEdition, InvalidOpts)).

subscriber_death_test(Config) ->
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = subscriber_death_test,
    Subscribers = [spawn(fun () -> timer:sleep(X*100) end) || X <- [2,3,4,5]],
    LoaderOpts = [{event_subscriber, Pid} || Pid <- Subscribers],
    ok = locus:start_loader(Loader, URLOrEdition, LoaderOpts),
    OriginalPid = locus_database:whereis(Loader),
    ?assertEqual(lists:sort([locus_logger | Subscribers]),
                 lists:sort(locus_database:list_subscribers(Loader))),
    timer:sleep(750),
    ?assertEqual([locus_logger], locus_database:list_subscribers(Loader)),
    ?assertEqual(OriginalPid, locus_database:whereis(Loader)),
    ok = locus:stop_loader(Loader).

loader_child_spec_test(Config) ->
    _ = process_flag(trap_exit, true),
    URLOrEdition = proplists:get_value(url_or_edition, Config),
    Loader = loader_child_spec_test,

    {ok, Supervisor} = custom_loader_sup:start_link(Loader, URLOrEdition),
    test_successful_loader_await(Loader),

    % it conflicts with supervisor-spawned loaders under the same name
    ?assertMatch({error, {shutdown, {failed_to_start_child,_,{already_started,_}}}},
                 custom_loader_sup:start_link(Loader, URLOrEdition)),

    % it conflicts with regular loaders under the same name
    ?assertMatch({error, already_started}, locus:start_loader(Loader, URLOrEdition)),

    % addresses can be looked up like in regular loaders
    ?assertEqual({error, not_found}, locus:lookup(Loader, "127.0.0.1")),
    ?assertMatch({ok, #{}},          locus:lookup(Loader, ?IPV4_STR_ADDR)),
    ?assertMatch({ok, #{}},          locus:lookup(Loader, ?IPV6_STR_ADDR)),

    % if stopped like a regular loader,
    % its supervisor will decide whether to restart it
    % (and it will, in this case)
    ?assertMatch({ok,#{}}, locus:get_info(Loader)),
    ?assertEqual(ok, locus:stop_loader(Loader)),
    timer:sleep(500),
    ?assertMatch({ok,#{}}, locus:get_info(Loader)),

    % it can be permanently stopped by stopping its supervisor
    ?assertMatch({ok,#{}}, locus:get_info(Loader)),
    ok = custom_loader_sup:stop(Supervisor),
    timer:sleep(500),
    ?assertEqual({error,database_unknown}, locus:get_info(Loader)),

    ok.

await_loader_failures_test(_Config) ->
    Loader = await_loader_failures_test,

    ?assertEqual({error, database_unknown},
                 locus:await_loader(Loader)),

    ?assertMatch({error, {#{Loader := database_unknown}, PartialSuccesses}}
                   when map_size(PartialSuccesses) =:= 0,
                 locus:await_loaders([Loader], 500)),
    ok.

wait_for_loader_failures_test(_Config) ->
    Loader = wait_for_loader_failures_test,

    ?assertEqual({error, database_unknown},
                 locus:wait_for_loader(Loader)),

    ?assertEqual({error, timeout},
                 locus:wait_for_loader(Loader, 0)),
    ?assertEqual({error, database_unknown},
                 locus:wait_for_loader(Loader, 500)),

    ?assertMatch({error, timeout},
                 locus:wait_for_loaders([Loader], 0)),
    ?assertMatch({error, {Loader, database_unknown}},
                 locus:wait_for_loaders([Loader], 500)),
    ok.

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

address_forms(StrAddr) ->
    BinAddr = list_to_binary(StrAddr),
    {ok, Addr} = inet:parse_address(StrAddr),
    {StrAddr, BinAddr, Addr}.

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

cached_database_path(Edition) when is_atom(Edition) ->
    {maxmind, ParsedEdition} = locus:parse_database_edition(Edition),
    locus_loader:cached_database_path_for_maxmind_edition(ParsedEdition, undefined);
cached_database_path(URL) ->
    locus_loader:cached_database_path_for_url(URL).
