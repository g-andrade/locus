%% Copyright (c) 2017-2018 Guilherme Andrade
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

-define(REMOTE_COUNTRY_BASE_URL, "https://geolite.maxmind.com").
-define(REMOTE_COUNTRY_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz").
-define(REMOTE_COUNTRY_CORRUPT_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country-CSV.zip").

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
    case should_run_remote_http_tests() of
        false ->
            [{filesystem_tests, [], test_cases("_fstest")},
             {local_http_tests, [], test_cases("_httptest")}
            ];
        true ->
            [{filesystem_tests, [], test_cases("_fstest")},
             {local_http_tests, [], test_cases("_httptest")},
             {remote_http_tests, [], test_cases("_httptest")}
            ]
    end.

-ifdef(RUNNING_ON_TRAVIS).
should_run_remote_http_tests() ->
    false.
-else.
should_run_remote_http_tests() ->
    {ok, _} = application:ensure_all_started(locus),
    Apps = application:which_applications(),
    {locus, _Descr, Vsn} = lists:keyfind(locus, 1, Apps),
    case string:tokens(Vsn, ".") of
        [_,_,_] -> true; % likely a tagged commit
        _ -> false
    end.
-endif.

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
init_per_group(filesystem_tests, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    ok = locus_logger:set_loglevel(debug),
    BaseURL = ?PATH_WITH_TEST_TARBALLS,
    DatabasePath = filename:join(?PATH_WITH_TEST_TARBALLS, "GeoLite2-Country.tar.gz"),
    CorruptPath = filename:join(?PATH_WITH_TEST_TARBALLS, "corruption.tar.gz"),
    [{is_http, false},
     {url, DatabasePath},
     {path, DatabasePath},
     {corrupt_url, CorruptPath},
     {base_url, BaseURL}
     | Config];
init_per_group(local_http_tests, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    ok = locus_logger:set_loglevel(debug),
    {ok, HttpdPid, BaseURL} = locus_httpd:start(?PATH_WITH_TEST_TARBALLS),
    locus_rand_compat:seed(),
    DatabasePath = filename:join(?PATH_WITH_TEST_TARBALLS, "GeoLite2-Country.tar.gz"),
    RandomAnchor = integer_to_list(locus_rand_compat:uniform(1 bsl 64), 36),
    DatabaseURL = BaseURL ++ "/GeoLite2-Country.tar.gz" ++ "#" ++ RandomAnchor,
    CorruptURL = BaseURL ++ "/corruption.tar.gz",
    ok = set_file_mtime(DatabasePath, ?VERSION1_TIMESTAMP),
    [{is_http, true},
     {is_remote, false},
     {httpd_pid, HttpdPid},
     {url, DatabaseURL},
     {path, DatabasePath},
     {corrupt_url, CorruptURL},
     {base_url, BaseURL}
     | Config];
init_per_group(remote_http_tests, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    ok = locus_logger:set_loglevel(debug),
    locus_rand_compat:seed(),
    RandomAnchor = integer_to_list(locus_rand_compat:uniform(1 bsl 64), 36),
    URL = ?REMOTE_COUNTRY_URL ++ "#" ++ RandomAnchor,
    CorruptURL = ?REMOTE_COUNTRY_CORRUPT_URL,
    BaseURL = ?REMOTE_COUNTRY_BASE_URL,
    [{is_http, true},
     {is_remote, true},
     {url, URL},
     {base_url, BaseURL},
     {corrupt_url, CorruptURL}
     | Config].

end_per_group(filesystem_tests, Config) ->
    ok = application:stop(locus),
    Config;
end_per_group(local_http_tests, Config) ->
    URL = proplists:get_value(url, Config),
    CacheFilename = locus_http_loader:cached_tarball_name_for_url(URL),
    HttpdPid = proplists:get_value(httpd_pid, Config),

    ok = application:stop(locus),
    ok = locus_httpd:stop(HttpdPid),
    _ = file:delete(CacheFilename),
    Config;
end_per_group(remote_http_tests, Config) ->
    URL = proplists:get_value(url, Config),
    CacheFilename = locus_http_loader:cached_tarball_name_for_url(URL),

    ok = application:stop(locus),
    _ = file:delete(CacheFilename),
    Config.

%% ------------------------------------------------------------------
%% Filesystem Test Cases
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

-ifdef(BAD_HTTPC).
cacheless_loading_httptest(_Config) ->
    {skip, "The httpc version bundled with this OTP release has issues with URL fragments"}.
-else.
cacheless_loading_httptest(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = cacheless_loading_httptest,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    {ok, LoadedVersion} = locus:wait_for_loader(Loader),
    % check events
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, LoadedVersion}}}),
    % check info
    ?assertMatch({ok, #{ metadata := #{}, source := {remote,URL}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertEqual({ok, {remote,URL}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).
-endif.


-ifdef(BAD_HTTPC).
cold_remote_loading_httptest(_Config) ->
    {skip, "The httpc version bundled with this OTP release has issues with URL fragments"}.
-else.
cold_remote_loading_httptest(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = cold_regular_loading_httptest,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    {ok, LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    % check events
    ?assertRecv({locus, Loader, {load_attempt_finished, {cache,_}, {error,_}}}),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, LoadedVersion}}}),
    ?assertRecv({locus, Loader, {cache_attempt_finished, _CacheFilename, ok}}),
    % check info
    ?assertMatch({ok, #{ metadata := #{}, source := {remote,URL}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertEqual({ok, {remote,URL}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).
-endif.

-ifdef(BAD_HTTPC).
warm_remote_loading_httptest(_Config) ->
    {skip, "The httpc version bundled with this OTP release has issues with URL fragments"}.
-else.
warm_remote_loading_httptest(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = warm_regular_loading_httptest,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    {ok, LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    CacheFilename = locus_http_loader:cached_tarball_name_for_url(URL),
    % check events
    ?assertRecv({locus, Loader, {load_attempt_finished, {cache,_}, {ok,LoadedVersion}}}),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_dismissed, {http, {304,_}, _Headers, _Body}}}),
    % check info
    ?assertMatch({ok, #{ metadata := #{}, source := {cache,CacheFilename}, version := LoadedVersion }},
                 locus:get_info(Loader)),
    ?assertMatch({ok, #{}}, locus:get_info(Loader, metadata)),
    ?assertEqual({ok, {cache,CacheFilename}}, locus:get_info(Loader, source)),
    ?assertEqual({ok, LoadedVersion}, locus:get_info(Loader, version)),
    ok = locus:stop_loader(Loader).
-endif.

-ifdef(BAD_HTTPC).
update_works_httptest(_Config) ->
    {skip, "The httpc version bundled with this OTP release has issues with URL fragments"}.
-else.
update_works_httptest(Config) ->
    IsRemote = proplists:get_value(is_remote, Config),
    update_works_httptest(IsRemote, Config).

update_works_httptest(IsRemote, _Config) when IsRemote ->
    {skip, "Unable to tweak modification time of remote files"};
update_works_httptest(_IsRemote, Config) ->
    URL = proplists:get_value(url, Config),
    Path = proplists:get_value(path, Config),
    Loader = update_works_httptest,
    PostReadinessUpdatePeriod = 100,
    LoaderOpts = [no_cache, {post_readiness_update_period, PostReadinessUpdatePeriod},
                  {event_subscriber, self()}],
    %%
    ok = set_file_mtime(Path, ?VERSION1_TIMESTAMP),
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, _}}}),
    %%
    ok = set_file_mtime(Path, ?VERSION2_TIMESTAMP),
    {TimeElapsedA, _} = timer:tc(fun () -> ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}) end),
    MillisecondsElapsedA = TimeElapsedA / 1000,
    ct:pal("MillsecondsElapsed: ~p", [MillisecondsElapsedA]),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, _}}}),
    ?assert(MillisecondsElapsedA / PostReadinessUpdatePeriod >= 0.90),
    %%
    {TimeElapsedB, _} = timer:tc(fun () -> ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}) end),
    MillisecondsElapsedB = TimeElapsedB / 1000,
    ct:pal("MillsecondsElapsed: ~p", [MillisecondsElapsedB]),
    ?assertRecv({locus, Loader, {download_dismissed, {http, {304,_}, _Headers, _Body}}}),
    ?assert(MillisecondsElapsedB / PostReadinessUpdatePeriod >= 0.90),
    %%
    ok = locus:stop_loader(Loader).
-endif.

-ifdef(BAD_HTTPC).
ipv4_country_lookup_test(_Config) ->
    {skip, "The httpc version bundled with this OTP release has issues with URL fragments"}.
-else.
ipv4_country_lookup_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv4_country_lookup_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV4_STR_ADDR),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).
-endif.

-ifdef(BAD_HTTPC).
ipv4_invalid_addr_test(_Config) ->
    {skip, "The httpc version bundled with this OTP release has issues with URL fragments"}.
-else.
ipv4_invalid_addr_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv4_invalid_addr_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    ?assertEqual({error, invalid_address}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).
-endif.

-ifdef(BAD_HTTPC).
ipv6_country_lookup_test(_Config) ->
    {skip, "The httpc version bundled with this OTP release has issues with URL fragments"}.
-else.
ipv6_country_lookup_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv6_country_lookup_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV6_STR_ADDR),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ prefix := _, <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).
-endif.

-ifdef(BAD_HTTPC).
ipv6_invalid_addr_test(_Config) ->
    {skip, "The httpc version bundled with this OTP release has issues with URL fragments"}.
-else.
ipv6_invalid_addr_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv6_invalid_addr_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    ?assertEqual({error, invalid_address}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).
-endif.

-ifndef(POST_OTP_17).
connect_timeout_httptest(_Config) ->
    {skip, "Not working properly on OTP 17"}.
-else.
connect_timeout_httptest(Config) ->
    % Undeterministic test case
    URL = proplists:get_value(url, Config),
    Loader = connect_timeout_httptest,
    LoaderOpts = [no_cache, {connect_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, URL, LoaderOpts),
             ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
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
-endif.

download_start_timeout_httptest(Config) ->
    %% Undeterministic test case
    URL = proplists:get_value(url, Config),
    Loader = download_start_timeout_httptest,
    LoaderOpts = [no_cache, {download_start_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, URL, LoaderOpts),
             ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
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

-ifdef(BAD_HTTPC).
idle_download_timeout_httptest(_Config) ->
    {skip, "The httpc version bundled with this OTP release has issues with URL fragments"}.
-else.
idle_download_timeout_httptest(Config) ->
    % Undeterministic test case
    URL = proplists:get_value(url, Config),
    Loader = idle_download_timeout_httptest,
    LoaderOpts = [no_cache, {idle_download_timeout, 0}, {event_subscriber, self()}],
    MaxAttempts = max_undeterministic_attempts(Config),
    (fun F(AttemptsLeft) ->
             ok = locus:start_loader(Loader, URL, LoaderOpts),
             ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
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
-endif.

wrong_url_fstest(Config) ->
    BaseURL = proplists:get_value(base_url, Config),
    URL = filename:join(BaseURL, "foobarbarfoofoobar"),
    Loader = wrong_url_fstest,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {load_attempt_started, {filesystem,_}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {filesystem,_}, {error,_}}}),
    ok = locus:stop_loader(Loader).

wrong_url_httptest(Config) ->
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

corrupt_database_httptest(Config) ->
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

database_still_loading_httptest(Config) ->
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
    URL = proplists:get_value(url, Config),
    NotAnURL = not_an_url,
    Loader = invalid_args_test,
    ?assertEqual({error, invalid_url}, locus:start_loader(Loader, NotAnURL)),
    InvalidOpts = [not_an_opt],
    ?assertEqual({error, {invalid_opt, not_an_opt}}, locus:start_loader(Loader, URL, InvalidOpts)).

subscriber_death_test(Config) ->
    LoaderModule =
        case proplists:get_value(is_http, Config) of
            false -> locus_filesystem_loader;
            true -> locus_http_loader
        end,
    URL = proplists:get_value(url, Config),
    Loader = subscriber_death_test,
    Subscribers = [spawn(fun () -> timer:sleep(X*100) end) || X <- [2,3,4,5]],
    LoaderOpts = [{event_subscriber, Pid} || Pid <- Subscribers],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    OriginalPid = LoaderModule:whereis(Loader),
    ?assertEqual(lists:sort([locus_logger | Subscribers]),
                 lists:sort(LoaderModule:list_subscribers(Loader))),
    timer:sleep(750),
    ?assertEqual([locus_logger], LoaderModule:list_subscribers(Loader)),
    ?assertEqual(OriginalPid, LoaderModule:whereis(Loader)),
    ok = locus:stop_loader(Loader).

%%%

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
