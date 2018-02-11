%% Copyright (c) 2017-2018 Guilherme Andrade <locus.lib@gandrade.net>
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

-define(REMOTE_COUNTRY_BASE_URL, "https://geolite.maxmind.com").
-define(REMOTE_COUNTRY_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz").
-define(REMOTE_COUNTRY_CORRUPT_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country-CSV.zip").
%-define(CITY_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz").
%-define(ASN_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-ASN.tar.gz").

-define(IPV4_STR_ADDR, "93.184.216.34"). % example.com
-define(IPV6_STR_ADDR, "2606:2800:220:1:248:1893:25c8:1946"). % example.com

-define(assertRecv(Pattern),
        ((fun () -> receive Msg -> ?assertMatch((Pattern), Msg)
                    after 30000 -> error(timeout) end end)())).

%%%%%%%%%%%%%%%
all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    case should_run_remote_tests() of
        false ->
            [{local_tests, [], all_test_cases()}];
        true ->
            [{local_tests, [], all_test_cases()},
             {remote_tests, [], all_test_cases()}]
    end.

-ifdef(RUNNING_ON_TRAVIS).
should_run_remote_tests() ->
    false.
-else.
should_run_remote_tests() ->
    {ok, _} = application:ensure_all_started(locus),
    Apps = application:which_applications(),
    {locus, _Descr, Vsn} = lists:keyfind(locus, 1, Apps),
    case string:tokens(Vsn, ".") of
        [_,_,_] -> true; % likely a tagged commit
        _ -> false
    end.
-endif.

all_test_cases() ->
    [Name || {Name, 1} <- exported_functions(),
             lists:suffix("_test", atom_to_list(Name))].

exported_functions() ->
    ModuleInfo = ?MODULE:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    Exports.

%%%%%%%%%%%%%%%
init_per_group(local_tests, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    ok = locus_logger:set_loglevel(debug),
    {ok, HttpdPid, BaseURL} = locus_httpd:start(),
    RandomAnchor = integer_to_list(rand:uniform(1 bsl 64), 36),
    DatabaseURL = BaseURL ++ "/GeoLite2-Country.tar.gz" ++ "#" ++ RandomAnchor,
    CorruptURL = BaseURL ++ "/corruption.tar.gz" ++ "#" ++ RandomAnchor,
    [{httpd_pid, HttpdPid},
     {url, DatabaseURL},
     {corrupt_url, CorruptURL},
     {base_url, BaseURL}
     | Config];
init_per_group(remote_tests, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    ok = locus_logger:set_loglevel(debug),
    RandomAnchor = integer_to_list(rand:uniform(1 bsl 64), 36),
    URL = ?REMOTE_COUNTRY_URL ++ "#" ++ RandomAnchor,
    CorruptURL = ?REMOTE_COUNTRY_CORRUPT_URL ++ "#" ++ RandomAnchor,
    BaseURL = ?REMOTE_COUNTRY_BASE_URL,
    [{url, URL},
     {base_url, BaseURL},
     {corrupt_url, CorruptURL}
     | Config].

end_per_group(local_tests, Config) ->
    URL = proplists:get_value(url, Config),
    CacheFilename = locus_http_loader:cached_tarball_name_for_url(URL),
    HttpdPid = proplists:get_value(httpd_pid, Config),

    ok = application:stop(locus),
    ok = locus_httpd:stop(HttpdPid),
    _ = file:delete(CacheFilename),
    Config;
end_per_group(remote_tests, Config) ->
    URL = proplists:get_value(url, Config),
    CacheFilename = locus_http_loader:cached_tarball_name_for_url(URL),

    ok = application:stop(locus),
    _ = file:delete(CacheFilename),
    Config.

%%%

cacheless_loading_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = cacheless_loading_test,
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

cold_remote_loading_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = cold_regular_loading_test,
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

warm_remote_loading_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = warm_regular_loading_test,
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

ipv4_country_lookup_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv4_country_lookup_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV4_STR_ADDR),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).

ipv4_invalid_addr_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv4_invalid_addr_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    ?assertEqual({error, invalid_address}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).

ipv6_country_lookup_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv6_country_lookup_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV6_STR_ADDR),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).

ipv6_invalid_addr_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv6_invalid_addr_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(30)),
    ?assertEqual({error, invalid_address}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).

connect_timeout_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = connect_timeout_test,
    LoaderOpts = [no_cache, {connect_timeout, 0}, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_failed_to_start, {error, {failed_connect, _}}}}),
    ok = locus:stop_loader(Loader).

download_start_timeout_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = download_start_timeout_test,
    LoaderOpts = [no_cache, {download_start_timeout, 0}, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_failed_to_start, timeout}}),
    ok = locus:stop_loader(Loader).

idle_download_timeout_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = idle_download_timeout_test,
    LoaderOpts = [no_cache, {idle_download_timeout, 0}, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_started, _Headers}}),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {error, timeout}}}),
    ok = locus:stop_loader(Loader).

wrong_url_test(Config) ->
    BaseURL = proplists:get_value(base_url, Config),
    URL = BaseURL ++ "/foobarbarfoofoobar",
    Loader = wrong_url_test,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
    ?assertRecv({locus, Loader, {download_failed_to_start,
                                 {http, {404 = _StatusCode, _StatusDesc}, _Headers, _Body}}}),
    ok = locus:stop_loader(Loader).

corrupt_database_test(Config) ->
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
    Loader = database_unknown_test,
    ?assertEqual({error, database_unknown}, locus:lookup(Loader, "127.0.0.1")),
    ?assertEqual({error, database_unknown}, locus:get_version(Loader)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader, metadata)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader, source)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader, version)).

database_still_loading_test(Config) ->
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
    URL = proplists:get_value(url, Config),
    NotAnURL = "not an url",
    Loader = invalid_args_test,
    ?assertEqual({error, invalid_url}, locus:start_loader(Loader, NotAnURL)),
    InvalidOpts = [not_an_opt],
    ?assertEqual({error, {invalid_opt, not_an_opt}}, locus:start_loader(Loader, URL, InvalidOpts)).

subscriber_death_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = warm_regular_loading_test,
    Subscribers = [spawn(fun () -> timer:sleep(X*100) end) || X <- [2,3,4,5]],
    LoaderOpts = [{event_subscriber, Pid} || Pid <- Subscribers],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    OriginalPid = locus_http_loader:whereis(Loader),
    ?assertEqual(lists:sort([locus_logger | Subscribers]),
                 lists:sort(locus_http_loader:list_subscribers(Loader))),
    timer:sleep(750),
    ?assertEqual([locus_logger], locus_http_loader:list_subscribers(Loader)),
    ?assertEqual(OriginalPid, locus_http_loader:whereis(Loader)),
    ok = locus:stop_loader(Loader).

%%%

address_forms(StrAddr) ->
    BinAddr = list_to_binary(StrAddr),
    {ok, Addr} = inet:parse_address(StrAddr),
    {StrAddr, BinAddr, Addr}.

