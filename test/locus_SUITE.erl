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

%-define(COUNTRY_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz").
%-define(CITY_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz").
%-define(ASN_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-ASN.tar.gz").

-define(IPV4_STR_ADDR, "93.184.216.34"). % example.com
-define(IPV6_STR_ADDR, "2606:2800:220:1:248:1893:25c8:1946"). % example.com

-define(assertRecv(Pattern, Timeout),
        ((fun () -> receive Msg -> ?assertMatch((Pattern), Msg)
                    after Timeout -> error(timeout) end end)())).

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [{individual_tests, [], all_individual_tests()}].

all_individual_tests() ->
    [Name || {Name, 1} <- exported_functions(),
             lists:suffix("_test", atom_to_list(Name))].


init_per_group(individual_tests, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    ok = locus_logger:set_loglevel(debug),
    {ok, HttpdPid, BaseURL} = locus_httpd:start(),
    RandomAnchor = integer_to_list(rand:uniform(1 bsl 64), 36),
    DatabaseURL = BaseURL ++ "/GeoLite2-Country.tar.gz" ++ "#" ++ RandomAnchor,
    [{httpd_pid, HttpdPid}, {url, DatabaseURL},
     {base_url, BaseURL} | Config].

end_per_group(individual_tests, Config) ->
    URL = proplists:get_value(url, Config),
    CacheFilename = locus_http_loader:cached_tarball_name_for_url(URL),
    HttpdPid = proplists:get_value(httpd_pid, Config),

    ok = application:stop(locus),
    ok = locus_httpd:stop(HttpdPid),
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
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_started, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}, 1000),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, LoadedVersion}}}, 1000),
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
    {ok, LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(5)),
    % check events
    ?assertRecv({locus, Loader, {load_attempt_finished, {cache,_}, {error,_}}}, 1000),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_started, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {ok, _TrailingHeaders}}}, 1000),
    ?assertRecv({locus, Loader, {load_attempt_finished, {remote,_}, {ok, LoadedVersion}}}, 1000),
    ?assertRecv({locus, Loader, {cache_attempt_finished, _CacheFilename, ok}}, 1000),
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
    {ok, LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(5)),
    CacheFilename = locus_http_loader:cached_tarball_name_for_url(URL),
    % check events
    ?assertRecv({locus, Loader, {load_attempt_finished, {cache,_}, {ok,LoadedVersion}}}, 1000),
    ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_dismissed, {http, {304,_}, _Headers, _Body}}}, 1000),
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
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(5)),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV4_STR_ADDR),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).

ipv4_invalid_addr_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv4_invalid_addr_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(5)),
    ?assertEqual({error, invalid_address}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).

ipv6_country_lookup_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv6_country_lookup_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(5)),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV6_STR_ADDR),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).

ipv6_invalid_addr_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = ipv6_invalid_addr_test,
    ok = locus:start_loader(Loader, URL),
    {ok, _LoadedVersion} = locus:wait_for_loader(Loader, timer:seconds(5)),
    ?assertEqual({error, invalid_address}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).

connect_timeout_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = connect_timeout_test,
    LoaderOpts = [no_cache, {connect_timeout, 0}, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_failed_to_start, {error, {failed_connect, _}}}}, 1000),
    ok = locus:stop_loader(Loader).

download_start_timeout_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = download_start_timeout_test,
    LoaderOpts = [no_cache, {download_start_timeout, 0}, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_failed_to_start, timeout}}, 1000),
    ok = locus:stop_loader(Loader).

idle_download_timeout_test(Config) ->
    URL = proplists:get_value(url, Config),
    Loader = idle_download_timeout_test,
    LoaderOpts = [no_cache, {idle_download_timeout, 0}, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_started, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_finished, _BytesReceived, {error, timeout}}}, 1000),
    ok = locus:stop_loader(Loader).

wrong_url_test(Config) ->
    BaseURL = proplists:get_value(base_url, Config),
    URL = BaseURL ++ "/foobarbarfoofoobar",
    Loader = wrong_url_test,
    LoaderOpts = [no_cache, {event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_failed_to_start,
                                 {http, {404 = _StatusCode, _StatusDesc}, _Headers, _Body}}},
                1000),
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
    ?assertRecv({locus, Loader, {request_sent, _URL, _Headers}}, 1000),
    ?assertRecv({locus, Loader, {download_failed_to_start,
                                 {http, {404 = _StatusCode, _StatusDesc}, _Headers, _Body}}},
                1000),
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


%%%

address_forms(StrAddr) ->
    BinAddr = list_to_binary(StrAddr),
    {ok, Addr} = inet:parse_address(StrAddr),
    {StrAddr, BinAddr, Addr}.

exported_functions() ->
    ModuleInfo = ?MODULE:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    Exports.
