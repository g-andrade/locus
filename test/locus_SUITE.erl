%% Copyright (c) 2017 Guilherme Andrade <locus.lib@gandrade.net>
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

-define(COUNTRY_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz").
%-define(CITY_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz").
%-define(ASN_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-ASN.tar.gz").

-define(IPV4_STR_ADDR, "93.184.216.34"). % example.com
-define(IPV6_STR_ADDR, "2606:2800:220:1:248:1893:25c8:1946"). % example.com

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [{individual_tests, [parallel], all_individual_tests()}].

all_individual_tests() ->
    [Name || {Name, 1} <- exported_functions(),
             lists:suffix("_test", atom_to_list(Name))].


init_per_group(individual_tests, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    ok = locus:start(tests, ?COUNTRY_URL),
    {ok, _LoadedVersion} = locus:wait_until_ready(tests, timer:seconds(60)),
    Config.

end_per_group(individual_tests, Config) ->
    ok = application:stop(locus),
    Config.

%%%

ipv4_country_lookup_test(_Config) ->
    country_lookup_test_(?IPV4_STR_ADDR).

ipv4_country_localized_lookup_test(_Config) ->
    country_localized_lookup_test_(?IPV4_STR_ADDR).

ipv4_invalid_addr_test(_Config) ->
    invalid_addr_test_("256.0.1.2").

%%%

ipv6_country_lookup_test(_Config) ->
    country_lookup_test_(?IPV6_STR_ADDR).

ipv6_country_localized_lookup_test(_Config) ->
    country_localized_lookup_test_(?IPV6_STR_ADDR).

ipv6_invalid_addr_test(_Config) ->
    invalid_addr_test_("22606:2800:220:1:248:1893:25c8:1946").

%%%

country_lookup_test_(StrAddr) ->
    {StrAddr, BinAddr, Addr} = address_forms(StrAddr),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(tests, StrAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(tests, BinAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(tests, Addr)).

country_localized_lookup_test_(StrAddr) ->
    {StrAddr, BinAddr, Addr} = address_forms(StrAddr),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(tests, StrAddr, <<"fr">>)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(tests, BinAddr, <<"fr">>)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(tests, Addr, <<"fr">>)).

invalid_addr_test_(StrAddr) ->
    ?assertMatch({error, invalid_address}, locus:lookup(tests, StrAddr)).

address_forms(StrAddr) ->
    BinAddr = list_to_binary(StrAddr),
    {ok, Addr} = inet:parse_address(StrAddr),
    {StrAddr, BinAddr, Addr}.

exported_functions() ->
    ModuleInfo = ?MODULE:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    Exports.
