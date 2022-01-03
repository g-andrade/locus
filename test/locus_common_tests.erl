%% Copyright (c) 2017-2022 Guilherme Andrade
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

-module(locus_common_tests).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([test_successful_loader_await/1]).

%% ------------------------------------------------------------------
%% Test Case Exports
%% ------------------------------------------------------------------

-export([ipv4_country_lookup_test/1,
         ipv4_invalid_addr_test/1,
         ipv6_country_lookup_test/1,
         ipv6_invalid_addr_test/1,
         database_unknown_test/0,
         invalid_args_test/1,
         subscriber_death_test/1,
         loader_child_spec_test/1,
         await_loader_failures_test/0]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(IPV4_STR_ADDR, "93.184.216.34"). % example.com
-define(IPV6_STR_ADDR, "2606:2800:220:1:248:1893:25c8:1946"). % example.com

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type config() :: [config_pair(), ...].
-export_type([config/0]).

-type config_pair() :: {load_from, load_from()}.
-export_type([config_pair/0]).

-type load_from()
    :: {maxmind, atom()}
    |  binary()
    |  string().
-export_type([load_from/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec test_successful_loader_await(atom()) -> ok.
test_successful_loader_await(Loader) ->
    {ok, LoadedVersion} = locus:await_loader(Loader),
    {ok, #{Loader := LoadedVersion}} = locus:await_loaders([Loader], 500),
    LoadedVersion.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec ipv4_country_lookup_test(config()) -> ok.
ipv4_country_lookup_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = ipv4_country_lookup_test,
    ok = locus:start_loader(Loader, LoadFrom),
    test_successful_loader_await(Loader),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV4_STR_ADDR),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).

-spec ipv4_invalid_addr_test(config()) -> ok.
ipv4_invalid_addr_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = ipv4_invalid_addr_test,
    ok = locus:start_loader(Loader, LoadFrom),
    test_successful_loader_await(Loader),
    ?assertEqual({error, {invalid_address, "256.0.1.2"}}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).

-spec ipv6_country_lookup_test(config()) -> ok.
ipv6_country_lookup_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = ipv6_country_lookup_test,
    ok = locus:start_loader(Loader, LoadFrom),
    test_successful_loader_await(Loader),
    {StrAddr, BinAddr, Addr} = address_forms(?IPV6_STR_ADDR),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, StrAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, BinAddr)),
    ?assertMatch({ok, #{ <<"country">> := _ }}, locus:lookup(Loader, Addr)),
    ok = locus:stop_loader(Loader).

-spec ipv6_invalid_addr_test(config()) -> ok.
ipv6_invalid_addr_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = ipv6_invalid_addr_test,
    ok = locus:start_loader(Loader, LoadFrom),
    test_successful_loader_await(Loader),
    ?assertEqual({error, {invalid_address, "256.0.1.2"}}, locus:lookup(Loader, "256.0.1.2")),
    ok = locus:stop_loader(Loader).

-spec database_unknown_test() -> ok.
database_unknown_test() ->
    Loader = database_unknown_test,
    ?assertEqual({error, database_unknown}, locus:lookup(Loader, "127.0.0.1")),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader, metadata)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader, source)),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader, version)).

-spec invalid_args_test(config()) -> ok.
invalid_args_test(Config) ->
    _ = process_flag(trap_exit, true),
    LoadFrom = proplists:get_value(load_from, Config),
    NotAnURLNorAnEdition = [{}],
    Loader = invalid_args_test,
    NotAnOpt = not_an_opt,
    InvalidOpts = [NotAnOpt],

    % regular loaders
    ?assertEqual({error, invalid_url}, locus:start_loader(Loader, NotAnURLNorAnEdition)),
    ?assertEqual({error, {invalid_opt, NotAnOpt}}, locus:start_loader(Loader, LoadFrom,
                                                                      InvalidOpts)),

    % child spec'd loaders
    ?assertMatch({error, {invalid_url, _}},
                 custom_loader_sup:start_link(Loader, NotAnURLNorAnEdition)),
    ?assertMatch({error, {shutdown, {failed_to_start_child, _, {invalid_opt, NotAnOpt}}}},
                 custom_loader_sup:start_link(Loader, LoadFrom, InvalidOpts)).

-spec subscriber_death_test(config()) -> ok.
subscriber_death_test(Config) ->
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = subscriber_death_test,
    Subscribers = [spawn(fun () -> timer:sleep(X * 100) end) || X <- [2, 3, 4, 5]],
    LoaderOpts = [{event_subscriber, Pid} || Pid <- Subscribers],
    ok = locus:start_loader(Loader, LoadFrom, LoaderOpts),
    OriginalPid = locus_database:whereis(Loader),
    ?assertEqual(lists:sort([locus_logger | Subscribers]),
                 lists:sort(locus_database:list_subscribers(Loader))),
    timer:sleep(750),
    ?assertEqual([locus_logger], locus_database:list_subscribers(Loader)),
    ?assertEqual(OriginalPid, locus_database:whereis(Loader)),
    ok = locus:stop_loader(Loader).

-spec loader_child_spec_test(config()) -> ok.
loader_child_spec_test(Config) ->
    _ = process_flag(trap_exit, true),
    LoadFrom = proplists:get_value(load_from, Config),
    Loader = loader_child_spec_test,

    {ok, Supervisor} = custom_loader_sup:start_link(Loader, LoadFrom),
    test_successful_loader_await(Loader),

    % it conflicts with supervisor-spawned loaders under the same name
    ?assertMatch({error, {shutdown, {failed_to_start_child, _, {already_started, _}}}},
                 custom_loader_sup:start_link(Loader, LoadFrom)),

    % it conflicts with regular loaders under the same name
    ?assertMatch({error, already_started}, locus:start_loader(Loader, LoadFrom)),

    % addresses can be looked up like in regular loaders
    ?assertEqual(not_found, locus:lookup(Loader, "127.0.0.1")),
    ?assertMatch({ok, #{}}, locus:lookup(Loader, ?IPV4_STR_ADDR)),
    ?assertMatch({ok, #{}}, locus:lookup(Loader, ?IPV6_STR_ADDR)),

    % if stopped like a regular loader,
    % its supervisor will decide whether to restart it
    % (and it will, in this case)
    ?assertMatch({ok, #{}}, locus:get_info(Loader)),
    ?assertEqual(ok, locus:stop_loader(Loader)),
    timer:sleep(500),
    ?assertMatch({ok, #{}}, locus:get_info(Loader)),

    % it can be permanently stopped by stopping its supervisor
    ?assertMatch({ok, #{}}, locus:get_info(Loader)),
    ok = custom_loader_sup:stop(Supervisor),
    timer:sleep(500),
    ?assertEqual({error, database_unknown}, locus:get_info(Loader)),

    ok.

-spec await_loader_failures_test() -> ok.
await_loader_failures_test() ->
    Loader = await_loader_failures_test,

    ?assertEqual({error, database_unknown},
                 locus:await_loader(Loader)),

    ?assertMatch({error, {#{Loader := database_unknown}, PartialSuccesses}}
                   when map_size(PartialSuccesses) =:= 0,
                 locus:await_loaders([Loader], 500)),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

address_forms(StrAddr) ->
    BinAddr = list_to_binary(StrAddr),
    {ok, Addr} = inet:parse_address(StrAddr),
    {StrAddr, BinAddr, Addr}.
