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

-module(locus_filesystem_SUITE).
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
    [{list_to_atom("for_" ++ FileExtension), _Opts = [], locus_test_utils:test_cases(?MODULE)}
     || FileExtension <- LocalExtensions].

%%%%%%%%%%%%%%%
init_per_group(GroupName, Config) ->
    PathWithTestTarballs = locus_test_utils:path_with_test_tarballs(),
    "for_" ++ FileExtension = atom_to_list(GroupName),
    {ok, _} = application:ensure_all_started(locus),
    ok = application:set_env(kernel, logger_level, debug),
    ok = locus_logger:set_loglevel(debug),
    BaseURL = PathWithTestTarballs,
    DatabasePath = filename:join(PathWithTestTarballs, "GeoLite2-Country." ++ FileExtension),
    CorruptPath = filename:join(PathWithTestTarballs, "corruption." ++ FileExtension),
    [{load_from, DatabasePath},
     {path, DatabasePath},
     {corrupt_url, CorruptPath},
     {base_url, BaseURL}
     | Config].

end_per_group(GroupName, Config) ->
    ?assertMatch("for_" ++ _FileExtension, atom_to_list(GroupName)),
    ok = application:stop(locus),
    Config.

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

wrong_url_test(Config) ->
    BaseURL = proplists:get_value(base_url, Config),
    URL = filename:join(BaseURL, "foobarbarfoofoobar"),
    Loader = wrong_url_test,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, URL, LoaderOpts),
    ?assertRecv({locus, Loader, {load_attempt_started, {filesystem,_}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {filesystem,_}, {error,_}}}),
    ok = locus:stop_loader(Loader).

corrupt_database_test(Config) ->
    CorruptURL = proplists:get_value(corrupt_url, Config),
    Loader = corrupt_database_test,
    LoaderOpts = [{event_subscriber, self()}],
    ok = locus:start_loader(Loader, CorruptURL, LoaderOpts),
    ?assertRecv({locus, Loader, {load_attempt_started, {filesystem,_}}}),
    ?assertRecv({locus, Loader, {load_attempt_finished, {filesystem,_}, {error, _}}}),
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
