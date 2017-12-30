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

-module(locus).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1]).                              -ignore_xref({start,1}).
-export([start/2]).                              -ignore_xref({start,2}).
-export([stop/1]).                               -ignore_xref({stop,1}).
-export([wait_until_ready/1]).                   -ignore_xref({wait_until_ready,1}).
-export([wait_until_ready/2]).                   -ignore_xref({wait_until_ready,2}).
-export([supported_languages/1]).                -ignore_xref({supported_languages,1}).
-export([lookup/2]).                             -ignore_xref({lookup,2}).
-export([lookup/3]).                             -ignore_xref({lookup,3}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(DEFAULT_DATABASE_URL,
        "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz").

-define(DEFAULT_WAIT_TIMEOUT, infinity).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start(DatabaseId) -> ok | {error, Error}
            when DatabaseId :: atom(),
                 Error :: already_started.
start(DatabaseId) ->
    start(DatabaseId, ?DEFAULT_DATABASE_URL).

-spec start(DatabaseId, DatabaseURL) -> ok | {error, Error}
            when DatabaseId :: atom(),
                 DatabaseURL :: nonempty_string(),
                 Error :: already_started.
start(DatabaseId, DatabaseURL) ->
    locus_sup:start_child(DatabaseId, DatabaseURL).

-spec stop(DatabaseId) -> ok | {error, Error}
            when DatabaseId :: atom(),
                 Error :: not_found.
stop(DatabaseId) ->
    locus_sup:stop_child(DatabaseId).

-spec wait_until_ready(DatabaseId) -> {ok, LoadedVersion} | {error, Error}
            when DatabaseId :: atom(),
                 LoadedVersion :: calendar:datetime(),
                 Error :: database_unknown | timeout | {loading, LoadingError},
                 LoadingError :: term().
wait_until_ready(DatabaseId) ->
    wait_until_ready(DatabaseId, ?DEFAULT_WAIT_TIMEOUT).

-spec wait_until_ready(DatabaseId, Timeout) -> {ok, LoadedVersion} | {error, Error}
            when DatabaseId :: atom(),
                 Timeout :: timeout(),
                 LoadedVersion :: calendar:datetime(),
                 Error :: database_unknown | timeout | {loading, LoadingError},
                 LoadingError :: term().
wait_until_ready(DatabaseId, Timeout) ->
    locus_http_loader:wait_until_database_is_loaded(DatabaseId, Timeout).

-spec supported_languages(DatabaseId) -> {ok, Languages} | {error, Error}
            when DatabaseId :: atom(),
                 Languages :: [binary()],
                 Error :: not_applicable | database_unknown | database_not_loaded.
supported_languages(DatabaseId) ->
    case locus_mmdb:get_metadata(DatabaseId) of
        {ok, #{ <<"languages">> := Languages }} ->
            {ok, lists:usort(Languages)};
        {ok, #{}} ->
            {error, not_applicable};
        {error, Error} ->
            {error, Error}
    end.

-spec lookup(DatabaseId, Address) -> {ok, Entry} | {error, Error}
            when DatabaseId :: atom(),
                 Address :: inet:ip_address() | nonempty_string() | binary(),
                 Entry :: #{ binary() => term() | Entry },
                 Error :: (not_found | invalid_address |
                           database_unknown | database_not_loaded |
                           ipv4_database).
lookup(DatabaseId, Address) ->
    lookup(DatabaseId, Address, <<"en">>).

-spec lookup(DatabaseId, Address, Language) -> {ok, Entry} | {error, Error}
            when DatabaseId :: atom(),
                 Address :: inet:ip_address() | nonempty_string() | binary(),
                 Language :: binary(),
                 Entry :: #{ binary() => term() | Entry },
                 Error :: (not_found | invalid_address |
                           database_unknown | database_not_loaded |
                           ipv4_database).
lookup(DatabaseId, Address, Language) ->
    case locus_mmdb:lookup(DatabaseId, Address) of
        {ok, Entry} ->
            {ok, localize_entry(Entry, Language)};
        {error, Error} ->
            {error, Error}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

localize_entry(Entry, Language) when is_map(Entry) ->
    case maps:take(<<"names">>, Entry) of
        {#{ Language := Name }, Entry2} ->
            Entry2#{ <<"name">> => Name };
        {_MissingLanguage, Entry2} ->
            Entry2;
        error ->
            maps:map(
              fun (_Key, ChildEntry) ->
                      localize_entry(ChildEntry, Language)
              end,
              Entry)
    end;
localize_entry(Entry, Language) when is_list(Entry) ->
    [localize_entry(ChildEntry, Language) || ChildEntry <- Entry];
localize_entry(Entry, _Language) ->
    Entry.
