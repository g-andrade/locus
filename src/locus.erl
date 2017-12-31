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

-export([start/2]).                              -ignore_xref({start,2}).
-export([stop/1]).                               -ignore_xref({stop,1}).
-export([wait_until_ready/1]).                   -ignore_xref({wait_until_ready,1}).
-export([wait_until_ready/2]).                   -ignore_xref({wait_until_ready,2}).
-export([loaded_version/1]).                     -ignore_xref({loaded_version,1}).
-export([supported_languages/1]).                -ignore_xref({supported_languages,1}).
-export([lookup/2]).                             -ignore_xref({lookup,2}).
-export([lookup/3]).                             -ignore_xref({lookup,3}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(DEFAULT_WAIT_TIMEOUT, infinity).
-define(DEFAULT_LANGUAGE, <<"en">>).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Starts a database loader under id `DatabaseId'
%%
%% `DatabaseId' must be an atom.
%% `DatabaseURL' must be a non-empty string containing a HTTP(S) URL.
%%
%% Returns `ok' in case of success, `{error, already_started}' otherwise.
%% @see wait_until_ready/1
%% @see wait_until_ready/2
-spec start(DatabaseId, DatabaseURL) -> ok | {error, Error}
            when DatabaseId :: atom(),
                 DatabaseURL :: nonempty_string(),
                 Error :: already_started.
start(DatabaseId, DatabaseURL) ->
    locus_sup:start_child(DatabaseId, DatabaseURL).

%% @doc Stops the database loader under id `DatabaseId'
%%
%% `DatabaseId' must be an atom and refer to a started database loader.
%%
%% Returns `ok' in case of success, `{error, not_found}' otherwise.
-spec stop(DatabaseId) -> ok | {error, Error}
            when DatabaseId :: atom(),
                 Error :: not_found.
stop(DatabaseId) ->
    locus_sup:stop_child(DatabaseId).

%% @doc Blocks caller execution until the database is ready to use
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%%
%% Returns:
%% - `{ok, LoadedVersion}' when the database is ready to use.
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, {loading, term()}}' if loading the database failed for some reason.
%%
%% @see wait_until_ready/2
%% @see start/2
-spec wait_until_ready(DatabaseId) -> {ok, LoadedVersion} | {error, Error}
            when DatabaseId :: atom(),
                 LoadedVersion :: calendar:datetime(),
                 Error :: database_unknown | {loading, LoadingError},
                 LoadingError :: term().
wait_until_ready(DatabaseId) ->
    wait_until_ready(DatabaseId, ?DEFAULT_WAIT_TIMEOUT).

%% @doc Like `wait_until_ready/1' but it can time-out
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%% - `Timeout' must be either a non-negative integer (milliseconds) or `infinity'.
%%
%% Returns:
%% - `{ok, LoadedVersion}' when the database is ready to use.
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, timeout}' if we've given up on waiting.
%% - `{error, {loading, term()}}' if loading the database failed for some reason.
%% @see wait_until_ready/1
%% @see start/2
-spec wait_until_ready(DatabaseId, Timeout) -> {ok, LoadedVersion} | {error, Error}
            when DatabaseId :: atom(),
                 Timeout :: timeout(),
                 LoadedVersion :: calendar:datetime(),
                 Error :: database_unknown | timeout | {loading, LoadingError},
                 LoadingError :: term().
wait_until_ready(DatabaseId, Timeout) ->
    locus_http_loader:wait_until_database_is_loaded(DatabaseId, Timeout).

%% @doc Returns the currently loaded database version
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%%
%% Returns:
%% - `{ok, LoadedVersion}' in case of success
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, database_not_loaded}' if the database hasn't yet been loaded.
-spec loaded_version(DatabaseId) -> {ok, LoadedVersion} | {error, Error}
            when DatabaseId :: atom(),
                 LoadedVersion :: calendar:datetime(),
                 Error :: database_unknown | database_not_loaded.
loaded_version(DatabaseId) ->
    locus_mmdb:get_version(DatabaseId).

%% @doc Returns the localization languages supported by the database
%%
%% `DatabaseId' must be an atom and refer to a started database loader.
%%
%% Returns:
%% - `{ok, Languages}', with `Languages' a list of binaries, in case of success.
%% - `{error, not_applicable}' if the database doesn't support localization.
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, database_not_loaded}' if the database hasn't yet been loaded.
%% @see lookup/3
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

%% @doc Looks-up info on IPv4 and IPv6 addresses
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%% - `Address' must be either an `inet:ip_address()' tuple, or a string/binary
%%    containing a valid representation of the address.
%%
%% Returns:
%% - `{ok, Entry}' in case of success, with `Entry' being a map containing
%%    the relevant data and with place names localized in English (when applicable.)
%% - `{error, not_found}' if no data was found for this `Address'.
%% - `{error, invalid_address}' if `Address' is not either a `inet:ip_address()'
%%    tuple or a valid textual representation of an IP address.
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, database_not_loaded}' if the database hasn't yet been loaded.
%% - `{error, ipv4_database}' if `Address' represents an IPv6 address and the database
%% only supports IPv4 addresses.
%% @see lookup/3
-spec lookup(DatabaseId, Address) -> {ok, Entry} | {error, Error}
            when DatabaseId :: atom(),
                 Address :: inet:ip_address() | nonempty_string() | binary(),
                 Entry :: #{ binary() => term() | Entry },
                 Error :: (not_found | invalid_address |
                           database_unknown | database_not_loaded |
                           ipv4_database).
lookup(DatabaseId, Address) ->
    case locus_mmdb:lookup(DatabaseId, Address) of
        {ok, Entry} ->
            localize_entry(Entry, ?DEFAULT_LANGUAGE, false);
        {error, Error} ->
            {error, Error}
    end.

%% @doc Looks-up localized info on IPv4 and IPv6 addresses
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%% - `Address' must be either an `inet:ip_address()' tuple, or a string/binary
%% - `Language' must be a non-empty binary containing a language code.
%%
%% Returns:
%% - `{ok, Entry}' in case of success, with `Entry' being a map containing
%%    the relevant data and with place names localized in `Language' (when applicable.)
%% - `{error, not_found}' if no data was found for this `Address'.
%% - `{error, invalid_address}' if `Address' is not either a `inet:ip_address()'
%%    tuple or a valid textual representation of an IP address.
%% - `{error, unsupported_language}' if the chosen `Language' isn't supported
%%    by this particular database.
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, database_not_loaded}' if the database hasn't yet been loaded.
%% - `{error, ipv4_database}' if `Address' represents an IPv6 address and the database
%%    only supports IPv4 addresses.
%% @see lookup/2
%% @see supported_languages/1
-spec lookup(DatabaseId, Address, Language) -> {ok, Entry} | {error, Error}
            when DatabaseId :: atom(),
                 Address :: inet:ip_address() | nonempty_string() | binary(),
                 Language :: binary(),
                 Entry :: #{ binary() => term() | Entry },
                 Error :: (not_found | invalid_address | unsupported_language |
                           database_unknown | database_not_loaded |
                           ipv4_database).
lookup(DatabaseId, Address, Language) ->
    case locus_mmdb:lookup(DatabaseId, Address) of
        {ok, Entry} ->
            localize_entry(Entry, Language, true);
        {error, Error} ->
            {error, Error}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

localize_entry(Entry, Language, CatchFailure) ->
    try localize_entry_recur(Entry, Language) of
        LocalizedEntry ->
            {ok, LocalizedEntry}
    catch
        error:unsupported_language when CatchFailure ->
            {error, unsupported_language}
    end.

localize_entry_recur(Entry, Language) when is_map(Entry) ->
    case maps:take(<<"names">>, Entry) of
        {#{ Language := Name }, Entry2} ->
            Entry2#{ <<"name">> => Name };
        {#{} = _MissingLanguage, _Entry2} ->
            error(unsupported_language);
        {_NotLocalization, _Entry2} ->
            Entry;
        error ->
            maps:map(
              fun (_Key, ChildEntry) ->
                      localize_entry_recur(ChildEntry, Language)
              end,
              Entry)
    end;
localize_entry_recur(Entry, Language) when is_list(Entry) ->
    [localize_entry_recur(ChildEntry, Language) || ChildEntry <- Entry];
localize_entry_recur(Entry, _Language) ->
    Entry.
