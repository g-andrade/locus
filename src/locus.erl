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

-module(locus).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_loader/2]).                -ignore_xref({start_loader,2}).
-export([stop_loader/1]).                 -ignore_xref({stop_loader,1}).
-export([wait_for_loader/1]).             -ignore_xref({wait_for_loader,1}).
-export([wait_for_loader/2]).             -ignore_xref({wait_for_loader,2}).
-export([lookup/2]).                      -ignore_xref({lookup,2}).
-export([get_version/1]).                 -ignore_xref({get_version,1}).
-export([get_info/1]).                    -ignore_xref({get_info,1}).
-export([get_info/2]).                    -ignore_xref({get_info,2}).

%-deprecated([{get_version,1,eventually}]). % TODO Uncomment this on next major release

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type database_error() :: database_unknown | database_not_loaded.
-export_type([database_error/0]).

-type database_info() ::
        #{ metadata := database_metadata(),
           source := database_source(),
           version := calendar:datetime()
         }.
-export_type([database_info/0]).

-type database_metadata() :: #{ binary() => term() }.
-export_type([database_metadata/0]).

-type database_source() :: {cache, string()} | {remote, string()}.
-export_type([database_source/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Starts a database loader under id `DatabaseId'
%%
%% `DatabaseId' must be an atom.
%% `DatabaseURL' must be either a string or a binary containing a HTTP(S) URL.
%%
%% Returns:
%% - `ok' in case of success.
%% - `{error, invalid_url}' if the URL is invalid.
%% - `{error, already_started}' if the loader under `DatabaseId' has already been started.
%% @see wait_for_loader/1
%% @see wait_for_loader/2
-spec start_loader(DatabaseId, DatabaseURL) -> ok | {error, Error}
            when DatabaseId :: atom(),
                 DatabaseURL :: string() | binary(),
                 Error :: invalid_url | already_started.
start_loader(DatabaseId, BinDatabaseURL) when is_binary(BinDatabaseURL) ->
    DatabaseURL = binary_to_list(BinDatabaseURL),
    start_loader(DatabaseId, DatabaseURL);
start_loader(DatabaseId, DatabaseURL) ->
    case is_url(DatabaseURL) of
        true -> locus_sup:start_child(DatabaseId, DatabaseURL);
        false -> {error, invalid_url}
    end.

%% @doc Stops the database loader under id `DatabaseId'
%%
%% `DatabaseId' must be an atom and refer to a started database loader.
%%
%% Returns `ok' in case of success, `{error, not_found}' otherwise.
-spec stop_loader(DatabaseId) -> ok | {error, Error}
            when DatabaseId :: atom(),
                 Error :: not_found.
stop_loader(DatabaseId) ->
    locus_sup:stop_child(DatabaseId).

%% @doc Blocks caller execution until either readiness is achieved or a database load attempt fails
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%%
%% Returns:
%% - `{ok, LoadedVersion}' when the database is ready to use.
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, {loading, term()}}' if loading the database failed for some reason.
%%
%% @see wait_for_loader/2
%% @see start_loader/2
-spec wait_for_loader(DatabaseId) -> {ok, LoadedVersion} | {error, Error}
            when DatabaseId :: atom(),
                 LoadedVersion :: calendar:datetime(),
                 Error :: database_unknown | {loading, LoadingError},
                 LoadingError :: term().
wait_for_loader(DatabaseId) ->
    wait_for_loader(DatabaseId, infinity).

%% @doc Like `wait_for_loader/1' but it can time-out
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%% - `Timeout' must be either a non-negative integer (milliseconds) or `infinity'.
%%
%% Returns:
%% - `{ok, LoadedVersion}' when the database is ready to use.
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, timeout}' if we've given up on waiting.
%% - `{error, {loading, term()}}' if loading the database failed for some reason.
%% @see wait_for_loader/1
%% @see start_loader/2
-spec wait_for_loader(DatabaseId, Timeout) -> {ok, LoadedVersion} | {error, Error}
            when DatabaseId :: atom(),
                 Timeout :: timeout(),
                 LoadedVersion :: calendar:datetime(),
                 Error :: database_unknown | timeout | {loading, LoadingError},
                 LoadingError :: term().
wait_for_loader(DatabaseId, Timeout) ->
    locus_http_loader:wait(DatabaseId, Timeout).

%% @doc Looks-up info on IPv4 and IPv6 addresses
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%% - `Address' must be either an `inet:ip_address()' tuple, or a string/binary
%%    containing a valid representation of the address.
%%
%% Returns:
%% - `{ok, Entry}' in case of success
%% - `{error, not_found}' if no data was found for this `Address'.
%% - `{error, invalid_address}' if `Address' is not either a `inet:ip_address()'
%%    tuple or a valid textual representation of an IP address.
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, database_not_loaded}' if the database hasn't yet been loaded.
%% - `{error, ipv4_database}' if `Address' represents an IPv6 address and the database
%% only supports IPv4 addresses.
-spec lookup(DatabaseId, Address) -> {ok, Entry} | {error, Error}
            when DatabaseId :: atom(),
                 Address :: inet:ip_address() | nonempty_string() | binary(),
                 Entry :: #{ binary() => term() | Entry },
                 Error :: (not_found | invalid_address |
                           database_unknown | database_not_loaded |
                           ipv4_database).
lookup(DatabaseId, Address) ->
    locus_mmdb:lookup(DatabaseId, Address).

%% @doc Returns the currently loaded database version [DEPRECATED]
%% @deprecated Please use {@link get_info/2} instead.
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%%
%% Returns:
%% - `{ok, LoadedVersion}' in case of success
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, database_not_loaded}' if the database hasn't yet been loaded.
-spec get_version(DatabaseId) -> {ok, LoadedVersion} | {error, Error}
            when DatabaseId :: atom(),
                 LoadedVersion :: calendar:datetime(),
                 Error :: database_unknown | database_not_loaded.
get_version(DatabaseId) ->
    get_info(DatabaseId, version).

%% @doc Returns the properties of the currently loaded database [DEPRECATED]
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%%
%% Returns:
%% - `{ok, database_info()}' in case of success
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, database_not_loaded}' if the database hasn't yet been loaded.
%% @see get_info/2
-spec get_info(DatabaseId) -> {ok, Info} | {error, Error}
            when DatabaseId :: atom(),
                 Info :: database_info(),
                 Error :: database_unknown | database_not_loaded.
get_info(DatabaseId) ->
    case locus_mmdb:get_parts(DatabaseId) of
        {ok, Parts} ->
            {ok, info_from_db_parts(Parts)};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Returns a specific property of the currently loaded database [DEPRECATED]
%%
%% - `DatabaseId' must be an atom and refer to a started database loader.
%% - `Property' must be either `metadata' or `version'.
%%
%% Returns:
%% - `{ok, Value}' in case of success
%% - `{error, database_unknown}' if the database loader for `DatabaseId' hasn't been started.
%% - `{error, database_not_loaded}' if the database hasn't yet been loaded.
%% @see get_info/2
-spec get_info(DatabaseId :: atom(), metadata) -> {ok, database_metadata()} | {error, database_error()};
              (DatabaseId :: atom(), source)   -> {ok, database_source()} | {error, database_error()};
              (DatabaseId :: atom(), version)  -> {ok, calendar:datetime()} | {error, database_error()}.
get_info(DatabaseId, Property) ->
    case get_info(DatabaseId) of
        {ok, #{ Property := Value }} ->
            {ok, Value};
        {error, Error} ->
            {error, Error}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

is_url(String) ->
    try io_lib:printable_latin1_list(String) andalso
        http_uri:parse(String)
    of
        false -> false;
        {ok, _Result} -> true;
        {error, _Reason} -> false
    catch
        error:badarg -> false
    end.

info_from_db_parts(Parts) ->
    maps:with([metadata, source, version], Parts).
