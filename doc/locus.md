

# Module locus #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-database_error">database_error()</a> ###


<pre><code>
database_error() = database_unknown | database_not_loaded
</code></pre>




### <a name="type-database_info">database_info()</a> ###


<pre><code>
database_info() = #{metadata =&gt; <a href="#type-database_metadata">database_metadata()</a>, source =&gt; <a href="#type-database_source">database_source()</a>, version =&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>}
</code></pre>




### <a name="type-database_metadata">database_metadata()</a> ###


<pre><code>
database_metadata() = #{binary() =&gt; term()}
</code></pre>




### <a name="type-database_source">database_source()</a> ###


<pre><code>
database_source() = {cache, string()} | {remote, string()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_info-1">get_info/1</a></td><td>Returns the properties of the currently loaded database [DEPRECATED].</td></tr><tr><td valign="top"><a href="#get_info-2">get_info/2</a></td><td>Returns a specific property of the currently loaded database [DEPRECATED].</td></tr><tr><td valign="top"><a href="#get_version-1">get_version/1</a></td><td>(<em>Deprecated</em>.) Returns the currently loaded database version [DEPRECATED].</td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td>Looks-up info on IPv4 and IPv6 addresses.</td></tr><tr><td valign="top"><a href="#start_loader-2">start_loader/2</a></td><td>Starts a database loader under id <code>DatabaseId</code></td></tr><tr><td valign="top"><a href="#stop_loader-1">stop_loader/1</a></td><td>Stops the database loader under id <code>DatabaseId</code></td></tr><tr><td valign="top"><a href="#wait_for_loader-1">wait_for_loader/1</a></td><td>Blocks caller execution until either readiness is achieved or a database load attempt fails.</td></tr><tr><td valign="top"><a href="#wait_for_loader-2">wait_for_loader/2</a></td><td>Like <code>wait_for_loader/1</code> but it can time-out.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_info-1"></a>

### get_info/1 ###

<pre><code>
get_info(DatabaseId) -&gt; {ok, Info} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Info = <a href="#type-database_info">database_info()</a></code></li><li><code>Error = database_unknown | database_not_loaded</code></li></ul>

Returns the properties of the currently loaded database [DEPRECATED]

- `DatabaseId` must be an atom and refer to a started database loader.

Returns:
- `{ok, database_info()}` in case of success
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, database_not_loaded}` if the database hasn't yet been loaded.

__See also:__ [get_info/2](#get_info-2).

<a name="get_info-2"></a>

### get_info/2 ###

<pre><code>
get_info(DatabaseId::atom(), Property::metadata) -&gt; {ok, <a href="#type-database_metadata">database_metadata()</a>} | {error, <a href="#type-database_error">database_error()</a>}
</code></pre>
<br />

Returns a specific property of the currently loaded database [DEPRECATED]

- `DatabaseId` must be an atom and refer to a started database loader.
- `Property` must be either `metadata` or `version`.

Returns:
- `{ok, Value}` in case of success
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, database_not_loaded}` if the database hasn't yet been loaded.

__See also:__ [get_info/2](#get_info-2).

<a name="get_version-1"></a>

### get_version/1 ###

<pre><code>
get_version(DatabaseId) -&gt; {ok, LoadedVersion} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>LoadedVersion = <a href="calendar.md#type-datetime">calendar:datetime()</a></code></li><li><code>Error = database_unknown | database_not_loaded</code></li></ul>

__This function is deprecated:__

Please use [`get_info/2`](#get_info-2) instead.

- `DatabaseId` must be an atom and refer to a started database loader.

Returns:
- `{ok, LoadedVersion}` in case of success
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, database_not_loaded}` if the database hasn't yet been loaded.

Returns the currently loaded database version [DEPRECATED]

<a name="lookup-2"></a>

### lookup/2 ###

<pre><code>
lookup(DatabaseId, Address) -&gt; {ok, Entry} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Address = <a href="inet.md#type-ip_address">inet:ip_address()</a> | nonempty_string() | binary()</code></li><li><code>Entry = #{binary() =&gt; term() | Entry}</code></li><li><code>Error = not_found | invalid_address | database_unknown | database_not_loaded | ipv4_database</code></li></ul>

Looks-up info on IPv4 and IPv6 addresses

- `DatabaseId` must be an atom and refer to a started database loader.
- `Address` must be either an `inet:ip_address()` tuple, or a string/binary
containing a valid representation of the address.

Returns:
- `{ok, Entry}` in case of success
- `{error, not_found}` if no data was found for this `Address`.
- `{error, invalid_address}` if `Address` is not either a `inet:ip_address()`
tuple or a valid textual representation of an IP address.
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, database_not_loaded}` if the database hasn't yet been loaded.
- `{error, ipv4_database}` if `Address` represents an IPv6 address and the database
only supports IPv4 addresses.

<a name="start_loader-2"></a>

### start_loader/2 ###

<pre><code>
start_loader(DatabaseId, DatabaseURL) -&gt; ok | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>DatabaseURL = string() | binary()</code></li><li><code>Error = invalid_url | already_started</code></li></ul>

Starts a database loader under id `DatabaseId`

`DatabaseId` must be an atom.
`DatabaseURL` must be either a string or a binary containing a HTTP(S) URL.

Returns:
- `ok` in case of success.
- `{error, invalid_url}` if the URL is invalid.
- `{error, already_started}` if the loader under `DatabaseId` has already been started.

__See also:__ [wait_for_loader/1](#wait_for_loader-1), [wait_for_loader/2](#wait_for_loader-2).

<a name="stop_loader-1"></a>

### stop_loader/1 ###

<pre><code>
stop_loader(DatabaseId) -&gt; ok | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Error = not_found</code></li></ul>

Stops the database loader under id `DatabaseId`

`DatabaseId` must be an atom and refer to a started database loader.

Returns `ok` in case of success, `{error, not_found}` otherwise.

<a name="wait_for_loader-1"></a>

### wait_for_loader/1 ###

<pre><code>
wait_for_loader(DatabaseId) -&gt; {ok, LoadedVersion} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>LoadedVersion = <a href="calendar.md#type-datetime">calendar:datetime()</a></code></li><li><code>Error = database_unknown | {loading, LoadingError}</code></li><li><code>LoadingError = term()</code></li></ul>

Blocks caller execution until either readiness is achieved or a database load attempt fails

- `DatabaseId` must be an atom and refer to a started database loader.

Returns:
- `{ok, LoadedVersion}` when the database is ready to use.
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, {loading, term()}}` if loading the database failed for some reason.

__See also:__ [start_loader/2](#start_loader-2), [wait_for_loader/2](#wait_for_loader-2).

<a name="wait_for_loader-2"></a>

### wait_for_loader/2 ###

<pre><code>
wait_for_loader(DatabaseId, Timeout) -&gt; {ok, LoadedVersion} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Timeout = timeout()</code></li><li><code>LoadedVersion = <a href="calendar.md#type-datetime">calendar:datetime()</a></code></li><li><code>Error = database_unknown | timeout | {loading, LoadingError}</code></li><li><code>LoadingError = term()</code></li></ul>

Like `wait_for_loader/1` but it can time-out

- `DatabaseId` must be an atom and refer to a started database loader.
- `Timeout` must be either a non-negative integer (milliseconds) or `infinity`.

Returns:
- `{ok, LoadedVersion}` when the database is ready to use.
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, timeout}` if we've given up on waiting.
- `{error, {loading, term()}}` if loading the database failed for some reason.

__See also:__ [start_loader/2](#start_loader-2), [wait_for_loader/1](#wait_for_loader-1).

