

# Module locus #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td>Looks-up info on IPv4 and IPv6 addresses.</td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td>Looks-up localized info on IPv4 and IPv6 addresses.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Starts a database loader under id <code>DatabaseId</code></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stops the database loader under id <code>DatabaseId</code></td></tr><tr><td valign="top"><a href="#supported_languages-1">supported_languages/1</a></td><td>Returns the localization languages supported by the database.</td></tr><tr><td valign="top"><a href="#wait_until_ready-1">wait_until_ready/1</a></td><td>Blocks caller execution until the database has been loaded.</td></tr><tr><td valign="top"><a href="#wait_until_ready-2">wait_until_ready/2</a></td><td>Like <code>wait_until_ready/1</code> but it can time-out.</td></tr></table>


<a name="functions"></a>

## Function Details ##

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
- `{ok, Entry}` in case of success, with `Entry` being a map containing
the relevant data and with place names localized in English (when applicable.)
- `{error, not_found}` if no data was found for this `Address`.
- `{error, invalid_address}` if `Address` is not either a `inet:ip_address()`
tuple or a valid text representation of an IP address.
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, database_not_loaded}` if the database hasn't yet been loaded.
- `{error, ipv4_database}` if `Address` represents an IPv6 address and the database
only supports IPv4 addresses.

__See also:__ [lookup/3](#lookup-3).

<a name="lookup-3"></a>

### lookup/3 ###

<pre><code>
lookup(DatabaseId, Address, Language) -&gt; {ok, Entry} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Address = <a href="inet.md#type-ip_address">inet:ip_address()</a> | nonempty_string() | binary()</code></li><li><code>Language = binary()</code></li><li><code>Entry = #{binary() =&gt; term() | Entry}</code></li><li><code>Error = not_found | invalid_address | unsupported_language | database_unknown | database_not_loaded | ipv4_database</code></li></ul>

Looks-up localized info on IPv4 and IPv6 addresses

- `DatabaseId` must be an atom and refer to a started database loader.
- `Address` must be either an `inet:ip_address()` tuple, or a string/binary
- `Language` must be a non-empty binary containing a language code.

Returns:
- `{ok, Entry}` in case of success, with `Entry` being a map containing
the relevant data and with place names localized in `Language` (when applicable.)
- `{error, not_found}` if no data was found for this `Address`.
- `{error, invalid_address}` if `Address` is not either a `inet:ip_address()`
tuple or a valid text representation of an IP address.
- `{error, unsupported_language}` if the chosen `Language` isn't supported
by this particular database.
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, database_not_loaded}` if the database hasn't yet been loaded.
- `{error, ipv4_database}` if `Address` represents an IPv6 address and the database
only supports IPv4 addresses.

__See also:__ [lookup/2](#lookup-2), [supported_languages/1](#supported_languages-1).

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(DatabaseId, DatabaseURL) -&gt; ok | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>DatabaseURL = nonempty_string()</code></li><li><code>Error = already_started</code></li></ul>

Starts a database loader under id `DatabaseId`

`DatabaseId` must be an atom.
`DatabaseURL` must be a non-empty string containing a HTTP(S) URL.

Returns `ok` in case of success, `{error, already_started}` otherwise.

__See also:__ [wait_until_ready/1](#wait_until_ready-1), [wait_until_ready/2](#wait_until_ready-2).

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(DatabaseId) -&gt; ok | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Error = not_found</code></li></ul>

Stops the database loader under id `DatabaseId`

`DatabaseId` must be an atom and refer to a started database loader.

Returns `ok` in case of success, `{error, not_found}` otherwise.

<a name="supported_languages-1"></a>

### supported_languages/1 ###

<pre><code>
supported_languages(DatabaseId) -&gt; {ok, Languages} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Languages = [binary()]</code></li><li><code>Error = not_applicable | database_unknown | database_not_loaded</code></li></ul>

Returns the localization languages supported by the database

`DatabaseId` must be an atom and refer to a started database loader.

Returns:
- `{ok, Languages}`, with `Languages` a list of binaries, in case of success.
- `{error, not_applicable}` if the database doesn't support localization.
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, database_not_loaded}` if the database hasn't yet been loaded.

__See also:__ [lookup/3](#lookup-3).

<a name="wait_until_ready-1"></a>

### wait_until_ready/1 ###

<pre><code>
wait_until_ready(DatabaseId) -&gt; {ok, LoadedVersion} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>LoadedVersion = <a href="calendar.md#type-datetime">calendar:datetime()</a></code></li><li><code>Error = database_unknown | {loading, LoadingError}</code></li><li><code>LoadingError = term()</code></li></ul>

Blocks caller execution until the database has been loaded

- `DatabaseId` must be an atom and refer to a started database loader.

Returns:
- `ok` when the database is ready to use.
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, {loading, term()}}` if loading the database failed for some reason.

__See also:__ [start/2](#start-2), [wait_until_ready/2](#wait_until_ready-2).

<a name="wait_until_ready-2"></a>

### wait_until_ready/2 ###

<pre><code>
wait_until_ready(DatabaseId, Timeout) -&gt; {ok, LoadedVersion} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Timeout = timeout()</code></li><li><code>LoadedVersion = <a href="calendar.md#type-datetime">calendar:datetime()</a></code></li><li><code>Error = database_unknown | timeout | {loading, LoadingError}</code></li><li><code>LoadingError = term()</code></li></ul>

Like `wait_until_ready/1` but it can time-out

- `DatabaseId` must be an atom and refer to a started database loader.
- `Timeout` must be either a non-negative integer (milliseconds) or `infinity`.

Returns:
- `ok` when the database is ready to use.
- `{error, database_unknown}` if the database loader for `DatabaseId` hasn't been started.
- `{error, timeout}` if we've given up on waiting.
- `{error, {loading, term()}}` if loading the database failed for some reason.

__See also:__ [start/2](#start-2), [wait_until_ready/1](#wait_until_ready-1).

