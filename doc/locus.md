

# Module locus #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#supported_languages-1">supported_languages/1</a></td><td></td></tr><tr><td valign="top"><a href="#wait_until_ready-1">wait_until_ready/1</a></td><td></td></tr><tr><td valign="top"><a href="#wait_until_ready-2">wait_until_ready/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="lookup-2"></a>

### lookup/2 ###

<pre><code>
lookup(DatabaseId, Address) -&gt; {ok, Entry} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Address = <a href="inet.md#type-ip_address">inet:ip_address()</a> | nonempty_string() | binary()</code></li><li><code>Entry = #{binary() =&gt; term() | Entry}</code></li><li><code>Error = not_found | invalid_address | database_unknown | database_not_loaded | ipv4_database</code></li></ul>

<a name="lookup-3"></a>

### lookup/3 ###

<pre><code>
lookup(DatabaseId, Address, Language) -&gt; {ok, Entry} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Address = <a href="inet.md#type-ip_address">inet:ip_address()</a> | nonempty_string() | binary()</code></li><li><code>Language = binary()</code></li><li><code>Entry = #{binary() =&gt; term() | Entry}</code></li><li><code>Error = not_found | invalid_address | database_unknown | database_not_loaded | ipv4_database</code></li></ul>

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(DatabaseId) -&gt; ok | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Error = already_started</code></li></ul>

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(DatabaseId, DatabaseURL) -&gt; ok | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>DatabaseURL = nonempty_string()</code></li><li><code>Error = already_started</code></li></ul>

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(DatabaseId) -&gt; ok | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Error = not_found</code></li></ul>

<a name="supported_languages-1"></a>

### supported_languages/1 ###

<pre><code>
supported_languages(DatabaseId) -&gt; {ok, Languages} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Languages = [binary()]</code></li><li><code>Error = not_applicable | database_unknown | database_not_loaded</code></li></ul>

<a name="wait_until_ready-1"></a>

### wait_until_ready/1 ###

<pre><code>
wait_until_ready(DatabaseId) -&gt; {ok, LoadedVersion} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>LoadedVersion = <a href="calendar.md#type-datetime">calendar:datetime()</a></code></li><li><code>Error = database_unknown | timeout | {loading, LoadingError}</code></li><li><code>LoadingError = term()</code></li></ul>

<a name="wait_until_ready-2"></a>

### wait_until_ready/2 ###

<pre><code>
wait_until_ready(DatabaseId, Timeout) -&gt; {ok, LoadedVersion} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>DatabaseId = atom()</code></li><li><code>Timeout = timeout()</code></li><li><code>LoadedVersion = <a href="calendar.md#type-datetime">calendar:datetime()</a></code></li><li><code>Error = database_unknown | timeout | {loading, LoadingError}</code></li><li><code>LoadingError = term()</code></li></ul>

