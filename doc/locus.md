

# Module locus #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#lookup-1">lookup/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="lookup-1"></a>

### lookup/1 ###

<pre><code>
lookup(Address) -&gt; {ok, Entry} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Address = <a href="inet.md#type-ip_address">inet:ip_address()</a> | nonempty_string() | binary()</code></li><li><code>Entry = #{atom() =&gt; term() | Entry}</code></li><li><code>Error = not_found | invalid_address | no_databases_configured | {database_not_loaded, DatabaseId}</code></li><li><code>DatabaseId = atom()</code></li></ul>

<a name="lookup-2"></a>

### lookup/2 ###

<pre><code>
lookup(Address, Language) -&gt; {ok, Entry} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Address = <a href="inet.md#type-ip_address">inet:ip_address()</a> | nonempty_string() | binary()</code></li><li><code>Language = binary()</code></li><li><code>Entry = #{atom() =&gt; term() | Entry}</code></li><li><code>Error = not_found | invalid_address | no_databases_configured | {database_not_loaded, DatabaseId} | unknown_language</code></li><li><code>DatabaseId = atom()</code></li></ul>

