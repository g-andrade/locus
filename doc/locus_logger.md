

# Module locus_logger #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`locus_event_subscriber`](locus_event_subscriber.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#set_loglevel-1">set_loglevel/1</a></td><td>Changes the logging verbosity in runtime.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="set_loglevel-1"></a>

### set_loglevel/1 ###

<pre><code>
set_loglevel(Level::debug | info | warning | error | none) -&gt; ok
</code></pre>
<br />

Changes the logging verbosity in runtime

`Level` must be either `info`, `warning`, `error` or `none`.

