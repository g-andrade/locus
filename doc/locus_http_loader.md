

# Module locus_http_loader #
* [Data Types](#types)

__Behaviours:__ [`?gen_statem`](%3fgen_statem.md).

<a name="types"></a>

## Data Types ##




### <a name="type-body">body()</a> ###


<pre><code>
body() = binary()
</code></pre>




### <a name="type-event">event()</a> ###


<pre><code>
event() = <a href="#type-event_request_sent">event_request_sent()</a> | <a href="#type-event_stream_dismissed">event_stream_dismissed()</a> | <a href="#type-event_stream_failed_to_start">event_stream_failed_to_start()</a> | <a href="#type-event_stream_started">event_stream_started()</a> | <a href="#type-event_stream_finished">event_stream_finished()</a> | <a href="#type-event_load_attempt_finished">event_load_attempt_finished()</a> | <a href="#type-event_cache_attempt_finished">event_cache_attempt_finished()</a>
</code></pre>




### <a name="type-event_cache_attempt_finished">event_cache_attempt_finished()</a> ###


<pre><code>
event_cache_attempt_finished() = {cache_attempt_finished, <a href="#type-filename">filename()</a>, ok} | {cache_attempt_finished, <a href="#type-filename">filename()</a>, {error, term()}}
</code></pre>




### <a name="type-event_load_attempt_finished">event_load_attempt_finished()</a> ###


<pre><code>
event_load_attempt_finished() = {load_attempt_finished, <a href="locus_mmdb.md#type-source">locus_mmdb:source()</a>, {ok, Version::<a href="calendar.md#type-datetime">calendar:datetime()</a>}} | {load_attempt_finished, <a href="locus_mmdb.md#type-source">locus_mmdb:source()</a>, {error, term()}}
</code></pre>




### <a name="type-event_request_sent">event_request_sent()</a> ###


<pre><code>
event_request_sent() = {request_sent, <a href="#type-url">url()</a>, <a href="#type-headers">headers()</a>}
</code></pre>




### <a name="type-event_stream_dismissed">event_stream_dismissed()</a> ###


<pre><code>
event_stream_dismissed() = {stream_dismissed, {http, <a href="#type-response_status">response_status()</a>, <a href="#type-headers">headers()</a>, <a href="#type-body">body()</a>}}
</code></pre>




### <a name="type-event_stream_failed_to_start">event_stream_failed_to_start()</a> ###


<pre><code>
event_stream_failed_to_start() = {stream_failed_to_start, {http, <a href="#type-response_status">response_status()</a>, <a href="#type-headers">headers()</a>, <a href="#type-body">body()</a>}} | {stream_failed_to_start, {error, term()}} | {stream_failed_to_start, timeout}
</code></pre>




### <a name="type-event_stream_finished">event_stream_finished()</a> ###


<pre><code>
event_stream_finished() = {stream_finished, BodySize::non_neg_integer(), {ok, TrailingHeaders::<a href="#type-headers">headers()</a>}} | {stream_finished, BodySize::non_neg_integer(), {error, term()}} | {stream_finished, BodySize::non_neg_integer(), {error, timeout}}
</code></pre>




### <a name="type-event_stream_started">event_stream_started()</a> ###


<pre><code>
event_stream_started() = {stream_started, <a href="#type-headers">headers()</a>}
</code></pre>




### <a name="type-filename">filename()</a> ###


<pre><code>
filename() = string()
</code></pre>




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = [{string(), string()}]
</code></pre>




### <a name="type-opt">opt()</a> ###


<pre><code>
opt() = {event_subscriber, module() | pid()} | no_cache
</code></pre>




### <a name="type-response_status">response_status()</a> ###


<pre><code>
response_status() = {100..999, binary()}
</code></pre>




### <a name="type-url">url()</a> ###


<pre><code>
url() = string()
</code></pre>

