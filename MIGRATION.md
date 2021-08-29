# Migration guide

Whenever there's an interface breaking change (a change in the project's major version),
required migration instructions will be detailed in this file.

## From [1.x] to [2.x]

### Update

- any code matching the return of `locus:lookup/2` to match `not_found` rather than
`{error, not_found}`:

```erlang
% before:
case locus:lookup(country, "93.184.216.34") of
    {ok, #{<<"country">> := #{<<"iso_code">> := Country}}} ->
        % [...]
    {error, not_found} ->
        % [...]
end.

% after:
case locus:lookup(country, "93.184.216.34") of
    {ok, #{<<"country">> := #{<<"iso_code">> := Country}}} ->
        % [...]
    not_found ->
        % [...]
end.
```

- calls to `wait_for_loader/1` to invoke `await_loader/1` instead:

```erlang
% before
case locus:wait_for_loader(country) of
    {ok, _LoadedVersion} ->
        % [...]
    {error, {loading, LoadAttemptFailure}} ->
        % [...]
end.

% after
case locus:await_loader(country) of
    {ok, _LoadedVersion} ->
        % [...]
    {error, {timeout, LoadAttemptFailures}} ->
        % Notice how `LoadAttemptFailures' is a list
        % [...]
end.
```

- calls to `wait_for_loader/2` to invoke `await_loader/2` instead:

```erlang
% before
case locus:wait_for_loader(country, _Timeout = 30000) of
    {ok, _LoadedVersion} ->
        % [...]
    {error, {loading, LoadAttemptFailure}} ->
        % [...]
    {error, timeout} ->
        % [...]
end.

% after
case locus:await_loader(country, _Timeout = 30000) of
    {ok, _LoadedVersion} ->
        % [...]
    {error, {timeout, LoadAttemptFailures}} ->
        % Notice how `LoadAttemptFailures' is a list
        % and there's no longer a separate return type for `timeout`
        % [...]
end.
```

- calls to `wait_for_loaders/2` to invoke `await_loaders/2` instead:

```erlang
% before
case locus:wait_for_loaders([country, asn], _Timeout = 30000) of
    {ok, _LoadedVersionPerDatabase} ->
        % [...]
    {error, {DatabaseId, LoaderFailure}} ->
        % [...]
    {error, timeout} ->
        % [...]
end.

% after
case locus:await_loaders([country, asn], _Timeout = 30000) of
    {ok, _LoadedVersionPerDatabase} ->
        % [...]
    {error, ErrorPerDatabase, PartialSuccesses} ->
        % Notice how `ErrorPerDatabase' and `PartialSuccesses' are maps
        % and there's no longer a separate return type for `timeout`
        % [...]
end.
```

- calls to `get_version/1` to invoke `get_info/2` instead:

```erlang
% before
{ok, LoadedVersion} = locus:get_version(country),

% after
{ok, LoadedVersion} = locus:get_info(country, version),
```

- any use of the `{pre_readiness_update_period, Interval}` loader option,
if exponential backoff (the default for some time) is not to your liking:

```erlang
% before
{pre_readiness_update_period, Interval}

% after
{error_retries, {backoff, Interval}}
```

- any use of the `{post_readiness_update_period, Interval}` loader option:

```erlang
% before
{post_readiness_update_period, Interval}

% after
{update_period, Interval}
```

- any code matching/using `metadata`, whether through `locus:get_info/1` or `locus:get_info/2`,
to use atom keys instead of binary ones:

```erlang
% before:
{ok, Metadata} = locus:get_info(country, metadata),
#{<<"binary_format_major_version">> := Major,
  <<"binary_format_minor_version">> := Minor,
  <<"node_count">> := _,
  <<"record_size">> := _,
  <<"ip_version">> := _,
  <<"database_type">> := _,
  <<"languages">> := _,
  <<"build_epoch">> := _,
  <<"description">> := _} = Metadata,

% after:
{ok, Metadata} = locus:get_info(country, metadata),
#{binary_format_version := {Major, Minor}, % Notice how this was two keys before
  node_count := _,
  record_size := _,
  ip_version := _,
  database_type := _,
  languages := _,
  build_epoch := _,
  description := _} = Metadata,
```

- any code calling and matching the return of `locus:analyze/1` with one
to `locus:check/1` (the semantics of the return type are wildly different;
consult the API reference for details)

### Remove

- your code matching or making use of `prefix` for a successful `locus:lookup/2`, e.g.

```erlang
% `prefix' is gone (open an issue if you'd like to see it back)
{ok, #{prefix := _}} = locus:lookup(country, "94.186.216.34")
```
