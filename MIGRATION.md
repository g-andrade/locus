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
