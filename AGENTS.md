# locus

Erlang/OTP library for IP geolocation using MaxMind GeoIP2/GeoLite2 databases (MMDB format). Supports loading from HTTP(S) URLs, MaxMind's API, local filesystem, or custom fetchers. Also works with IPinfo and DB-IP. Includes a CLI tool (`locus check`) for database validation.

## Build, test, check

```bash
make compile         # compile
make test            # eunit + CT + CLI smoke test
make check           # xref + dead-code (hank) + lint (elvis) + dialyzer
make eunit           # unit tests only
make ct              # common test suites + coverage
make dialyzer        # type analysis
make shell           # interactive REPL with the app started
```

All checks run sequentially (`.NOTPARALLEL`). CI runs both `make test` and `make check` on OTP 24–28, Linux and Windows.

## Compiler flags

All four flags are always on: `warn_export_vars`, `warn_missing_spec`, `warn_unused_import`, `warnings_as_errors`. Every exported function must have a `-spec`. Test profiles relax `warn_missing_spec` and `warnings_as_errors`.

## Architecture

```
locus_sup
└── locus_database_sup  (one_for_one, transient children)
    └── locus_loader  (gen_server per named database)
```

- `locus_loader` manages the full lifecycle: fetch → validate → cache → periodic refresh.
- Loaded databases live in `persistent_term` (via `locus_database`), making lookups allocation-free.
- `locus_waiter` handles concurrent `await_loader` calls via monitors, not blocking the loader.

### Key modules

| Module | Role |
|---|---|
| `locus` | Public API: `start_loader/2-3`, `lookup/2`, `await_loader/1-2`, `get_info/1-2`, `check/1` |
| `locus_loader` | Core gen_server; orchestrates download/load, caching, updates |
| `locus_database` | `persistent_term`-backed in-memory database storage |
| `locus_mmdb_tree` | Binary search tree for IP range lookups (IPv4 + IPv6) |
| `locus_mmdb_data_codec` | MMDB binary format decoder |
| `locus_http_download` | Async HTTP(S) downloader with TLS verification |
| `locus_maxmind_download` | MaxMind-specific downloader with SHA-256 checksum |
| `locus_custom_fetcher` | Behaviour for custom data sources |
| `locus_event_subscriber` | Behaviour for event hooks |
| `locus_cli` | Escript CLI (`locus check`) |

## Code conventions

- Module names follow the pattern `locus_<subsystem>[_<role>].erl`.
- Internal functions only callable by tests are exported under `-ifdef(TEST)` or tagged `-ignore_xref([…])`.
- No `if` expressions; use pattern matching or `case`.
- Atom naming and line-length rules come from `elvis.config`; exceptions are documented there.
- `ERL_FLAGS = -enable-feature maybe_expr` is set by the Makefile — required for katana-code under OTP 25.

## Tests

Common Test suites are in `test/`:

- `locus_remote_sources_SUITE` — HTTP/custom-fetcher loading, multiple archive formats
- `locus_local_sources_SUITE` — Filesystem loading and updates
- `maxmind_main_mmdb_SUITE` — Official MaxMind test databases (IPv4/IPv6 lookups, metadata)
- `maxmind_bad_mmdb_SUITE` — Malformed database handling
- `locus_common_tests` — Shared test helpers
- `locus_test_utils` — Dynamic test-case discovery (`test_cases/1-2`)

Test data lives in `test/priv/` (fetched via `rebar_raw_resource` from MaxMind's git repo). Remote HTTP tests are disabled by default to avoid MaxMind rate-limiting; they require `MAXMIND_LICENSE_KEY` in the environment.

Use `?assertRecv(Pattern)` (30 s timeout) for async message assertions in test code.

## Dependencies

Runtime: `tls_certificate_check ~> 1.9` (TLS cert validation for HTTPS downloads).

Dev plugins: `rebar3_ex_doc`, `rebar3_hank` (dead code), `rebar3_lint` (Elvis), `rebar3_hex`. Both hank and lint are excluded on OTP 22 via `rebar.config.script`.

Test deps: `jsx ~> 3.1` (JSON), `maxmind_test_data` (git submodule of MaxMind's test DB repo).

## Releasing

`make publish` runs `rebar3 hex publish`. Versioning follows SemVer; history is in `CHANGELOG.md` (Keep a Changelog format).
