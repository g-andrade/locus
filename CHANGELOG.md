# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- OTP 25.3 to CI

## [2.3.6] - 2022-09-20

### Fixed

- failed load of .tgz database when its mime type is listed as "application/x-tar"

## [2.3.5] - 2022-09-20

### Fixed

- broken CLI check when loading database from a HTTP(S) URL

## [2.3.4] - 2022-09-12

### Fixed

- broken compilation under Elixir's Mix (introduced in 2.3.3)

## [2.3.3] - 2022-09-12

### Fixed

- unwarranted compilation warning on OTP 25

## [2.3.2] - 2022-08-07

### Added

- OTP 25 to CI

### Fixed

- a few wrong specs
- `rebar3_lint` warnings on OTP 25
- `rebar3_hank` warnings on OTP 25
- Dialyzer warnings on OTP 25
- a few eqWAlizer warnings

## [2.3.1] - 2022-04-19

### Fixed

- spelling mistakes in CHANGELOG [Kian-Meng Ang]

## [2.3.0] - 2022-01-25

### Added

- the ability to customize database cache path [Bartosz Szafran]

## [2.2.2] - 2021-12-01

### Fixed

- wrong reference to custom database fetcher in README [Bartosz Szafran]

## [2.2.1] - 2021-09-22

### Fixed

- decoding of negative integers [made wrong in 2.0.0]

## [2.2.0] - 2021-09-03

### Changed

- documentation from edoc to ExDoc
- imported version of `tls_certificate_check` to '~> 1.9'

## [2.1.0] - 2021-08-30

### Added

- `locus_mmdb:unpack_tree_data_and_data_section/2` to the API
- `locus_mmdb_data_codec:parse_on_index/3` to the API
- CI on Windows

### Removed

- no longer warranted run time dependency (`public_key`)

### Fixed

- wrong return of errors during `:check/1` for maps containing maps or arrays
which had been checked previously
- wrong paths in `:check/1` errors or warnings concerning maps
- no longer warranted type definition

## [2.0.0] - 2021-08-30

### Added

- support for retrieving databases using consumer-defined `locus_custom_fetcher`s
- support for decoding IEEE-754 infinities in MMDB data
- `locus:check/1` to API (which replaces `locus:analyze/1` and can be up to *3200% faster* 🏎️)
- details to MMDB unpacking errors
- linting checks with `rebar3_lint`
- dead code checks with `rebar3_hank`

### Changed

- ⚠️ **return type of `locus:lookup/2`** (see MIGRATION.md)
- database loader to use `persistent_term` instead of ETS
- MMDB decoder to perform stricter metadata validations
- MMDB decoder to not crash upon maps containing duplicate keys
- databases downloaded through HTTP(S) without a `last-modified` response header to no longer
be cached
- imported version of `tls_certificate_check` to '~> 1.7'
- single CT suite covering both filesystem and HTTP sources into one for each
- test coverage for the better

### Removed

- ⚠️ **`locus:wait_for_loader/1` from API** (deprecated in 1.10.0 - see MIGRATION.md)
- ⚠️ **`locus:wait_for_loader/2` from API** (deprecated in 1.10.0 - see MIGRATION.md)
- ⚠️ **`locus:wait_for_loaders/2` from API** (deprecated in 1.10.0 - see MIGRATION.md)
- ⚠️ **`locus:get_version/1` from API** (deprecated in 1.4.0 - see MIGRATION.md)
- ⚠️ **`locus:analyze/1` from API** (`locus:check/1` now fulfills this role - see MIGRATION.md)
- **deprecated loader options** `pre_readiness_update_period` and `post_readiness_update_period`
(see MIGRATION.md)
- warnings on the use of discontinued GeoLite2 HTTP URLs

## [1.16.1] - 2021-07-12

### Fixed

- erroneous decoding of 32-bit signed integers as signed when they're less than 4 bytes in length
- small risk of API functions `:await_loader` and `:await_loaders` blocking indefinitely longer
than they should

## [1.16.0] - 2021-05-13

### Added

- OTP 24 to CI targets

### Changed

- minimum Erlang/OTP version to 22
- imported version of `tls_certificate_check` to '~> 1.5'

## [1.15.0] - 2021-04-02

### Changed

- imported version of `tls_certificate_check` to '~> 1.3'
- minimum Erlang/OTP version to 21.2

### Removed

- `stacktrace_compat` dependency
- (now-)dead code that dealt with pre- OTP 21.2 differences in `stdlib` and such
- unmaintained compatibility with rebar 2

### Fixed

- import of MaxMind test data in a way that works both locally and on GHA

## [1.14.1] - 2021-03-16

### Fixed

- Hex documentation

## [1.14.0] - 2021-03-12

### Changed

- imported version of `tls_certificate_check` to 1.2.0

### Fixed

- broken `tls_certificate_check`s when using rebar3 3.14.4

## [1.13.2] - 2020-12-10

### Changed

- CI from Travis to GitHub actions

### Fixed

- documentation unavailability in hexdocs.pm (hopefully)

## [1.13.1] - 2020-12-08

### Fixed

- `tls_certificate_check` compilation errors on OTP 20.1+, when on top of macOS Big Sur

## [1.13.0] - 2020-12-05

### Changed

- CA bundles, based on the latest mkcert.org full CA list as of Nov 13, 2020
- imported `stacktrace_compat` version to benefit from its latest improvements

## [1.12.2] - 2020-10-29

### Fixed

- erroneous execution of the optional "remote HTTP" test group when a license key is lacking

## [1.12.1] - 2020-10-16

### Fixed

- misdetection of Mix as being rebar 2 and the erroneous compilation warnings that followed it

## [1.12.0] - 2020-05-21

### Added

- support for OTP 23

### Changed

- checksum verification algorithm of MaxMind downloads from MD5 to SHA-256

## [1.11.0] - 2020-05-11

### Added

- support for HTTPS redirections across distinct hostnames
- support for not censoring license keys from MaxMind URLs mentioned in logs
- support for censorship of arbitrary query arguments from HTTP URLs mentioned in logs
- truncation of large HTTP URLs mentioned in logs
- `download_redirected` events to HTTP downloads
- `too_many_redirections` and `invalid_redirection` reasons to HTTP download failures
- mention of compatibility with other providers

### Changed

- consumer-subscribed events as to follow the same URL censorship rules as the built-in logger
- documented MaxMind database edition format from atoms to tuples while keeping retrocompatibility

## [1.10.2] - 2020-03-13

### Added

- unmaintained compatibility with rebar 2

## [1.10.1] - 2020-03-11

### Changed

- CA bundles, based on the latest Mozilla Included CA Certificate List [Paulo Oliveira]

### Fixed

- misformatted log warning
- use of deprecated function in README example of how to use the library
- display of private function in generated function reference

## [1.10.0] - 2020-02-12

### Added

- `:await_loader` API function which, contrary to `:wait_for_loader`, will await readiness
up to the entire specified interval (rather than return upon the first encountered failure)
- checksum verification of databases downloaded directly from MaxMind
- rejection of successful HTTP downloads if body size doesn't match `content-length`
- censorship of license key from database URLs mentioned in logs
- purging of very large binaries from internally caught exceptions which are known error cases,
as to lower the risk of the VM getting OOM-killed when logging formatters get their hands
on those very large chunks of data

### Changed

- default behaviour upon failing to load a database, as to retry loading while exponentially backing off
(using very short intervals at first)

### Deprecated

- `:wait_for_loader` and `:wait_for_loaders` API functions (use `:await_loader` and `:await_loaders`
instead)

### Fixed

- incomplete spec for `locus_loader:event()` type
- wrong spec for `locus_maxmind_download:msg()` and `locus_maxmind_download:event()` types

## [1.9.0] - 2020-02-03

### Changed

- documentation as to reflect the recent changes to MaxMind licensing requirements

## [1.9.0-beta] - 2020-01-02

### Added

- support for loading databases with full awareness of license keys (now mandatory)

### Deprecated

- the use of discontinued "https://geolite.maxmind.com/download/geoip/database/GeoLite2-..." database URLs

## [1.8.0] - 2019-11-05

### Added

- support for returning types other than map upon successful lookups

### Changed

- MMDB decoder, which was split into separate tree, data section and analysis modules
- imported `stacktrace_compat` version [1.0.2 => 1.1.1]

### Removed

- support for OTP 18

### Fixed

- incidents of `locus` managerial processes keeping references to old binaries, upon a database update,
  for a potentially unlimited time (OTP 20+ only)
- broken logging of playground shell on OTP 21.1+

## [1.7.0] - 2019-08-12

### Added

- ability of loading databases from uncompressed tarballs (`.tar` files)
- ability of loading unpacked databases (`.mmdb` and `.mmdb.gz` files)
- stacktrace of caught exceptions to event reporting (including custom logger)
- ability of launching database loaders under library consumers' own supervisors
- `wait_for_loaders/2` API method for concurrently awaiting multiple database loaders

### Changed

- log level of HTTP and filesystem database loading failures from warning to error
- HTTP and filesystem loaders into a common loader codebase
- caching of HTTP databases as to store and load compressed `.mmdb` files rather than tarballs
- supervision structure as to launch database loaders as transient processes under a new `simple_one_for_one` supervisor
- dependency versions:
    - `certifi` [2.4.2 => 2.5.1]
    - `ssl_verify_fun` [1.1.4 => 1.1.5]

### Removed

- support for OTP 17.4 and 17.5
- undocumented support for rebar 2
- half-baked and unwarranted support for `file://`-prefixed URLs

### Fixed

- case-sensitive patterning of `.mmdb` file extensions within tarballs
- overly verbose `logger` messages on OTP 21.1+
- HTTPS certificate validation test cases on OTP 22

## [1.6.2] - 2019-03-16

### Fixed

- Dialyzer warning on OTP 21.3

## [1.6.1] - 2019-02-04

### Fixed

- crash in HTTP loader when database URL scheme is not in lower case
  (introduced in 1.6.0)

## [1.6.0] - 2019-01-27

### Added

- new API method for validating loaded databases (`locus:analyze/1`)
- new command line tool supporting database validation
- new dependencies:
    - `certifi` 2.4.2
    - `ssl_verify_fun` 1.1.4
    - `stacktrace_transform` 1.0.2

### Changed

- test coverage using MaxMind's test data was greatly extended
- database decoder was thoroughly optimized
- documentation was mildly improved

### Fixed

- misguided rejection of UTF-8 strings with non-printable (but valid) codepoints
- unnecessarily strict refusal to load 2.x database formats succeeding 2.0

### Security

- safety of database HTTPS downloads was substantially improved by now
  rejecting expired certificates, mismatched hostnames, self-signed
  certificates or unknown certificate authorities
- infinite recursion in maliciously crafted databases due to
  circular paths is now prevented

## [1.5.1] - 2019-01-19

### Fixed

- unwarranted import of rebar3_hex plugin in library consumers

## [1.5.0] - 2018-11-25

### Added

- ability of tweaking pre- and post-readiness database update periods
- test coverage of HTTP-loaded database updates

### Fixed

- undeterministic test cases which sometimes broke

## [1.4.0] - 2018-06-20

### Added

- official test cases for good data at https://github.com/maxmind/MaxMind-DB/

### Changed

- string and binary IP address parsing to handle ranges and shortened addresses

### Deprecated

- :get_version/1 (use :get_info/2)

## [1.3.1] - 2018-05-04

### Fixed

- incompatibility with OTP 20.3.1 due to gen_statem bug (issue with init actions)

## [1.2.3] - 2018-05-04

### Fixed

- incompatibility with OTP 20.3.1 due to gen_statem bug (issue with init actions)

## [1.1.5] - 2018-05-04

### Fixed

- incompatibility with OTP 20.3.1 due to gen_statem bug (issue with init actions)

## [1.0.2] - 2018-05-04

### Fixed

- incompatibility with OTP 20.3.1 due to gen_statem bug (issue with init actions)

## [1.3.0] - 2018-03-28

### Added

- ability of loading databases from local file system
- type spec of database entries

### Fixed

- wrong handling of timezones on cached tarballs
- wrong handling of daylight saving time on conditional HTTP requests

## [1.2.2] - 2018-03-28

### Fixed

- wrong handling of timezones on cached tarballs
- wrong handling of daylight saving time on conditional HTTP requests

## [1.1.4] - 2018-03-28

### Fixed

- wrong handling of timezones on cached tarballs
- wrong handling of daylight saving time on conditional HTTP requests

## [1.0.1] - 2018-03-28

### Fixed

- wrong handling of timezones on cached tarballs
- wrong handling of daylight saving time on conditional HTTP requests

## [1.2.1] - 2018-02-15

### Fixed

- undesirable inlining

## [1.2.0] - 2018-02-15

### Added

- OTP 17.4 and 17.5 support

## [1.1.3] - 2018-02-13

### Fixed

- matched IP address prefix not being returned on successful lookups

## [1.1.2] - 2018-02-13

### Fixed

- documentation external links not opening in parent frame

## [1.1.1] - 2018-02-12

### Fixed

- internal functions showing up as public in documentation
- documented description of `locus_logger:set_loglevel/2`

## [1.1.0] - 2018-02-11

### Added

- OTP 18 support
- OTP 19.0, 19.1 and 19.2 support
- ability of consulting database metadata, source and version through `:get_info`
- ability of subscribing database loader events
- ability of specifying connect, download start and idle download timeouts
- ability of turning off cache

## [1.0.0] - 2017-12-31

### Added

- ability of downloading .mmdb databases using HTTP
- ability of querying those databases
- ability of caching said databases on the local filesystem
- ability of conditioning said downloads based on cache content and 'if-modified-since' headers
- ability of regularly performing database update attempts
- OTP 19.3 and 20.x for all of the above
