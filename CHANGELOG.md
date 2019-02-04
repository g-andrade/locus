# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

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
- test coverage using MaxMind`s test data was greatly extended
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
