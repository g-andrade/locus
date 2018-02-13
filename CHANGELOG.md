# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

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
