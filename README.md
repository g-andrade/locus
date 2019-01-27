# locus

[![](https://img.shields.io/hexpm/v/locus.svg?style=flat)](https://hex.pm/packages/locus)
[![](https://travis-ci.org/g-andrade/locus.png?branch=master)](https://travis-ci.org/g-andrade/locus)

`locus` is library for Erlang/OTP and Elixir that allows you to pinpoint
the country, city or ASN of IP addresses using MaxMind GeoIP2.

The free [MaxMind
databases](https://dev.maxmind.com/geoip/geoip2/geolite2/) you choose
are loaded on-demand and, if using HTTP, cached on the filesystem and
updated automatically.

You're encouraged to host your own private copies of the databases when
using this library in production, both for reliability and netiquette
towards MaxMind.

#### Usage

Clone the repository and run `make console` to bring up a
shell.

##### 1\. Start the database loader

``` erlang
URL = "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz",
ok = locus:start_loader(country, URL).

% URL can also be a local path, e.g. "/opt/MaxMind/GeoLite2-Country.tar.gz"
```

##### 2\. Wait for the database to load (optional)

``` erlang
% Either block indefinitely
{ok, _DatabaseVersion} = locus:wait_for_loader(country).
```

``` erlang
% ... or give-up after 30 seconds
{ok, _DatabaseVersion} = locus:wait_for_loader(country, 30000). % {error,timeout}
```

##### 3\. Lookup IP addresses

``` erlang

% > locus:lookup(country, "93.184.216.34").
% > locus:lookup(country, "2606:2800:220:1:248:1893:25c8:1946").

{ok,#{prefix => {{93,184,216,0},24},
      <<"continent">> =>
          #{<<"code">> => <<"NA">>,
            <<"geoname_id">> => 6255149,
            <<"names">> =>
                #{<<"de">> => <<"Nordamerika">>,
                  <<"en">> => <<"North America">>,
                  <<"es">> => <<"Norteamérica"/utf8>>,
                  <<"fr">> => <<"Amérique du Nord"/utf8>>,
                  <<"ja">> => <<"北アメリカ"/utf8>>,
                  <<"pt-BR">> => <<"América do Norte"/utf8>>,
                  <<"ru">> => <<"Северная Америка"/utf8>>,
                  <<"zh-CN">> => <<"北美洲"/utf8>>}},
      <<"country">> =>
          #{<<"geoname_id">> => 6252001,
            <<"iso_code">> => <<"US">>,
            <<"names">> =>
                #{<<"de">> => <<"USA">>,
                  <<"en">> => <<"United States">>,
                  <<"es">> => <<"Estados Unidos">>,
                  <<"fr">> => <<"États-Unis"/utf8>>,
                  <<"ja">> => <<"アメリカ合衆国"/utf8>>,
                  <<"pt-BR">> => <<"Estados Unidos">>,
                  <<"ru">> => <<"США"/utf8>>,
                  <<"zh-CN">> => <<"美国"/utf8>>}},
      <<"registered_country">> =>
          #{<<"geoname_id">> => 6252001,
            <<"iso_code">> => <<"US">>,
            <<"names">> =>
                #{<<"de">> => <<"USA">>,
                  <<"en">> => <<"United States">>,
                  <<"es">> => <<"Estados Unidos">>,
                  <<"fr">> => <<"États-Unis"/utf8>>,
                  <<"ja">> => <<"アメリカ合衆国"/utf8>>,
                  <<"pt-BR">> => <<"Estados Unidos">>,
                  <<"ru">> => <<"США"/utf8>>,
                  <<"zh-CN">> => <<"美国"/utf8>>}}}}
```

#### Documentation

1.  [On Databases](#on-databases)
2.  [On Formats](#on-formats)
3.  [On HTTP URLS: Downloading and
    Updating](#on-http-urls-downloading-and-updating)
4.  [On HTTP URLs: Caching](#on-http-urls-caching)
5.  [On Filesystem URLs: Loading and
    Updating](#on-filesystem-urls-loading-and-updating)
6.  [Logging](#logging)
7.  [Event Subscriptions](#event-subscriptions)
8.  [API Reference](#api-reference)
9.  [Tested Setup](#tested-setup)
10. [License](#license)
11. [Alternative Libraries (Erlang)](#alternative-libraries-erlang)
12. [Alternative Libraries (Elixir)](#alternative-libraries-elixir)

##### On Databases

  - The free GeoLite2 [Country, City and ASN
    databases](https://dev.maxmind.com/geoip/geoip2/geolite2/) were all
    successfully tested; presumably `locus` can deal with any MaxMind DB
    -formatted database that maps IP address prefixes to arbitrary data,
    but no [commercial
    databases](https://dev.maxmind.com/geoip/geoip2/downloadable/) have
    yet been tested
  - The databases are loaded into memory (mostly) as-is; reference
    counted binaries are shared with the application callers using ETS
    tables, and the original binary search tree is used to lookup
    addresses. The data for each entry is decoded on the fly upon
    successful lookups.

##### On Formats

  - Only gzip-compressed tarballs are supported at this moment
  - The first file to be found, within the tarball, with an .mmdb
    extension, is the one that's chosen for loading
  - The implementation of [MaxMind DB
    format](https://maxmind.github.io/MaxMind-DB/) is mostly complete

##### On HTTP URLs: Downloading and Updating

  - The downloaded tarballs are uncompressed in memory
  - The 'last-modified' response header, if present, is used to
    condition subsequent download attempts (using 'if-modified-since'
    request headers) in order to save bandwidth
  - The downloaded tarballs are cached on the filesystem in order to
    more quickly achieve readiness on future launches of the database
    loader
  - Until a HTTP database loader achieves readiness, download attempts
    are made every minute; once readiness is achieved (either from cache
    or network), this interval increases to every 6 hours. These can be
    tweaked using the 'pre\_readiness\_update\_period' and
    'post\_readiness\_update\_period' loader settings (in milliseconds.)
  - When downloading from a HTTPS URL, the remote certificate will be
    authenticated against a [list of known
    CAs](https://github.com/certifi/erlang-certifi) and connection
    negotiation will fail in case of expired certificates, mismatched
    hosts, and so on. These checks can be disabled using the `insecure`
    loader option.

##### On HTTP URLs: Caching

  - Caching is a best-effort; the system falls back to relying
    exclusively on the network if needed
  - A caching directory named 'locus\_erlang' is created under the
    ['user\_cache'
    basedir](http://erlang.org/doc/man/filename.html#basedir-3)
  - Cached tarballs are named after the SHA256 hash of their source URL
  - Modification time of the tarballs is extracted from 'last-modified'
    response header (when present) and used to condition downloads on
    subsequent boots and save bandwidth
  - Caching can be disabled by specifying the `no_cache` option when
    running `:start_loader`

##### On Filesystem URLs: Loading and Updating

  - The loaded tarballs are uncompressed in memory
  - Until a filesystem database loader achieves readiness, load attempts
    are made every 5 seconds; once readiness is achieved, this interval
    increases to every 30 seconds and load attempts are dismissed as
    long as the tarball modification timestamp keeps unchanged

##### Logging

  - Five logging levels are supported: `debug`, `info`, `warning`,
    `error` and `none`
  - The backend is
    [error\_logger](http://erlang.org/doc/man/error_logger.html); this
    usually plays nicely with `lager`
  - The default log level is `error`; it can be changed in the
    application's `env` config
  - To tweak the log level in runtime, use `locus_logger:set_loglevel/1`

##### Event Subscription

  - Any number of event subscribers can be attached to a database loader
    by specifying the `{event_subscriber, Subscriber}` option when
    starting the database
  - A `Subscriber` can be either a module implementing the
    `locus_event_subscriber` behaviour or an arbitrary `pid()`
  - The format and content of reported events can be consulted in detail
    on the `locus_event_subscriber` module documentation; most key steps
    in the loader pipeline are reported (download started, download
    succeeded, download failed, caching succeeded, loading failed, etc.)

##### API Reference

The API reference can be found on [HexDocs](https://hexdocs.pm/locus/).

##### Tested setup

  - Erlang/OTP 17.4 or newer
  - rebar3

##### License

MIT License

Copyright (c) 2017-2019 Guilherme Andrade

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

`locus` is an independent project and has not been authorized,
sponsored, or otherwise approved by MaxMind.

`locus` includes code extracted from OTP source code, by Ericsson AB,
released under the Apache License 2.0.

##### Alternative Libraries (Erlang)

  - [egeoip](https://github.com/mochi/egeoip): IP Geolocation module,
    currently supporting the MaxMind GeoLite City Database
  - [geodata2](https://github.com/brigadier/geodata2): Application for
    working with MaxMind geoip2 (.mmdb) databases
  - [geoip](https://github.com/manifest/geoip): Returns the location of
    an IP address; based on the ipinfodb.com web service
  - [geolite2data](https://hex.pm/packages/geolite2data): Periodically
    fetches the free MaxMind GeoLite2
    databases
  - [ip2location-erlang](https://github.com/ip2location/ip2location-erlang):
    Uses IP2Location geolocation database

##### Alternative Libraries (Elixir)

  - [asn](https://hex.pm/packages/asn): IP-to-AS-to-ASname lookup
  - [freegeoip](https://hex.pm/packages/freegeoip): Simple wrapper for
    freegeoip.net HTTP API
  - [freegeoipx](https://hex.pm/packages/freegeoipx): API Client for
    freegeoip.net
  - [geoip](https://hex.pm/packages/geoip): Lookup the geo location for
    a given IP address, hostname or Plug.Conn instance
  - [geolix](https://hex.pm/packages/geolix): MaxMind GeoIP2 database
    reader/decoder
  - [plug\_geoip2](https://hex.pm/packages/plug_geoip2): Adds geo
    location to a Plug connection based upon the client IP address by
    using MaxMind's GeoIP2 database
  - [tz\_world](https://hex.pm/packages/tz_world): Resolve timezones
    from a location efficiently using PostGIS and
Ecto

-----

*Generated by EDoc*
