

# locus #

[![Hex pm](http://img.shields.io/hexpm/v/locus.svg?style=flat)](https://hex.pm/packages/locus)


### <a name="locus_-_Geolocation_and_ASN_lookup_of_IP_addresses_using_MaxMind_GeoIP2">locus - Geolocation and ASN lookup of IP addresses using MaxMind GeoIP2</a> ###

`locus` is library for Erlang/OTP and Elixir that allows you to pinpoint the country,
city or ASN of IPv4 and IPv6 addresses.

The free [MaxMind databases](https://dev.maxmind.com/geoip/geoip2/geolite2/) you choose
are downloaded on-demand, cached on the filesystem and updated automatically.

You're encouraged to host your own private copies of the databases when using this library in production,
both for reliability and netiquette towards MaxMind.


#### <a name="Usage">Usage</a> ####

Run `make console` to bring up a shell.

<h5><a name="1._Start_the_database_loader">1. Start the database loader</a></h5>

```erlang

URL = "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz",
ok = locus:start_loader(country, URL).

```

<h5><a name="2._Wait_for_the_database_to_load_(optional)">2. Wait for the database to load (optional)</a></h5>

```erlang

% Either block indefinitely
{ok, _DatabaseVersion} = locus:wait_for_loader(country).

```

```erlang

% ... or give-up after 30 seconds
{ok, _DatabaseVersion} = locus:wait_for_loader(country, 30000). % {error,timeout}

```

<h5><a name="3._Lookup_IP_addresses">3. Lookup IP addresses</a></h5>

```erlang


% > locus:lookup(country, "93.184.216.34").
% > locus:lookup(country, "2606:2800:220:1:248:1893:25c8:1946").

{ok,#{<<"continent">> =>
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

Check the [function reference](#modules) for an overview on everything you can do.


#### <a name="Details">Details</a> ####

<h5><a name="Requirements">Requirements</a></h5>

* Erlang/OTP 19.3 or higher
* rebar3

<h5><a name="On_databases">On databases</a></h5>

* The free GeoLite2 [Country, City and ASN databases](https://dev.maxmind.com/geoip/geoip2/geolite2/)
  were all successfully tested; presumably `locus` can deal with any MaxMind DB -formatted database that maps
  IP address prefixes to arbitrary data, but no [
  commercial databases](https://dev.maxmind.com/geoip/geoip2/downloadable/) have yet been tested
* The databases are loaded into memory (mostly) as-is; reference counted binaries are shared with the
application callers using ETS tables, and the original binary search tree is used to lookup addresses.
The data for each entry is decoded on the fly upon successful lookups.

<h5><a name="On_formats">On formats</a></h5>

* Only gzip-compressed tarballs are supported at this moment
* The first file to be found, within the tarball, with an .mmdb extension, is the one that's chosen for loading
* The implementation of [MaxMind DB format](https://maxmind.github.io/MaxMind-DB/) is mostly complete

<h5><a name="On_downloading_and_updating">On downloading and updating</a></h5>

* The downloaded tarballs are uncompressed in memory
* The 'last-modified' response header, if present, is used to condition subsequent download
attempts (using 'if-modified-since' request headers) in order to save bandwidth
* The downloaded tarballs are cached on the filesystem in order to more quickly achieve readiness
on future launches of the database loader
* Until the database loader achieves readiness, download attempts are made every minute;
once readiness is achieved (either from cache or network), this interval increases to every 6 hours

<h5><a name="On_caching">On caching</a></h5>

* Caching is a best-effort; the system falls back to relying exclusively on the network if needed
* A caching directory named 'locus_erlang' is created under the ['user_cache' basedir](http://erlang.org/doc/man/filename#basedir-3)
* Cached tarballs are named after the SHA256 hash of their source URL
* Modification time of the tarballs is extracted from 'last-modified' response header (when present)
and used to condition downloads on subsequent boots and save bandwidth

<h5><a name="On_logging">On logging</a></h5>

* Four logging levels are supported: `info`, `warning`, `error` and `none`
* The backend is [error_logger](http://erlang.org/doc/man/error_logger); this usually plays nicely with `lager`
* The default log level is `error`; it can be changed in the application's `env` config
* To tweak the log level in runtime, use `locus_logger:set_loglevel/1`


#### <a name="Alternatives_(Erlang)">Alternatives (Erlang)</a> ####

* [egeoip](https://github.com/mochi/egeoip): IP Geolocation module, currently supporting the MaxMind GeoLite City Database
* [geodata2](https://github.com/brigadier/geodata2): Application for working with MaxMind geoip2 (.mmdb) databases
* [geoip](https://github.com/manifest/geoip): Returns the location of an IP address; based on the ipinfodb.com web service
* [geolite2data](https://hex.pm/packages/geolite2data): Periodically fetches the free MaxMind GeoLite2 databases
* [ip2location-erlang](https://github.com/ip2location/ip2location-erlang): Uses IP2Location geolocation database


#### <a name="Alternatives_(Elixir)">Alternatives (Elixir)</a> ####

* [asn](https://hex.pm/packages/asn): IP-to-AS-to-ASname lookup
* [freegeoip](https://hex.pm/packages/freegeoip): Simple wrapper for freegeoip.net HTTP API
* [freegeoipx](https://hex.pm/packages/freegeoipx): API Client for freegeoip.net
* [geoip](https://hex.pm/packages/geoip): Lookup the geo location for a given IP address, hostname or Plug.Conn instance
* [geolix](https://hex.pm/packages/geolix): MaxMind GeoIP2 database reader/decoder
* [plug_geoip2](https://hex.pm/packages/plug_geoip2): Adds geo location to a Plug connection based upon the client IP address by using MaxMind's GeoIP2 database
* [tz_world](https://hex.pm/packages/tz_world): Resolve timezones from a location efficiently using PostGIS and Ecto


#### <a name="Disclosure">Disclosure</a> ####
`locus` is an independent project and has not been authorized, sponsored, or otherwise approved by MaxMind.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="locus.md" class="module">locus</a></td></tr>
<tr><td><a href="locus_logger.md" class="module">locus_logger</a></td></tr></table>

