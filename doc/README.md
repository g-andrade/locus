

# locus #

[![Hex pm](http://img.shields.io/hexpm/v/locus.svg?style=flat)](https://hex.pm/packages/locus)


### <a name="locus_-_Geolocation_and_ASN_lookup_of_IP_addresses_using_MaxMind_GeoLite2">locus - Geolocation and ASN lookup of IP addresses using MaxMind GeoLite2</a> ###

```erlang

% locus:start(city).
% locus:lookup(city, "85.246.84.33").
{ok,#{<<"continent">> =>
          #{<<"code">> => <<"EU">>,<<"geoname_id">> => 6255148,<<"name">> => <<"Europe">>},<<"country">> =>
          #{<<"geoname_id">> => 2264397,<<"iso_code">> => <<"PT">>,<<"name">> => <<"Portugal">>},<<"registered_country">> =>
          #{<<"geoname_id">> => 2264397,<<"iso_code">> => <<"PT">>,<<"name">> => <<"Portugal">>}}}

```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="locus.md" class="module">locus</a></td></tr>
<tr><td><a href="locus_logger.md" class="module">locus_logger</a></td></tr></table>

