

# locus #

[![Build Status](https://travis-ci.org/g-andrade/locus.png?branch=master)](https://travis-ci.org/g-andrade/locus)
[![Hex pm](http://img.shields.io/hexpm/v/locus.svg?style=flat)](https://hex.pm/packages/locus)


### <a name="locus_-_Geolocation_and_ASN_lookup_of_IP_addresses_using_MaxMind_GeoLite2">locus - Geolocation and ASN lookup of IP addresses using MaxMind GeoLite2</a> ###

```erlang

% locus:lookup("85.246.84.33").
{ok,#{autonomous_system_number => 3243,
      autonomous_system_organization =><<"Servicos De Comunicacoes E Multimedia S.A.">>,
      city => #{geoname_id => 2267057,name => <<"Lisbon">>},
      continent =>
          #{code => <<"EU">>,geoname_id => 6255148,name => <<"Europe">>},
      country =>
          #{geoname_id => 2264397,iso_code => <<"PT">>,
            name => <<"Portugal">>},
      location =>
          #{accuracy_radius => 200,latitude => 38.7167,
            longitude => -9.1333,time_zone => <<"Europe/Lisbon">>},
      postal => #{code => <<"1099-091">>},
      registered_country =>
          #{geoname_id => 2264397,iso_code => <<"PT">>,
            name => <<"Portugal">>},
      subdivisions =>
          [#{geoname_id => 2267056,iso_code => <<"11">>,
             name => <<"Lisbon">>}]}}

```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/locus/blob/master/doc/locus.md" class="module">locus</a></td></tr></table>

