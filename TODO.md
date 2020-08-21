## Bugs
- [ ] Take a look on the bugs mentioned in REST section

## Core / DNS
- [ ] Migrate crypto:hmac/3 (depricated) to crypto:mac/4
- [ ] If IXFR source not set or equal AXFR - get removed records for IXFR
- [ ] Force RPZ, Source refresh
- [ ] RPZ from RPZs
- [ ] simple permissions model
- [ ] REST API rate limiting
- [ ] DNS requests rate limiting
- [ ] HotCache optimization if refresh time less than hotcache storage time
- [ ] Zone update - flush hot cache
- [x] Enforce domain validation. Discard indicators with wrong chars
  - [ ] (ioc2rpz:clean_labels). Performance should be validated.
- [ ] DoH https://tools.ietf.org/html/rfc8484
https://developers.google.com/speed/public-dns/docs/secure-transports
https://developers.cloudflare.com/1.1.1.1/dns-over-https/wireformat/

```
   When using the GET method, the data payload for this media type MUST
   be encoded with base64url [RFC4648] and then provided as a variable
   named "dns" to the URI Template expansion.  Padding characters for
   base64url MUST NOT be included.

   When using the POST method, the data payload for this media type MUST
   NOT be encoded and is used directly as the HTTP message body.

   The first example request uses GET to request "www.example.com".
   :method = GET
   :scheme = https
   :authority = dnsserver.example.net
   :path = /dns-query?dns=AAABAAABAAAAAAAAA3d3dwdleGFtcGxlA2NvbQAAAQAB
   accept = application/dns-message	 

   The same DNS query for "www.example.com", using the POST method would
   be:

   :method = POST
   :scheme = https
   :authority = dnsserver.example.net
   :path = /dns-query
   accept = application/dns-message
   content-type = application/dns-message
   content-length = 33

   <33 bytes represented by the following hex encoding>
   00 00 01 00 00 01 00 00  00 00 00 00 03 77 77 77
   07 65 78 61 6d 70 6c 65  03 63 6f 6d 00 00 01 00
   01


4.2.2.  HTTP Response Example

   This is an example response for a query for the IN AAAA records for
   "www.example.com" with recursion turned on.  The response bears one
   answer record with an address of 2001:db8:abcd:12:1:2:3:4 and a TTL
   of 3709 seconds.

   :status = 200
   content-type = application/dns-message
   content-length = 61
   cache-control = max-age=3709

   <61 bytes represented by the following hex encoding>
   00 00 81 80 00 01 00 01  00 00 00 00 03 77 77 77
   07 65 78 61 6d 70 6c 65  03 63 6f 6d 00 00 1c 00
   01 c0 0c 00 1c 00 01 00  00 0e 7d 00 10 20 01 0d
   b8 ab cd 00 12 00 01 00  02 00 03 00 04

```
- [ ] , A and AAAA requests. Optional A/AAAA support is added to be able to access the server via unique hostnames. In that case ioc2rpz behaves as an authoritative server
- [x] upgrade to tls1.3 (supported by Erlang)
- [ ] RPZ storage type: ets, mnesia
- [ ] Mnesia for storage (and auto creation)
https://github.com/ChicagoBoss/ChicagoBoss/wiki/Automatic-schema-initialization-for-mnesia

- [ ] Redo AXFR logs
- [ ] Access to the hotcache and the cfg_table via FUNs
- [ ] (1) Terminate updating zones during config reload
- [ ] (1) Clean up the code & add comments
- [ ] Logs level startup config
- [ ] Check delete in ioc2rpz: rpz_hotcache_table/pkthotcache

- [ ] Distributed configuration
- [ ] Wait while a remote server confirms receiving a notification
- [ ] (2) EDNS0 Support: DNS Cookie, edns-tcp-keepalive, NSID
- [ ] (3) Memory optimization for huge zones (erl -pa ebin +MEas bf ?????)
- [ ] DoD https://tools.ietf.org/html/draft-ietf-dprive-dnsodtls-06

- [ ] EUnit Tests for main funs.
- [ ] Handle RPZ update if one of a sources is not availble or a recent update returned significatnly low number of indicators

## Sources
- [x] Add a script for RPZ via "shell:"
- [ ] Simultanious source downloads
- [ ] Add source PostreSQL, MySQL via "shell:"
- [/] Dedup IoC from different sources with different expiration dates
- [ ] RPZ action per source
- [ ] (2) Source based on files check by mod.date and size -> read by chunks

- [ ] ioc type in config
- [ ] max file size
- [ ] RPZ action
- [ ] NS type
- [ ] lowcase optimization option
  - [ ] (1) IOC to lowercase - check memory usage impact (in ioc2rpz_conn)
- [ ] spawn processes
- [ ] Hot cache optimization depending on RPZ refresh time and source usage in multiple feeds
- [ ] Cache optimization for huge zones
- [ ] Statistics table


## RPZ
- [ ] Monitor significant drop in # of IoCs and if detected - postpone an update to 1 - 3 IXF cycles or specified time
- [ ] RPZ by source intersection
- [ ] Max # of IOCs
- [ ] Catalog zones
- [ ] Statistics per zone (# records, last update, # AXFR, # IXFR, last axfr update time, avg axfr update time, last ixfr update time, avg ixfr update time)
- [ ] RPZ behavior: ignore unreachable sources, use old data for unreachable sources, do not update the zone
- [ ] Additional local records: ptr, srv, mx etc
- [X] Replaced by RPZ from RPZs ----- An action per source: {"",action,locdata} //default action ,{"source_name",action,locdata}
- [ ] RPZ transfer rate limiting

- [ ] (2) FDateTime,ToDateTime,FDateTimeZ,ToDateTimeZ + support them for AXFR  
[:FDateTime:] = "2017-10-13 13:13:13", [:FDateTimeZ:] = "2017-10-13T13:13:13Z"  
[:ToDateTime:] = "2017-10-13 13:13:13", [:ToDateTimeZ:] = "2017-10-13T13:13:13Z"


## Servers
- [ ] Enforcement max # of IOCs
- [ ] Secondary DNS via MNESIA and distributed

## REST
- [ ] MGMT via REST API
  - [ ] Statistics per source, RPZ, performance
  - [ ] Bug RPZ stats after reload config
- [ ] Bug in cowboy. Can not send 501 in "catch all"

## Configuration

## Management
- [ ] DNS health check requests
- [ ] Disable MGMT via DNS (update ioc2rpz.gui first) - default behaviour


## Unsorted
- [ ] Performance testing vs bind:
  - [ ] 1 core/8GB RAM: start time, zone transfer time, zone size, CPU, Memory
    - [ ] 100k rules
    - [ ] 1M rules
    - [ ] 10M rules
  - [ ] 4 cores/32 GB RAM: start time, zone transfer time, zone size
    - [ ] 100k rules
    - [ ] 1M rules
    - [ ] 10M rules
- [ ] Switch from IXFR cache to Sources cache. IXFR cache allows you to support less zone updates but IOCs can be stored multiple times. Sources cache will contain duplicate IOCs from the same source but RPZs will be updated more frequently (looks like it is not bad).
  - [ ] (3) Share IOC between the feeds in IXFR table (do not forget about different whitelists)

## Other/optimization TODO
- [ ] (1) Do not cache expired IOCs if ExpDateTime<Serial_IXFR / update ExpDateTime if exists
- [ ] (1) Check zones IXFR update from multiple sources
