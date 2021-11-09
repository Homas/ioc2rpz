## Bugs
- [ ] Take a look on the bugs mentioned in REST section

## Core / DNS
- [x] Migrate crypto:hmac/3 (depricated) to crypto:mac/4
- [x] Upgrade Cowboy
- [ ] Sample zone is broken
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
- [ ] , A and AAAA requests. Optional A/AAAA support is added to be able to access the server via unique hostnames. In that case ioc2rpz behaves as an authoritative server
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
- [ ] Simultanious source downloads
- [ ] Add source PostreSQL, MySQL via "shell:"
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
- [ ] warm cache in mnesia
- [x] validate if source is downloading before trying to get it again
- [ ] Monitor significant drop in # of IoCs and if detected - postpone an update to 1 - 3 IXF cycles or specified time
- [ ] RPZ by source intersection
- [ ] Max # of IOCs
- [ ] Catalog zones
- [ ] Statistics per zone (# records, last update, # AXFR, # IXFR, last axfr update time, avg axfr update time, last ixfr update time, avg ixfr update time)
- [ ] RPZ behavior: ignore unreachable sources, use old data for unreachable sources, do not update the zone
- [ ] Additional local records: ptr, srv, mx etc
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
- [/] Disable MGMT via DNS (update ioc2rpz.gui first) - default behaviour


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
