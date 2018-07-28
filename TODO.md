#TODO

## Configuration

## Sources
- [ ] IOC type to a source level
- [ ] Max # or rules
- [x] "shell:" source type (executing a shell cmd)
- [ ] additional tools and scripts for "shell:" (exists cat, awk, grep, nslookup )
  - [x] dig
  - [x] curl
  - [x] python3
- [ ] Add source RPZ via "shell:"
- [ ] Add source PostreSQL, MySQL via "shell:"
- [ ] RPZ action per source
- [ ] Max IOCs, current IOCs

## REST

## Management
- [ ] MGMT via REST API
  - [ ] Reload CFG
  - [ ] Refresh a zone
  - [ ] Refresh all zones
  - [ ] Terminate processes/Exit
  - [ ] Statistics per source, RPZ, performance
- [ ] (2) MGMT via DNS move to a separate port/IP

## RPZ maintanance
- [ ] (2) FDateTime,ToDateTime,FDateTimeZ,ToDateTimeZ + support them for AXFR  
[:FDateTime:] = "2017-10-13 13:13:13", [:FDateTimeZ:] = "2017-10-13T13:13:13Z"  
[:ToDateTime:] = "2017-10-13 13:13:13", [:ToDateTimeZ:] = "2017-10-13T13:13:13Z"

## Unsorted
- [ ] Configuration file name pass as a variable to the container
- [x] (1) http/https/ftp errors handling - source status in the record. If a source is not available - work w/o it
  - [ ] RPZ behavior: ignore unreachable sources, use old data for unreachable sources, do not update the zone
  - [ ] (2) Source based on files check by mod.date and size -> read by chunks
  - [ ] Retry if source is not available
- [ ] Statistics per zone (# records, last update, # AXFR, # IXFR, last axfr update time, avg axfr update time, last ixfr update time, avg ixfr update time)
- [ ] Performance testing vs bind:
  - [ ] 1 core/8GB RAM: start time, zone transfer time, zone size, CPU, Memory
    - [ ] 100k rules
    - [ ] 1M rules
    - [ ] 10M rules
  - [ ] 4 cores/32 GB RAM: start time, zone transfer time, zone size
    - [ ] 100k rules
    - [ ] 1M rules
    - [ ] 10M rules
- [ ] Mnesia for storage (and auto creation)
- [ ] Distributed configuration
- [ ] Wait while a remote server confirms receiving a notification
- [ ] Additional local records: ptr, srv, mx etc
- [ ] An action per source: {"",action,locdata} //default action ,{"source_name",action,locdata}
- [ ] Switch from IXFR cache to Sources cache. IXFR cache allows you to support less zone updates but IOCs can be stored multiple times. Sources cache will contain duplicate IOCs from the same source but RPZs will be updated more frequently (looks like it is not bad).
  - [ ] (3) Share IOC between the feeds in IXFR table (do not forget about different whitelists)
- [ ] Access to the hotcache and the cfg_table via FUNs
- [ ] DNS over TLS https://tools.ietf.org/html/rfc8310
- [ ] Parrallel source downloads

## Other/optimization TODO
- [ ] (1) Do not cache expired IOCs if ExpDateTime<Serial_IXFR / update ExpDateTime if exists
- [ ] (1) Check zones IXFR update from multiple sources
- [ ] (1) Clean up the code & add comments
- [ ] (2) EDNS0 Support: DNS Cookie, edns-tcp-keepalive, NSID
- [ ] (1) IOC to lowercase - check memory usage impact (in ioc2rpz_conn)
- [ ] (2) UDP & TableMGMT under supervisors
- [ ] (3) Memory optimization for huge zones (erl -pa ebin +MEas bf ?????)
- [ ] (*) saveZones - doesn't correctly save zones if there a lot of updates. Save strategy based on update size and time and currently running updates.
- [ ] Logs level startup config
- [ ] Check delete in ioc2rpz: rpz_hotcache_table/pkthotcache
- [ ] (1) Terminate updating zones during config reload