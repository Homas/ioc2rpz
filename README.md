#  ioc2rpz - makes your threat intelligence actionable
turns your threat intelligence into RPZ feeds.
ioc2rpz is a place there threat intelligence meets DNS
## Overview

## Where to use
You can use ioc2rpz with any DNS server which supports Responce Policy Zones e.g. recent versions of bind.

## ioc2rpz vs bind:
- ioc2rpz built to handle RPZ distribution only
- ioc2rpz supports as many RPZ as you need. bind supports only 32 zones per DNS view
- live zones
- indicators via REST API calls
- IOC expiration time
- configurable packet size --> zone transfer optimization
- performace

## How to start ioc2rpz service

## ioc2rpz management
- ioc2rpz status
- reload config
- refresh all zones
- refresh a zone
- terminate ioc2rpz

## Configuration
1. AXFR update time - full Zone update and rebuild if MD5 is different. IOC added as insert_new (zone, ioc) -> to have an ability support IXFR
2. IXFR update time - incremental zone update



### Config file
The configuration is a file which consists of Erlang terms so the configuration must comply with Erlang syntax. ioc2rpz does not check the configuration file for possible errors, typos etc.
The configuration consist of:
- one **srv** record;
- zero or more **key** records;
- zero or more **whitelist** records;
- one or more **source** records;
- one or more **rpz** records.
#### **srv** record
**srv** record contains 3 parameters:
- NS server name used in SOA record;
- an email address for SOA record;
- list of management TSIG keys (names only). Please refer [the management section](#ioc2rpz-management) for the details.   
Sample **srv** record:  
```
{srv,{"ns1.example.com","support.email.example.com",["dnsmkey_1","dnsmkey_2","dnsmkey_3"]}}.
```
#### **key** record
Keys are used for zone transfers and management. It is recomended to use different keys for management and zones transfers.
**key** record contain:
- TSIG key name;
- algorythm. md5, sha256 and sha512 are supported';
- the key.
Sample **key** record:  
```
{key,{"key_name_1","md5","Hbxw9kzCdDp5XgWSWT/5OfRc1+jDIaSvFjpbv/V3IT2ah6xUfLGFcoA7cCLaPh40ni9nvmzlAArj856v3xEnBw=="}}.
```
dnssec-keygen utility can be used to generate TSIG keys. E.g. the command below will generate a TSIG key 512 bits lenght with "tsig-key" name using MD5 algorithm, and save the key in the files with extensions "key" and "private". A TSIG the key will be the same in the both files.
```
dnssec-keygen -a HMAC-MD5 -b512 -n USER tsig-key
```
For other details please refer "dnssec-keygen" documentation.
#### **whitelist** record
Whitelists are used to prevent clean/important domains and IPs from blocking by RPZ. The whitelisted IOCs are removed from the feeds. Whitelists must contain valid FQDNs and IP addresses. Indicators must be on separate lines.
**whitelists** record contains:
- whitelist name;
- whitelist path. URLs(http/https/fts) and local files are supported. Prefix "file:" is used for local files;
- REGEX which is used to extract indicators. A regular expression must be included in double quotes. If you specify an empty REGEX (`""`), a default REGEX will be used (`"^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$"`). `none` is used if no REGEX is required (the source already provides data in the required format).
Sample **whitelist** record:
```
{whitelist,{"whitelist_1","file:cfg/whitelist1.txt",none}}.
```
#### **source** record
Sources are malicious indicators feeds. The indicators must be on separate lines. FQDNs, IPv4 and IPv6-addresses are supported.
**source** record contains:
- source name;
- source path for full source transfer (AXFR). URLs(http/https/fts) and local files are supported. Prefix "file:" is used for local files;
- source path for incremental source transfer (IXFR). AXFR,IXFR paths support keywords to shortern URLs and provide zone update timestamps:
  - **[:AXFR:]** - full AXFR path. Can be used only in IXFR paths;
  - **[:FTimestamp:]** - timestamp when the source was last time updated  (e.g. 1507946281)
  - **[:ToTimestamp:]** - current timestamp;
- REGEX which is used to extract indicators and their expiration time. The first match is an indicator, the second match is expiration time. Expiration time is an optional parameter. 
Sample **source** record:
```
{source,{"blackhole_exp","http://data.netlab.360.com/feeds/dga/blackhole.txt","[:AXFR:]","^([A-Za-z0-9][A-Za-z0-9\-\._]+)\t.*:00\t([0-9: -]+)$"}}.
```
#### **rpz** record

{rpz,{"zone_name",soa_refresh, soa_update_retry,soa_expire,soa_nxdomain_ttl,"cache" true/false,"wildcards","action",["key1","key2"],"zone_type" mixed/fqdn/ip,AXFT_Time, IXFR_Time,["source1","source2"],["notify_ip1","notify_ip2"],["whitelist_1","whitelist_2"]}}.
```
{rpz,{"mixed.ioc2rpz",7202,3600,2592000,7200,"true","true","passthru",["dnsproxykey_1","dnsproxykey_2"],"mixed",86400,3600,["small_ioc","blackhole","bot.list"],[],["whitelist_1","whitelist_2"]}}.
```
actions: nxdomain, nodata, passthru, drop, tcp-only, [{"redirect_domain","example.com"},{"redirect_ip","127.0.0.1"},{"local_aaaa","fe80::1"},{"local_a","127.0.0.1"},{"local_cname","www.example.com"},{"local_txt","Text Record"}]
redirect_domain is an alias for local_cname
redirect_ip is an alias for local_a, local_aaaa

cache - cache the RPZ zones or request on a fly. Possible values: true/false
wildcards - generate wildcard rules to block sub-domain. Possible values: true/false
notify - send notifications about feed update to the systems which downloaded the feed. Possible values: true/false
action: (nxdomain, nodata, drop, redirect=domain/ip)
if RPZ feed is not cached anyway it is temporary put into a hot cache. Just in case if the request timeouts from a client, we will be able to respond next time. axfr_time will be used to derermine cache life. 0 - means not cache at all.


Action can be: single value, list of tuples. Single value defines a single action for an RPZ. A list depending on the param can define multiple results.
- Single value: "nodata", "nxdomain", "passthru", "drop", "tcp-only", "blockns" ("nsdname", "nsip")
- List:
  - Single action: {"redirect_domain", "www.example.com"}, {"redirect_ip", "127.0.0.1"} - as a response CNAME or A/AAAA records will be returned
  - Multiple actions: {"local_a","127.0.0.1"}, {"local_aaaa", "fe80::1"}, {"local_cname","www.example.com"}, {"local_txt","Just TXT record"}
  - [TODO] An action per source: {"",action,locdata} //default action ,{"source_name",action,locdata}

### Constants - ioc2rpz.hrl


### How the Full (AXFR) and Incremental(IXFR) caches are updated
- AXFR cache always contains prebuilt zones without SOA/NS/TSIG records. Prebuilt means all records are splitted by packets and labels were shortened/zipped.
- If server recieve an AXFR request it retrieve packets from the AXFR cache, add SOA/NS records and TSIG if needed.
- AXFR zones update should be considered as a clean up procedure, which should periodicaly take place. Just to be sure that there is no desynchronization between the sources and the cache.
- For large zones, AXFR updates should be scheduled infrequently to minimize impact on server's performance and ammount of transferred data to all clients.
- All changes if it is possible should be done via incremental zone updates. In that case the AXFR cache will be rebuilt only in case if a zone was updated.
- [TODO] Due to an optimization, only last packet will be rebuilt for new IOCs and relevant and accordant packets for the expired IOCs.
- IXFR cache contains only IOCs and expiration dates. [TODO] and packets ID's (to make it possible rebuild the zone fast).
- RPZ record contains current zone Serial and Serial_IXFR. Serial_IXFR serve as a minimum incremental zone serial which is available for an incremental zone transfer.
- IXFR cache is flushed after full zone update (AXFR). Serial_IXFR = Serial. Clients will recieve full zone update in any case, this is why it is important to have AXFR zone transfer infrequently.
- When IXFR is updated, AXFR cache must be rebuilt.
- If a zone does not support IXFR updates -> it doesn't saved in the IXFR table.
- Live zones are not cached in the AXFR, IXFR caches but the sources (IOCs) can be cached in the hot cache.

### Hot cache
IXFR updates are not cached in the hot cache

## TODO features
- [x] (*) TSIG Found Key ... 2017-10-24 10:58:58 Bad timestamp ... Valid MAC
- [ ] (*) http/https/ftp errors handling - source status in the record. If a source is not available - work w/o it
- [ ] (*) Source based on files check by mod.date and size -> read by chunks
- [ ] RPZ behaviour: ignore unreachable sources, use old data for unreachable sources, do not update the zone
- [ ] ACL for MGMT
- [ ] "intellectual" configuration update
- [ ] Statistics per zone (# records, last update, # AXFR, # IXFR, last axfr update time, avg axfr update time, last ixfr update time, avg ixfr update time)
- [ ] Performance testing vs bind:
  - [ ] 1 core/8Gb RAM: start time, zone transfer time, zone size, CPU, Memory
    - [ ] 100k rules
    - [ ] 1M rules
    - [ ] 10M rules
  - [ ] 4 cores/32 Gb RAM: start time, zone transfer time, zone size
    - [ ] 100k rules
    - [ ] 1M rules
    - [ ] 10M rules
- [ ] By a signal
  - [ ] Reload CFG
  - [ ] Refresh a zone
  - [ ] Refresh all zones
  - [ ] Terminate processes/Exit
- [x] (*) Path for DB
- [x] (*) Fix IPv6 reversing
- [ ] (*) FDateTime,ToDateTime,FDateTimeZ,ToDateTimeZ + support them for AXFR  
[:FDateTime:] = "2017-10-13 13:13:13", [:FDateTimeZ:] = "2017-10-13T13:13:13Z"  
[:ToDateTime:] = "2017-10-13 13:13:13", [:ToDateTimeZ:] = "2017-10-13T13:13:13Z"
- [x] (*) Sample cfg
- [ ] (*) Docker container
- [ ] (*) Documentation
- [ ] Check if RPZs are propertly configured.
- [ ] Add source RPZ
- [ ] Add source SQL
- [ ] Mnesia for storage (and auto creation)
- [ ] Distributed configuration based on mnesia
- [ ] Wait for a remote server confirms receiving a notification
- [ ] Additional local records: ptr, srv, mx etc

## Other/optimization TODO
- [ ] (*) Do not cache expired IOCs if ExpDateTime<Serial_IXFR / update ExpDateTime if exists
- [x] (*) IOC to lowercase - check performance impact
- [ ] (*) Check zones IXFR update from multiple sources
- [x] (*) Check all TODO in the code
- [ ] (1) Clean up the code & add comments
- [ ] (1) EDNS0 Support: DNS Cookie, edns-tcp-keepalive, NSID
- [ ] (2) IOC to lowercase - check memory usage impact (in ioc2rpz_conn)
- [ ] (3) UDP under supervisor
- [ ] (3) Memory optimization for huge zones (erl -pa ebin +MEas bf ?????)
- [ ] (3) Share IOC between the feeds in IXFR table

## TODO Bugs
- [ ] (*) Sample zone - fix redirect_domain, redirect_ip
- [ ] saveZones - doesn't correctly save zones if there a lot of updates. Save strategy based on update size and time.
- [x] Fix response if Zone not ready - respond SERVFAIL + add TSIG


## Free threat intel
- [DNS-BH – Malware Domain Blocklist by RiskAnalytics](http://www.malwaredomains.com/)
- [Malware DGA](http://data.netlab.360.com)
- [Tor Exit Nodes](https://torstatus.blutmagie.de/ip_list_exit.php/Tor_ip_list_EXIT.csv)

### References
- Domain Name System (DNS) IANA Considerations
https://tools.ietf.org/html/rfc6895
- Domain Names - Implementation and Specification
https://tools.ietf.org/html/rfc1035
- Incremental Zone Transfer in DNS
https://tools.ietf.org/html/rfc1995
- DNS Response Policy Zones (RPZ)
https://tools.ietf.org/html/draft-ietf-dnsop-dns-rpz-00
https://tools.ietf.org/html/draft-vixie-dns-rpz-02
- Secret Key Transaction Authentication for DNS (TSIG)
https://tools.ietf.org/html/rfc2845
- HMAC: Keyed-Hashing for Message Authentication
https://tools.ietf.org/html/rfc2104
- HMAC SHA TSIG Algorithm Identifiers
https://tools.ietf.org/html/rfc4635
- DNS Transport over TCP - Implementation Requirements
https://tools.ietf.org/html/rfc5966
- A Mechanism for Prompt Notification of Zone Changes (DNS NOTIFY)
https://tools.ietf.org/html/rfc1996
- Extension Mechanisms for DNS (EDNS(0))
https://tools.ietf.org/html/rfc6891
https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-11
- Domain Name System (DNS) Cookies
https://tools.ietf.org/html/rfc7873