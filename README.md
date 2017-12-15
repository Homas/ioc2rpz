#  ioc2rpz makes your threat intelligence actionable
ioc2rpz is a place where threat intelligence meets DNS.
## Overview
ioc2rpz transforms IOC feeds into response policy zones. You can mix sources to generate a single RPZ or multiple RPZs. Good domains and IPs can be whitelisted. ioc2rpz supports expiration of indicators and accordingly rebuilding zones.  
![Alt ioc2rpz](https://github.com/Homas/ioc2rpz/blob/master/IOC2RPZ.jpg)
Currently ioc2rpz supports local files, http/https/ftp. You can do file transfers or REST API calls to retrive indicators from remote servers.

## How to use
You can use ioc2rpz with any DNS server which supports Responce Policy Zones e.g. recent versions of bind. A sample bind's configuration file is provided in the cfg folder.

## ioc2rpz vs bind:
- ioc2rpz was built to handle RPZ distribution only;
- ioc2rpz supports as many RPZ as you need. bind supports only 32 zones per DNS view;
- ioc2rpz supports "live"/non cached zones;
- indicators availble from different sources e.g. via REST API calls and RPZs are automatically updated;
- IOC expiration time is used to remove expired indicators in a timely manner;
- Performance and zone transfer time/size/packets optimizations.

## How to start ioc2rpz service

## Docker container

## ioc2rpz management
- ioc2rpz status
- reload config
- refresh all zones
- refresh a zone
- terminate ioc2rpz

## Configuration file
The configuration is an Erlang file. Every configuration option is an Erlang term so the configuration must comply with Erlang syntax. ioc2rpz does not check the configuration file for possible errors, typos etc.
ioc2rpz supports the following configuration parameters:
- a single **srv** record (required);
- zero or more **key** records (optional);
- zero or more **whitelist** records (optional);
- one or more **source** records (minimum one source is required);
- one or more **rpz** records (minimum one rpz is required).
### **srv** record
**srv** record contains 3 parameters:
- NS server name used in SOA record;
- an email address for SOA record;
- list of management TSIG keys (names only). Please refer [the management section](#ioc2rpz-management) for the details.   
Sample **srv** record:  
```
{srv,{"ns1.example.com","support.email.example.com",["dnsmkey_1","dnsmkey_2","dnsmkey_3"]}}.
```
### **key** record
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
For other information please refer "dnssec-keygen" documentation.
### **whitelist** record
Whitelists are used to prevent good domains and IPs from blocking by RPZ. The whitelisted IOCs are removed from response policy zones. A white list is a text file of feed of text data. Indicators should be separated by newline characters (/n,/r or both /n/r).  Whitelists must contain valid FQDNs and/or IP addresses. ioc2rpz supports unlimited count of indicators.
**whitelists** record contains:
- whitelist name;
- whitelist path. URLs(http/https/fts) and local files are supported. Prefix "file:" is used for local files;
- REGEX which is used to extract indicators. A regular expression must be included in double quotes. If you specify an empty REGEX (`""`), a default REGEX will be used (`"^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$"`). `none` is used if no REGEX is required (the source already provides data in the required format).
Sample **whitelist** record:
```
{whitelist,{"whitelist_1","file:cfg/whitelist1.txt",none}}.
```
### **source** record
A source is a feed of malicious indicators. FQDNs, IPv4 and IPv6-addresses are supported. A source is a text file of feed of text data. Indicators should be separated by newline/carriage return characters (/n,/r or both /r/n). ioc2rpz supports unlimited count of indicators.
**source** record contains:
- source name;
- source path for full source transfer (AXFR). URLs(http/https/fts) and local files are supported. Prefix "file:" is used for local files;
- source path for incremental source transfer (IXFR). AXFR,IXFR paths support keywords to shortern URLs and provide zone update timestamps:
  - **[:AXFR:]** - full AXFR path. Can be used only in IXFR paths;
  - **[:FTimestamp:]** - timestamp when the source was last time updated  (e.g. 1507946281)
  - **[:ToTimestamp:]** - current timestamp;
- REGEX which is used to extract indicators and their expiration time. The first match is an indicator, the second match is expiration time. Expiration time is an optional parameter. A regular expression must be included in double quotes. If you specify an empty REGEX (`""`), a default REGEX will be used (`"^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$"`). `none` is used if no REGEX is required (the source already provides data in the required format). 
Sample **source** record:
```
{source,{"blackhole_exp","http://data.netlab.360.com/feeds/dga/blackhole.txt","[:AXFR:]","^([A-Za-z0-9][A-Za-z0-9\-\._]+)\t.*:00\t([0-9: -]+)$"}}.
```
### **rpz** record
RPZ term defines a response policy zone.
**rpz** record contains:
- rpz name;
- SOA refresh time in seconds;
- SOA update retry time in seconds;
- SOA expiration time in seconds;
- SOA NXDomain TTL in seconds;
- Cache. Possible values: ``true`` or ``false``. ``true`` defines that the RPZ should be cached, ``false`` - non cached, live zone sources are downloaded and an RPZ generated by AXFR request. "Live" zones do not support incremental zone transfer. if RPZ feed is not cached anyway it is temporary stored into a hot cache. In case if a request timeouts from a client, we will be able to respond next time. AXFR time will be used to determine cache life;
- Wildcards. Possible values: ``true`` or ``false``. Defines if wildcard rules should be generated;
- Action. Supported actions: ``nxdomain``, ``nodata``, ``passthru``, ``drop``, ``tcp-only`` and list of local records ``[{"redirect_domain","example.com"}, {"redirect_ip","127.0.0.1"}, {"local_aaaa","fe80::1"}, {"local_a","127.0.0.1"}, {"local_cname","www.example.com"}, {"local_txt","Text Record"}]``. ``redirect_domain`` is an alias for ``local_cname``. ``redirect_ip`` is an alias for ``local_a``, ``local_aaaa``;
- List of TSIG keys;
- Type of IOCs used in the RPZ: ``mixed``, ``fqdn``, ``ip``. It is used for optimisation. 
- Full zone update time in seconds (AXFR Time). Full Zone update and rebuild if MD5 for IOCs is different;
- Incremental zone update time  (IXFR Time). Sources should support incremental updates. "0" means no incremental zone updates;
- List of the sources;
- List of DNS servers (IP addresses) which should be notified on an RPZ updates;
- List of whitelists.

```
{rpz,{"zone_name",soa_refresh, soa_update_retry,soa_expire,soa_nxdomain_ttl,"cache","wildcards","action",["key1","key2"],"Zone_type",AXFT_Time, IXFR_Time,["source1","source2"],["notify_ip1","notify_ip2"],["whitelist_1","whitelist_2"]}}.

{rpz,{"mixed.ioc2rpz",7202,3600,2592000,7200,"true","true","passthru",["dnsproxykey_1","dnsproxykey_2"],"mixed",86400,3600,["small_ioc","blackhole","bot.list"],[],["whitelist_1","whitelist_2"]}}.
```
## Sample configuration file
```
{srv,{"ns1.rpz-proxy.com","support.rpz-proxy.com",["dnsmkey_3"]}}.

{key,{"dnsproxykey_1","md5","Hbxw9kzCdDp5XgWSWT/5OfRc1+jDIaSvFjpbv/V3IT2ah6xUfLGFcoA7cCLaPh40ni9nvmzlAArj856v3xEnBw=="}}.
{key,{"dnsproxykey_2","sha512","03uuaGl9kqfenjRgIeCv6e29lVvMwviB1+cDX1I0jcVOcTU4jWFwRkfo3ULRMD+NGDfwzYvXkJ94FNEaAW4vzw=="}}.
{key,{"dnsmkey_3","sha512","03uuaGl9kqfenjRgIeCv6e29lVvMwviB1+cDX1I0jcVOcTU4jWFwRkfo3ULRMD+NGDfwzYvXkJ94FNEaAW4vzw=="}}.

{whitelist,{"whitelist_1","file:cfg/whitelist1.txt",none}}.
{whitelist,{"whitelist_2","file:cfg/whitelist2.txt",""}}.

{source,{"small_ioc","file:cfg/small_ioc.txt","[:AXFR:]","^(?!host)(?!ip)\"?\'?([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$"}}.
{source,{"dns-bh","http://mirror1.malwaredomains.com/files/spywaredomains.zones","[:AXFR:]","^zone \"([A-Za-z0-9\-\._]+)\".*$"}}.
{source,{"cryptolocker","http://data.netlab.360.com/feeds/dga/cryptolocker.txt","[:AXFR:]","^(?!host)(?!ip)\"?\'?([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$"}}.
{source,{"tor-exit","https://torstatus.blutmagie.de/ip_list_exit.php/Tor_ip_list_EXIT.csv","[:AXFR:]",none}}.

{rpz,{"localdata.ioc2rpz",7202,3600,2592000,7200,"false","true",[{"local_aaaa","fe80::1"},{"local_a","127.0.0.1"},{"local_a","127.0.0.2"},{"local_a","127.0.0.3"},{"local_a","127.0.0.4"},{"local_cname","www.example.com"},{"local_txt","Text Record www.example.com"},{"local_txt","Text Record 2"}],["dnsproxykey_1", "dnsproxykey_2"],"mixed",30,30,["small_ioc"],[],["whitelist_1","whitelist_2"]}}.
{rpz,{"dga.ioc2rpz",7202,3600,2592000,7200,"true","true","nodata",["dnsproxykey_1","dnsproxykey_2"],"fqdn",172800,3600,["cryptolocker"],[],[]}}.
{rpz,{"mixed.ioc2rpz",7202,3600,2592000,7200,"true","true","passthru",["dnsproxykey_1", "dnsproxykey_2"],"mixed",86400,3600,["small_ioc","dns-bh","cryptolocker"],[],["whitelist_1","whitelist_2"]}}.
{rpz,{"dns-bh.ioc2rpz",7202,3600,2592000,7200,"true","true","nxdomain",[],"mixed",86400,3600,["dns-bh"],[],["whitelist_1","whitelist_2"]}}.
{rpz,{"tor-exit-ip.ioc2rpz",7202,3600,2592000,7200,"false","true","nxdomain",["dnsproxykey_1","dnsproxykey_2"],"ip",172800,0,["tor-exit"],[],[]}}.
```

## Constants - ioc2rpz.hrl


## How the AXFR (full) and IXFR (incremental) caches are updated
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

## Hot cache
IXFR updates are not cached in the hot cache

## TODO features
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
- [ ] (*) FDateTime,ToDateTime,FDateTimeZ,ToDateTimeZ + support them for AXFR  
[:FDateTime:] = "2017-10-13 13:13:13", [:FDateTimeZ:] = "2017-10-13T13:13:13Z"  
[:ToDateTime:] = "2017-10-13 13:13:13", [:ToDateTimeZ:] = "2017-10-13T13:13:13Z"
- [ ] (*) Docker container
- [ ] (*) Documentation
- [ ] Check if RPZs are propertly configured.
- [ ] Add source RPZ
- [ ] Add source SQL
- [ ] Mnesia for storage (and auto creation)
- [ ] Distributed configuration based on mnesia
- [ ] Wait for a remote server confirms receiving a notification
- [ ] Additional local records: ptr, srv, mx etc
- [ ] An action per source: {"",action,locdata} //default action ,{"source_name",action,locdata}

## Other/optimization TODO
- [ ] (*) Do not cache expired IOCs if ExpDateTime<Serial_IXFR / update ExpDateTime if exists
- [ ] (*) Check zones IXFR update from multiple sources
- [ ] (1) Clean up the code & add comments
- [ ] (1) EDNS0 Support: DNS Cookie, edns-tcp-keepalive, NSID
- [ ] (2) IOC to lowercase - check memory usage impact (in ioc2rpz_conn)
- [ ] (3) UDP under supervisor
- [ ] (3) Memory optimization for huge zones (erl -pa ebin +MEas bf ?????)
- [ ] (3) Share IOC between the feeds in IXFR table
- [ ] saveZones - doesn't correctly save zones if there a lot of updates. Save strategy based on update size and time.

## TODO Bugs
- [ ] (*) Sample zone - fix redirect_domain, redirect_ip


## Free threat intel
- [DNS-BH – Malware Domain Blocklist by RiskAnalytics](http://www.malwaredomains.com/)
- [Malware DGA](http://data.netlab.360.com)
- [Tor Exit Nodes](https://torstatus.blutmagie.de/ip_list_exit.php/Tor_ip_list_EXIT.csv)

## References
- [Domain Name System (DNS) IANA Considerations](https://tools.ietf.org/html/rfc6895)
- [Domain Names - Implementation and Specification](https://tools.ietf.org/html/rfc1035)
- [Incremental Zone Transfer in DNS](https://tools.ietf.org/html/rfc1995)
- [DNS Response Policy Zones (RPZ)](https://tools.ietf.org/html/draft-ietf-dnsop-dns-rpz-00) + [vixie](https://tools.ietf.org/html/draft-vixie-dns-rpz-02)
- [Secret Key Transaction Authentication for DNS (TSIG)](https://tools.ietf.org/html/rfc2845)
- [HMAC: Keyed-Hashing for Message Authentication](https://tools.ietf.org/html/rfc2104)
- [HMAC SHA TSIG Algorithm Identifiers](https://tools.ietf.org/html/rfc4635)
- [DNS Transport over TCP - Implementation Requirements](https://tools.ietf.org/html/rfc5966)
- [A Mechanism for Prompt Notification of Zone Changes (DNS NOTIFY)](https://tools.ietf.org/html/rfc1996)
- [Extension Mechanisms for DNS (EDNS(0))](https://tools.ietf.org/html/rfc6891) + [ENDS Option Codes](https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-11)
- [Domain Name System (DNS) Cookies](https://tools.ietf.org/html/rfc7873)

# License
Copyright 2017 Vadim Pavlov pvm(dot)del[at]gmail[.]com

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License.
You may obtain a copy of the License at  
  
    http://www.apache.org/licenses/LICENSE-2.0  
  
Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
