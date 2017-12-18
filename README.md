#  ioc2rpz makes your threat intelligence actionable
According with [Cisco's 2016 annual security report](https://github.com/Homas/ioc2rpz/blob/master/Cisco_2016_ASR.pdf) 91.3% of malware use DNS in the following ways:
- to gain command and control;
- to exfiltrate data;
- to redirect traffic.
<p align="center"><img src="https://github.com/Homas/ioc2rpz/blob/master/DNS_Malware.png"></p>
With introduction of Response Policy Zones in the BIND nameserver 9.8 it is became a simple task to monitor and contain malware on DNS layer. A DNS server can handle millions of indicators but there is no automated and efficient way to maintain response policy zones on primary DNS servers.

ioc2rpz is a specially built DNS server which natively support different file formats, protocols and transforms threat intelligence into actionable RPZ feeds. The feeds can be used on any DNS server which support RPZ.  

<b>ioc2rpz is a place where threat intelligence meets DNS.</b>

## Overview
ioc2rpz transforms IOC feeds into response policy zones (RPZ). You can mix feeds to generate a single RPZ or multiple RPZs. Trusted domains and IPs can be whitelisted. ioc2rpz supports expiration of indicators and accordingly rebuilds zones.  
![Alt ioc2rpz](https://github.com/Homas/ioc2rpz/blob/master/IOC2RPZ.jpg)
The current release supports: local files and files/requests via http/https/ftp protocols. You can use any file format if you can write a REGEX to extract indicators and indicators are separated by newline or/and return carriage chars (/n, /r, /r/n).

## How to use ioc2rpz
You can use ioc2rpz with any DNS server which supports Response Policy Zones e.g. recent versions of ISC BIND. A sample bind's configuration file is provided in the cfg folder.

## ioc2rpz vs ISC BIND vs other DNS:
- ioc2rpz was built to handle RPZ distribution only;
- ioc2rpz supports as many RPZs as you need. ISC BIND supports only 32 zones per DNS view;
- ioc2rpz supports live/non cached zones. It creates zones by an incoming request;
- indicators can be pulled from different sources and via different protocols (e.g. via REST API calls);
- RPZs are automatically updated;
- IOC expiration time is used to remove expired indicators in a timely manner;
- Performance and zone transfer time/size/packets optimizations.

## How to start ioc2rpz service
ioc2rpz by default reads configuration from ./cfg/ioc2rpz.conf, listens on all network interfaces and save DB backup in ./db directory. You can change the default values in the erlang application configuration, which is located in ``ebin/ioc2rpz.app``.  
If you downloaded sources, before running ioc2rpz you have to compile the code. You can do it with the following command: ``erlc -I include/ -o ebin/ src/*.erl``.  
You can start the application by ``sudo erl -pa ebin -eval "application:start(ioc2rpz)" -noshell`` command.  

## Docker container
ioc2rpz is available on the docker hub. Just look for ioc2rpz.
Prerequisites:
- ioc2rpz doesn't contain a configuration file, you need to mount /opt/ioc2rpz/cfg to a directory on a host system with the configuration file (ioc2rpz.conf);
- ioc2rpz use 53/udp and 53/tcp ports. Both ports should be exposed to the host system;
- ioc2rpz saves ETS database into files for faster start up you may mount /opt/ioc2rpz/db to a directory on a host system to preserve DB over restarts;
You can start ioc2rpz with the following command:
```
docker run --mount type=bind,source=/home/ioc2rpz/cfg,target=/opt/ioc2rpz/cfg --mount type=bind,source=/home/ioc2rpz/db,target=/opt/ioc2rpz/db -p53:53 -p53:53/udp ioc2rpz
```
where /home/ioc2rpz/cfg, /home/ioc2rpz/db directories on a host system.

## ioc2rpz management
ioc2rpz supports management over DNS/TCP. The current version of ioc2rpz does not support ACL or a separate management IP. In any case it is highly recommended to create a separate TSIG key which will be used for management only.
Supported actions:
- ioc2rpz current status. Request ``ioc2rpz-status``, class ``CHAOS``, record ``TXT``. e.g.:  
```
dig +tcp -y dnsmkey_1:Hbxw9kzCdDp5XgWSWT/5OfRc1+jDIaSvFjpbv/V3IT2ah6xUfLGFcoA7cCLaPh40ni9nvmzlAArj856v3xEnBw== @127.0.0.1 ioc2rpz-status TXT -c CHAOS
```
- Reload configuration file. RR Name ``ioc2rpz-reload-cfg``, RR Class ``CHAOS``, RR Type ``TXT``
- Full refresh of all zones. RR Name ``ioc2rpz-update-all-rpz``, RR Class ``CHAOS``, RR Type ``TXT``
- Full refresh a zone. RR Name ``zone_name``, RR Class ``CHAOS``, RR Type ``TXT``. E.g. full refresh of ``dga.ioc2rpz`` can be invoked by: 
```
dig +tcp -y dnsmkey_1:Hbxw9kzCdDp5XgWSWT/5OfRc1+jDIaSvFjpbv/V3IT2ah6xUfLGFcoA7cCLaPh40ni9nvmzlAArj856v3xEnBw== @127.0.0.1 dga.ioc2rpz TXT -c CHAOS
```
- Stop ioc2rpz. RR Name ``ioc2rpz-terminate``, RR Class ``CHAOS``, RR Type ``TXT``
- Request a sample zone. RR Name ``sample-zone.ioc2rpz``, RR Class ``IN``, RR Type ``AXFR``

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
- an email address for SOA record (in SOA format);
- list of management TSIG keys (names only). Please refer [the management section](#ioc2rpz-management) for the details.   
Sample **srv** record:  
```
{srv,{"ns1.example.com","support.email.example.com",["dnsmkey_1","dnsmkey_2","dnsmkey_3"]}}.
```
### **key** record
Keys are used for authentication and authorization. It is recommended to use different keys for ioc2rpz management and zones transfers.
**key** record contain:
- TSIG key name;
- algorithm. md5, sha256 and sha512 are supported';
- the key.
Sample **key** record:  
```
{key,{"key_name_1","md5","Hbxw9kzCdDp5XgWSWT/5OfRc1+jDIaSvFjpbv/V3IT2ah6xUfLGFcoA7cCLaPh40ni9nvmzlAArj856v3xEnBw=="}}.
```
dnssec-keygen utility can be used to generate TSIG keys. E.g. the command below will generate a TSIG key 512 bits length with "tsig-key" name using MD5 algorithm, and save the key in the files with extensions "key" and "private". A TSIG the key will be the same in the both files.
```
dnssec-keygen -a HMAC-MD5 -b512 -n USER tsig-key
```
Please refer "dnssec-keygen" documentation for details.
### **whitelist** record
Whitelists are used to prevent possible errors and blocking trusted domains and IP addresses. The whitelisted IOCs are removed from response policy zones. ioc2rpz does check only exact match, so it will not split or discard a network if a whitelisted IP address is included into a blocked subnet and vice versa. A white list is a text file of feed of text data. Indicators should be separated by newline characters (/n,/r or both /n/r).  Whitelists must contain valid FQDNs and/or IP addresses. ioc2rpz supports unlimited count of indicators.
**whitelists** record contains:
- whitelist name;
- whitelist path. URLs(http/https/ftp) and local files are supported. Prefix "file:" is used for local files;
- REGEX which is used to extract indicators. A regular expression must be included in double quotes. If you specify an empty REGEX (`""`), a default REGEX will be used (`"^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$"`). `none` is used if no REGEX is required (the source already provides data in the required format).
Sample **whitelist** record:
```
{whitelist,{"whitelist_1","file:cfg/whitelist1.txt",none}}.
```
### **source** record
A source is a feed of malicious indicators. FQDNs, IPv4 and IPv6-addresses are supported. A source is a text file of feed of text data. Indicators should be separated by newline/carriage return characters (/n,/r or both /r/n). ioc2rpz supports unlimited count of indicators.
**source** record contains:
- source name;
- source path for full source transfer (AXFR). URLs(http/https/ftp) and local files are supported. Prefix "file:" is used for local files;
- source path for incremental source transfer (IXFR). AXFR,IXFR paths support keywords to shorten URLs and provide zone update timestamps:
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
- Type of IOCs used in the RPZ: ``mixed``, ``fqdn``, ``ip``. It is used for optimization. 
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

## Constants - include/ioc2rpz.hrl
include/ioc2rpz.hrl contains pre-compiled parameters.

Standard parameters:
- ``MGMToDNS`` (true/false) - enabled management over DNS/TCP;
- ``DBStorage`` (ets) - defines DB storage for AXFR and IXFR caches. Current version supports ETS only;
- ``SaveETS`` (true/false) - defines if ETS AXFR/IXFR tables should be saved on disk;
- ``Port`` (numerical value, 1 - 65535) - defines a port on which service is running;
- ``TTL`` (numerical value, in secodns) - default TTL for DNS records/RPZ rules.

Optimization parameters:
- ``DNSPktMax`` (numerical value, 100 - 65535) - maximum packet size. Recommended values:
  - 16384 - minimal zone transfer size;
  - 65535 - minimal count of DNS packets;
- ``Compression`` (numerical value, 0 - 9) - Compression level (0 - no compression, 9 - highest compression). AXFR cache and tables on a disk store compressed data;
- ``ZoneRefTime`` (numerical value, in milliseconds) - defines zone refresh check interval;
- ``TCPTimeout`` (numerical value, in milliseconds) - defines TCP session timeout;
- ``HotCacheTime`` (numerical value, in seconds) - Hot cache time for IOCs, Rules, Packets. Live zones are stored in a hot cache;
- ``HotCacheTimeIXFR`` (numerical value, in seconds) - Hot cache time for IXFR IOCs in a hot cache. By default IXFR indicators are cached for a minute (even if it set to 0) because current serial is always rounded to a previous minute.

## How the AXFR (full) and IXFR (incremental) caches are updated
- AXFR cache always contains prebuilt zones without SOA/NS/TSIG records. Prebuilt means all records are splitted by packets and labels were shortened/zipped.
- If server receive an AXFR request it retrieve packets from the AXFR cache, add SOA/NS records and TSIG if needed.
- AXFR zones update should be considered as a clean up procedure, which should periodically take place. Just to be sure that there is no desynchronization between the sources and the cache.
- For large zones, AXFR updates should be scheduled infrequently to minimize impact on server's performance and amount of transferred data to all clients.
- All changes if it is possible should be done via incremental zone updates. In that case the AXFR cache will be rebuilt only in case if a zone was updated.
- [TODO] Due to an optimization, only last packet will be rebuilt for new IOCs and relevant and accordant packets for the expired IOCs.
- IXFR cache contains only IOCs and expiration dates. [TODO] and packets ID's (to make it possible rebuild the zone fast).
- RPZ record contains current zone Serial and Serial_IXFR. Serial_IXFR serve as a minimum incremental zone serial which is available for an incremental zone transfer.
- IXFR cache is flushed after full zone update (AXFR). Serial_IXFR = Serial. Clients will receive full zone update in any case, this is why it is important to have AXFR zone transfer infrequently.
- When IXFR is updated, AXFR cache must be rebuilt.
- If a zone does not support IXFR updates -> it doesn't saved in the IXFR table.
- Live zones are not cached in the AXFR, IXFR caches but the sources (IOCs) can be cached in the hot cache.

## Hot cache
All IOCs, Rules, Packets including live RPZs are stored in the hot cache. Pre-compiled parameters ``HotCacheTime``, ``HotCacheTimeIXFR`` define storage time.

## TODO features
- [ ] (*) http/https/ftp errors handling - source status in the record. If a source is not available - work w/o it
- [ ] (*) Source based on files check by mod.date and size -> read by chunks
- [ ] RPZ behavior: ignore unreachable sources, use old data for unreachable sources, do not update the zone
- [ ] (*) ACL for MGMT
- [ ] "intellectual" configuration update
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
- [ ] MGMT by a signal
  - [ ] Reload CFG
  - [ ] Refresh a zone
  - [ ] Refresh all zones
  - [ ] Terminate processes/Exit
- [ ] MGMT via DNS move to a separate port/IP
- [ ] (*) FDateTime,ToDateTime,FDateTimeZ,ToDateTimeZ + support them for AXFR  
[:FDateTime:] = "2017-10-13 13:13:13", [:FDateTimeZ:] = "2017-10-13T13:13:13Z"  
[:ToDateTime:] = "2017-10-13 13:13:13", [:ToDateTimeZ:] = "2017-10-13T13:13:13Z"
- [x] (*) Docker container
- [ ] (*) Documentation
- [ ] Check if RPZs are properly configured.
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
- [ ] Possibility to turn off saving ETS on disk

## Free threat intel
- [DNS-BH – Malware Domain Blocklist by RiskAnalytics](http://www.malwaredomains.com/)
- [Malware DGA](http://data.netlab.360.com)
- [Tor Exit Nodes](https://torstatus.blutmagie.de/ip_list_exit.php/Tor_ip_list_EXIT.csv)
- [awesome-threat-intelligence list on github](https://github.com/hslatman/awesome-threat-intelligence)

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
