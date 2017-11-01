%Copyright 2017 Vadim Pavlov pvm(dot)del[at]gmail[.]com
%
%Licensed under the Apache License, Version 2.0 (the "License");
%you may not use this file except in compliance with the License.
%You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
%Unless required by applicable law or agreed to in writing, software
%distributed under the License is distributed on an "AS IS" BASIS,
%WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%See the License for the specific language governing permissions and
%limitations under the License.

%IOC2RPZ headers

-define(ioc2rpz_ver, 2017102101).
-define(Port,53).
-define(TTL,900).
-define(DNSPktMax,16384). %Max DNS packet size. DNS Label Zip is available up to 16384 bytes 65000/max
-define(Compression,6). % 0 - no compression, 9 - highest, 6 - default do it depending on the list/bin size.
-define(SaveETS,true). % Save DB into files if DB is ETS.
-define(ZoneRefTime,120000). %300000 Zone refresh check interval

-define(TCPTimeout, 3000). %in milliseconds

%hosts.ioc2rpz - 100000 - 16384
%Query time: 10393 msec
%199791 records (messages 275, bytes 4540252)
%hosts.ioc2rpz - 100000 - 65000 - 18% more data, 4 times less packets
%Query time: 12492 msec
%199791 records (messages 85, bytes 5494450)
-define(HotCacheTime,30). %900 Time to cache IOCs/Records/Pkts in a hot cache. More usefull for online rpz.

-define(MaxZipPSize,16#3FFF:16). %Max packet size to zip DNS labels
-define(DBStorage,ets). %Defines DBStorage to use. CFG and HotCache are always ETC (may be will be switched to MAP, need profiling)

-define(ZNameZip,16#c00c:16). %Zone name/original fqdn from a request is always at byte 10 in the response

-define(NOERROR,0).
-define(FORMERR,1).
-define(SERVFAIL,2).
-define(NXDOMAIN,3).
-define(NOTIMP,4).
-define(REFUSED,5).
-define(NOTAUTH,9).

-define(TSIG_BADSIG,16).
-define(TSIG_BADKEY,17).
-define(TSIG_BADTIME,18).


-define(C_IN,1).
-define(C_CHAOS,3).
-define(C_ANY,255).

-define(T_A,1).
-define(T_NS,2).
-define(T_CNAME,5).
-define(T_SOA,6).
-define(T_TXT,16).
-define(T_AAAA,28).
-define(T_IXFR,251).
-define(T_AXFR,252).
-define(T_ANY,255).

-define(RT_TSIG,250).

-define(OP_QUERY,0:4).
-define(OP_NOTIFY,4:4).

-record(dns_RR, {name, type, class, ttl, rdlength, rdata}).
-record(dns_TSIG_RR, {name, type, class, ttl, rdlength, alg, alg_str, key, time, fudge, mac_len, mac, oid, error, olen, odata, time_only}).
-record(dns_SOA_RR, {name, type, class, ttl, rdlength, mname, rname, serial, refresh, retry, expire, minimum}).


%State record
-record(state, {socket,params}).

%Protocol udp/tcp
-record(proto, {proto,rip,rport}).

%Config
-record(srv, {server,email,mkeys}).
-record(key, {name,alg,key}).
%SOA timers refresh, retry, expiration, neg_ttl
%status: notready, updating, ready
%serial_ixfr - minimum serial for ixfr - first ixfr update after axfr
-record(rpz, {rpzid, zone, zone_str, soa_timers, cache, wildcards, notify, action, akeys, ioc_type, axfr_time, ixfr_time, sources, status, serial, serial_new, serial_ixfr, notifylist, whitelist, ioc_md5, update_time, ixfr_update_time}).
-record(source, {name, axfr_url, ixfr_url, regex}).
