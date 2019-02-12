%Copyright 2017-2018 Vadim Pavlov ioc2rpz[at]gmail[.]com
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

%ioc2rpz params 
-define(IOCperProc,10000). %IOCs per spawn process

-define(MGMToDNS,true). %Defines if management ioc2rpz over DNS is enabled
-define(DBStorage,ets). %Defines DBStorage to use. CFG and HotCache are always ETC (may be will be switched to MAP, need profiling)
-define(SaveETS,false). % Save DB into files if DB is ETS.
-define(Port,53). %DNS Port
-define(TTL,900). %Default record TTL


%-define(logTS, true). % Log timestamps (comment or uncomment)


%%%Optimization
-define(DNSPktMax,16383). %Max DNS packet size. DNS Label Zip is available up to 16384 bytes 65000/max
-define(Compression,6). % 0 - no compression, 9 - highest, 6 - default do it depending on the list/bin size. Used to store zones in cache
-define(ZoneRefTime,60000). %Zone refresh check interval in milliseconds
-define(TCPTimeout,3000). %TCP timeout in milliseconds
-define(HotCacheTime,900). %900 Time to cache IOCs/Records/Pkts in the hot cache. More usefull for online rpz.
-define(HotCacheTimeIXFR,0). %Time to cache IXFR IOCs in a hot cache. By default it is cached for a minute because of curr_serial_60.
-define(ShellMaxRespSize,2*1024*1024*1024). %Maximum response size for shell source


%%%%%%
%%%%%% Do not modify any settings below the line
%%%%%%
-define(ioc2rpz_ver, "0.9.1-2019021101").

-define(ZNameZip,16#c00c:16). %Zone name/original fqdn from a request is always at byte 10 in the response
-define(MaxZipPSize,16#3FFF:16). %Max packet size to zip DNS labels

% Log timestamps
-ifdef(logTS).
-define(addTS(Dest),(fun() ->
		{{Y,M,D},{HH,MM,SS}}=calendar:local_time(),io:fwrite(Dest,"~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ",[Y,M,D,HH,MM,SS])
	end)()).
-else.
-define(addTS(Dest),true).
-endif.

%DNS Response codes
-define(NOERROR,0).
-define(FORMERR,1).
-define(SERVFAIL,2).
-define(NXDOMAIN,3).
-define(NOTIMP,4).
-define(REFUSED,5).
-define(NOTAUTH,9).

%TSIG Errors
-define(TSIG_BADSIG,16).
-define(TSIG_BADKEY,17).
-define(TSIG_BADTIME,18).

%DNS Request Class
-define(C_IN,1).
-define(C_CHAOS,3).
-define(C_ANY,255).

%DNS Request/Record Type
-define(T_A,1).
-define(T_NS,2).
-define(T_CNAME,5).
-define(T_SOA,6).
-define(T_TXT,16).
-define(T_AAAA,28).
-define(T_OPT,41).
-define(T_IXFR,251).
-define(T_AXFR,252).
-define(T_ANY,255).

-define(RT_TSIG,250).

%DNS Operation
-define(OP_QUERY,0:4).
-define(OP_NOTIFY,4:4).

%DNS Resourse Records
-record(dns_RR, {name, type, class, ttl, rdlength, rdata}).
-record(dns_TSIG_RR, {name, type, class, ttl, rdlength, alg, alg_str, key, time, fudge, mac_len, mac, oid, error, olen, odata, time_only}).
-record(dns_SOA_RR, {name, type, class, ttl, rdlength, mname, rname, serial, refresh, retry, expire, minimum}).

%State record
-record(state, {socket, socket6, params}).

%Protocol udp/tcp + qname, qtype, qclass, keyname
-record(proto, {proto,rip,rport, qname, qtype, qclass, keyname}).

%Config params
-record(srv, {server,email,mkeys,acl}).
-record(key, {name,alg,key,name_bin}).
%SOA timers refresh, retry, expiration, neg_ttl
%status: notready, updating, ready
%serial_ixfr - minimum serial for ixfr - first ixfr update after axfr
-record(rpz, {rpzid, zone, zone_str, soa_timers, cache, wildcards, notify, action, akeys, ioc_type, axfr_time, ixfr_time, sources, status, serial, serial_new, serial_ixfr, notifylist, whitelist, ioc_md5, update_time, ixfr_update_time, pid}).
-record(source, {name, axfr_url, ixfr_url, regex}).
