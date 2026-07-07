%Copyright 2017-2019 Vadim Pavlov ioc2rpz[at]gmail[.]com
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

%%%===================================================================
%%% ioc2rpz Application Parameters
%%%
%%% Core configuration constants that control the server's behavior,
%%% network ports, storage backends, and operational defaults.
%%%===================================================================

%% Number of IOC (Indicator of Compromise) entries processed per spawned
%% worker process during zone builds. Controls parallelism granularity
%% when constructing RPZ zone data from IOC sources.
-define(IOCperProc,10000).

%% Enables DNS-based management interface (e.g., zone reload triggers
%% via specially crafted DNS queries). Set to `false` to disable.
-define(MGMToDNS,true).

%% Backend storage engine for zone data and IOC caches.
%% Currently only `ets` (Erlang Term Storage) is supported.
%% Note: cfg_table and rpz_hotcache_table always use ETS regardless.
-define(DBStorage,ets).

%% Whether to persist ETS tables to disk files on shutdown.
%% Only relevant when ?DBStorage is `ets`.
-define(SaveETS,false).

%% Default listening ports for each protocol.
%% ?Port      — Standard DNS over UDP/TCP (RFC 1035)
%% ?PortTLS   — DNS over TLS / DoT (RFC 7858)
%% ?PortDoH   — DNS over HTTPS / DoH (RFC 8484)
%% ?PortREST  — REST management API (HTTPS)
-define(Port,53).
-define(PortTLS,853).
-define(PortDoH,443).
-define(PortREST,8443).

%% Default TTL (Time To Live) in seconds applied to DNS resource records
%% in RPZ zone responses when no explicit TTL is configured.
-define(TTL,900).

%% Filesystem path to the default configuration file, read at startup
%% via file:consult/1 in ioc2rpz_sup:read_config3/1.
-define(DefConf,"./cfg/ioc2rpz.conf").

%% Filesystem path to the default database directory, used for ETS
%% persistence files when ?SaveETS is true.
-define(DefDB,"./db").

%% Number of retry attempts when an IOC source download fails
%% (HTTP, HTTPS, file, or shell sources). Used in ioc2rpz_conn:get_ioc/3.
-define(Src_Retry,3).

%% Delay in seconds between source download retry attempts.
-define(Src_Retry_TimeOut,3).

%% Uncomment to prepend timestamps to log output.
%% When defined, ?addTS/1 writes "YYYY-MM-DD HH:MM:SS " before log lines.
%-define(logTS, true).

%% Enables debug-level log messages via ?logDebugMSG/2 macro.
%% When defined, debug messages are routed to ioc2rpz_fun:logMessage/2.
%% Comment out to suppress debug output in production.
-define(debug, true).

%% The virtual sample RPZ zone name used for built-in demonstration data.
%% This zone is not stored in cfg_table; it is handled by hardcoded logic
%% in ioc2rpz:send_sample_zone/9 for AXFR requests.
-define(ioc2rpzSampleRPZ,"sample-zone.ioc2rpz").

%% TLS protocol version(s) for DoT and REST HTTPS listeners.
%% Accepted values: 'tlsv1.2', 'tlsv1.3', 'tlsv1.2-1.3'.
%% Passed to ioc2rpz_fun:get_cipher_suites/1 to select cipher suites.
-define(TLSVersion,'tlsv1.2-1.3').

%%%===================================================================
%%% Performance & Optimization Constants
%%%===================================================================

%% Maximum DNS packet size in bytes for label compression (zip).
%% DNS name compression pointers use 14-bit offsets, limiting the
%% addressable range to 16384 bytes. Responses beyond this size
%% cannot use label compression for new names.
-define(DNSPktMax,16383).

%% Zlib compression level for cached zone data stored in ETS.
%% 0 = no compression, 1 = fastest, 6 = default, 9 = best compression.
%% Applied via term_to_binary/2 when writing zone packets to cache.
-define(Compression,6).

%% Interval in milliseconds between periodic zone refresh checks.
%% A timer fires every ?ZoneRefTime ms to call update_all_zones/1,
%% which checks each RPZ zone's axfr_time/ixfr_time against the
%% current time and triggers updates for expired zones.
-define(ZoneRefTime,60000).

%% TCP socket receive timeout in milliseconds. Applied to accepted
%% TCP connections to prevent workers from waiting indefinitely for
%% client data after the initial connection.
-define(TCPTimeout,3000).

%% Duration in seconds that individual IOC records and pre-built
%% response packets are cached in rpz_hotcache_table (ETS).
%% Primarily useful for "online" RPZ zones that serve live queries.
%% After this time, cached entries are considered stale on read.
-define(HotCacheTime,900).

%% Duration in seconds for caching IXFR (incremental transfer) IOC
%% entries in the hot cache. Default 0 means IXFR data relies on
%% curr_serial_60 (1-minute granularity serial) for freshness.
-define(HotCacheTimeIXFR,0).

%% Maximum allowed response size in bytes from shell-type IOC sources.
%% Prevents runaway shell commands from consuming excessive memory.
%% Default: 2 GiB.
-define(ShellMaxRespSize,2*1024*1024*1024).

%% Maximum time in milliseconds allowed for a single IOC source/feed
%% download operation. If the download exceeds this duration, it is
%% interrupted. Default: 5 minutes.
-define(SourcePullTimeout, 5 * 60 * 1000).

%%%===================================================================
%%% Internal Constants — Do Not Modify
%%%===================================================================

%% Application version string: "major.minor.patch.build-YYYYMMDDNN"
-define(ioc2rpz_ver, "1.4.0.1-2026070701").

%% DNS label compression pointer for the query name (QNAME) in responses.
%% In a standard DNS response, the original QNAME from the question section
%% starts at byte offset 12 (0x0C). The compression pointer 0xC00C encodes
%% this as a 2-byte pointer (top 2 bits = 11, offset = 12).
%% ?ZNameZip  — 16-bit binary form for use in binary construction
%% ?ZNameZipN — numeric value for use in pattern matching / arithmetic
-define(ZNameZip,16#c00c:16).
-define(ZNameZipN,16#c00c).

%% Maximum packet size (in bytes) within which DNS label compression
%% pointers are valid. Compression pointers use 14-bit offsets, so
%% the maximum addressable position is 0x3FFF = 16383.
-define(MaxZipPSize,16#3FFF:16).

%%%===================================================================
%%% Logging Macros
%%%===================================================================

%% Conditional timestamp macro for log output.
%% When `logTS` is defined, writes a formatted local timestamp
%% ("YYYY-MM-DD HH:MM:SS ") to the given output destination.
%% When undefined, evaluates to `true` (no-op).
-ifdef(logTS).
-define(addTS(Dest),(fun() ->
		{{Y,M,D},{HH,MM,SS}}=calendar:local_time(),io:fwrite(Dest,"~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ",[Y,M,D,HH,MM,SS])
	end)()).
-else.
-define(addTS(Dest),true).
-endif.

%% Inline if-then-else helper. Evaluates `Cond` and returns `True` or
%% `False` accordingly. Used throughout the codebase for concise
%% conditional expressions in binary construction and list operations.
-define(iif(Cond,True,False),(case Cond of true -> True; false -> False end)).

%% Conditional debug logging macro.
%% When `debug` is defined, routes messages to ioc2rpz_fun:logMessage/2.
%% When undefined, evaluates to `true` (no-op) to eliminate debug overhead.
-ifdef(debug).
-define(logDebugMSG(Message, Vars),ioc2rpz_fun:logMessage(Message, Vars)).
-else.
-define(logDebugMSG(Message, Vars),true).
-endif.

%%%===================================================================
%%% DNS Response Codes (RCODE) — RFC 1035 §4.1.1, RFC 2136
%%%
%%% 4-bit field in the DNS header indicating the outcome of a query.
%%% Used in response construction throughout ioc2rpz.erl.
%%%===================================================================

-define(NOERROR,0).   %% No error; query completed successfully
-define(FORMERR,1).   %% Format error; server couldn't interpret the query
-define(SERVFAIL,2).  %% Server failure; internal error processing the query
-define(NXDOMAIN,3).  %% Non-existent domain; the queried name does not exist
-define(NOTIMP,4).    %% Not implemented; server doesn't support the query type
-define(REFUSED,5).   %% Refused; server refuses to perform the operation
-define(NOTAUTH,9).   %% Not authorized; used for TSIG authentication failures

%%%===================================================================
%%% TSIG Error Codes — RFC 2845 §3
%%%
%%% Extended RCODE values specific to TSIG (Transaction Signature)
%%% authentication. Returned in TSIG error responses when zone transfer
%%% or query authentication fails in ioc2rpz:validate_REQ/9.
%%%===================================================================

-define(TSIG_BADSIG,16).  %% TSIG signature verification failed (bad MAC)
-define(TSIG_BADKEY,17).  %% TSIG key not recognized by the server
-define(TSIG_BADTIME,18). %% TSIG timestamp outside acceptable fudge window

%%%===================================================================
%%% DNS Query Classes — RFC 1035 §3.2.4
%%%
%%% The CLASS field in DNS questions and resource records.
%%% Used in query parsing (parse_dns_request) and record construction.
%%%===================================================================

-define(C_IN,1).     %% Internet class — standard DNS queries
-define(C_CHAOS,3).  %% Chaosnet class — used for server info queries (e.g., version.bind)
-define(C_ANY,255).  %% Any class — matches all classes in queries

%%%===================================================================
%%% DNS Record/Query Types — RFC 1035, RFC 3596, RFC 6891, RFC 1995/1996
%%%
%%% The TYPE field identifying the kind of DNS resource record or query.
%%% Used extensively in query parsing, response construction, and zone
%%% transfer logic in ioc2rpz.erl and ioc2rpz_db.erl.
%%%===================================================================

-define(T_A,1).      %% IPv4 address record (A)
-define(T_NS,2).     %% Authoritative name server record (NS)
-define(T_CNAME,5).  %% Canonical name / alias record (CNAME)
-define(T_SOA,6).    %% Start of Authority record — zone metadata, serial, timers
-define(T_TXT,16).   %% Text record — used for RPZ TXT-based actions
-define(T_AAAA,28).  %% IPv6 address record (AAAA)
-define(T_OPT,41).   %% EDNS0 pseudo-record (OPT) — RFC 6891, carries extended options
-define(T_IXFR,251). %% Incremental zone transfer request (IXFR) — RFC 1995
-define(T_AXFR,252). %% Full zone transfer request (AXFR) — RFC 5936
-define(T_ANY,255).  %% Wildcard query type — matches any record type

%% TSIG pseudo-record type used in TSIG authentication processing.
%% Not a standard QTYPE; used internally to identify TSIG RRs appended
%% to DNS messages for transaction signing (RFC 2845).
-define(RT_TSIG,250).

%% Classic DNS UDP message size limit (RFC 1035 §4.2.1). A UDP response that
%% would exceed this is truncated and the TC bit is set so the client retries
%% over TCP. Also the default #proto.edns_size for non-EDNS0 requests.
-define(DNS_UDP_SIZE,512).

%% Upper bound applied to a requestor's advertised EDNS0 (RFC 6891) UDP payload
%% size. A client OPT record may advertise a large buffer; ioc2rpz honours it up
%% to this cap so a single large UDP datagram cannot be abused for amplification
%% and stays within common path MTUs / reassembly limits.
-define(EDNS_MAX_UDP_SIZE,4096).

%%%===================================================================
%%% DNS Operation Codes — RFC 1035 §4.1.1
%%%
%%% 4-bit OPCODE field in the DNS header specifying the query type.
%%% Encoded as `Value:4` for direct binary pattern matching.
%%%===================================================================

-define(OP_QUERY,0:4).   %% Standard query (QUERY)
-define(OP_NOTIFY,4:4).  %% Zone change notification (NOTIFY) — RFC 1996

%%%===================================================================
%%% DNS Resource Record Definitions
%%%
%%% Erlang records representing parsed DNS resource records used
%%% throughout query processing, zone transfers, and response building.
%%%===================================================================

%% Generic DNS Resource Record (RR) — RFC 1035 §4.1.3
%% Represents a single resource record in a DNS message.
%%   name     — domain name (binary, wire format or string)
%%   type     — record type (e.g., ?T_A, ?T_SOA)
%%   class    — record class (e.g., ?C_IN)
%%   ttl      — time to live in seconds
%%   rdlength — length of rdata in bytes
%%   rdata    — record-type-specific data (binary)
-record(dns_RR, {name, type, class, ttl, rdlength, rdata}).

%% TSIG Resource Record — RFC 2845
%% Used for DNS transaction authentication on zone transfers and queries.
%% Parsed from the additional section of signed DNS messages in
%% ioc2rpz:validate_REQ/9 and constructed for signed responses.
%%   name      — TSIG key name (binary)
%%   type      — always ?RT_TSIG (250)
%%   class     — always ?C_ANY (255)
%%   ttl       — always 0
%%   rdlength  — total RDATA length
%%   alg       — algorithm name in wire format (binary)
%%   alg_str   — algorithm name as string (e.g., "hmac-sha256")
%%   key       — shared secret key (binary)
%%   time      — signing timestamp (48-bit seconds since epoch)
%%   fudge     — allowed time skew in seconds (typically 300)
%%   mac_len   — length of the MAC digest
%%   mac       — computed HMAC digest (binary)
%%   oid       — original message ID
%%   error     — TSIG error code (0, ?TSIG_BADSIG, ?TSIG_BADKEY, ?TSIG_BADTIME)
%%   olen      — other data length
%%   odata     — other data (e.g., server time on BADTIME errors)
%%   time_only — pre-computed binary of time+fudge for MAC calculation
-record(dns_TSIG_RR, {name, type, class, ttl, rdlength, alg, alg_str, key, time, fudge, mac_len, mac, oid, error, olen, odata, time_only}).

%% SOA Resource Record — RFC 1035 §3.3.13
%% Represents the Start of Authority record for a zone, containing
%% zone metadata and timing parameters for secondary DNS servers.
%%   name      — zone name
%%   type      — always ?T_SOA (6)
%%   class     — always ?C_IN (1)
%%   ttl       — record TTL
%%   rdlength  — RDATA length
%%   mname     — primary nameserver name
%%   rname     — responsible person email (DNS-encoded)
%%   serial    — zone serial number (used for transfer decisions)
%%   refresh   — seconds between SOA checks by secondaries
%%   retry     — seconds between retry after failed refresh
%%   expire    — seconds before zone is considered expired
%%   minimum   — negative caching TTL
-record(dns_SOA_RR, {name, type, class, ttl, rdlength, mname, rname, serial, refresh, retry, expire, minimum}).

%%%===================================================================
%%% gen_server State Record
%%%
%%% Carried through the gen_server callback lifecycle in TCP/TLS
%%% worker processes (ioc2rpz.erl), UDP listener (ioc2rpz_udp.erl),
%%% REST API handler (ioc2rpz_rest.erl), and DoH handler (ioc2rpz_doh.erl).
%%%===================================================================

%% Worker process state for TCP/TLS accept workers and protocol handlers.
%%   socket — the listen socket (before accept) or accepted connection socket
%%   tls    — `yes` or `no` indicating whether this is a TLS connection
%%   params — protocol-specific parameters:
%%            for TCP/TLS workers: [Pid, Proc] where Proc identifies the
%%            supervisor (e.g., tls_sup, tcp_sup) for spawning replacements
%%            for UDP: listener parameters
%%   op     — operation atom for REST/DoH handlers (e.g., catch_all, dns_query)
%%   user   — authenticated user identifier (set after REST API auth)
%%   lang   — reserved for future localization support
-record(state, {socket, tls, params, op, user, lang}).

%%%===================================================================
%%% Protocol Context Record
%%%
%%% Captures the full context of an incoming DNS request for use in
%%% query processing, logging, and response construction.
%%%===================================================================

%% DNS request protocol context, built during query parsing in
%% ioc2rpz:parse_dns_request/3 and passed to response functions.
%%   proto   — transport protocol atom: `udp` or `tcp`
%%   tls     — `yes` or `no` for TLS-wrapped connections
%%   rip     — remote (client) IP address tuple
%%   rport   — remote (client) port number
%%   qname   — queried domain name (binary, wire format)
%%   qtype   — query type (e.g., ?T_SOA, ?T_AXFR)
%%   qclass  — query class (e.g., ?C_IN)
%%   keyname — TSIG key name used for authentication (or `undefined`)
%%   edns_size — requestor's advertised UDP payload size (RFC 6891 EDNS0). Set
%%               from the request's OPT pseudo-record when present, otherwise the
%%               classic DNS limit `?DNS_UDP_SIZE` (512). Used by send_dns_udp/6
%%               to decide when a UDP response must be truncated with the TC bit.
-record(proto, {proto, tls, rip, rport, qname, qtype, qclass, keyname, edns_size = ?DNS_UDP_SIZE}).

%%%===================================================================
%%% Configuration Records
%%%
%%% Parsed from the configuration file (ioc2rpz.conf) by
%%% ioc2rpz_sup:read_config3/8 and stored in the `cfg_table` ETS table.
%%%===================================================================

%% TLS certificate configuration for DoT and REST HTTPS listeners.
%%   certfile   — path to the server certificate PEM file
%%   keyfile    — path to the private key PEM file
%%   cacertfile — path to the CA certificate chain PEM file (optional)
-record(cert, {certfile,keyfile,cacertfile}).

%% Server-level configuration. Stored in cfg_table as {[srv], Srv}.
%%   server     — server hostname / MNAME for SOA records
%%   email      — responsible person email / RNAME for SOA records
%%   mkeys      — list of management key names authorized for DNS mgmt
%%   acl        — access control list (IP ranges) for REST API and DNS management commands
%%   cert       — #cert{} record with TLS certificate paths
%%   max_ioc    — global maximum IOC count limit (optional)
%%   key_groups — list of key group names for access control
%%   track_sources — server-level global default for IOC source attribution:
%%                   `off | auto | on`. Applied to any feed whose own
%%                   #rpz.track_sources is `undefined`. Defaults to `off`, so
%%                   existing configs (which specify no value) keep the current
%%                   behavior of tracking disabled.
-record(srv, {server,email,mkeys,acl,cert, max_ioc, key_groups, track_sources = off}).

%% TSIG key definition. Stored in cfg_table as {[key, Name], ...}.
%% Used for authenticating zone transfers and DNS management requests.
%%   name       — key name string (e.g., "transfer-key")
%%   alg        — HMAC algorithm string (e.g., "hmac-sha256")
%%   key        — base64-decoded shared secret (binary)
%%   name_bin   — key name in DNS wire format (binary)
%%   key_groups — list of key group names this key belongs to
-record(key, {name,alg,key,name_bin, key_groups}).

%% Key group definition for grouping multiple TSIG keys under a
%% single access control policy. Referenced by #rpz.key_groups.
%%   name — group name string
%%   keys — list of key names belonging to this group
-record(key_group, {name,keys}).

%% RPZ (Response Policy Zone) definition. The central record for zone
%% management. Stored in cfg_table as {[rpz, ZoneBinary], ZoneBinary, RPZ}.
%%
%%   rpzid              — unique zone identifier (internal)
%%   zone               — zone name in DNS wire format (binary)
%%   zone_str           — zone name as a human-readable string
%%   soa_timers         — packed binary <<Refresh:32, Retry:32, Expire:32, MinTTL:32>>
%%   cache              — <<"true">> or <<"false">>; whether to cache zone data in ETS
%%   wildcards          — <<"true">> or <<"false">>; whether to generate wildcard rules
%%   notify             — <<"true">> or <<"false">>; whether to send NOTIFY on updates
%%   action             — RPZ action: <<"nxdomain">>, <<"nodata">>, <<"passthru">>,
%%                        <<"drop">>, <<"tcp-only">>, <<"local">> or local action data
%%   akeys              — list of authorized TSIG key names for this zone
%%   ioc_type           — IOC type filter: <<"mixed">>, <<"ip">>, <<"fqdn">>
%%   axfr_time          — full zone refresh interval in seconds
%%   ixfr_time          — incremental zone refresh interval in seconds
%%   sources            — list of source name strings feeding this zone
%%   status             — zone state atom: `notready`, `updating`, `ready`, `forceAXFR`
%%   serial             — current zone serial number (Unix timestamp based)
%%   serial_new         — serial being built during an update (-1 when idle)
%%   serial_ixfr        — minimum serial for which IXFR deltas are available
%%   notifylist         — list of {udp, IP} tuples for NOTIFY targets
%%   whitelist          — list of whitelist source names applied to this zone
%%   ioc_md5            — MD5 hash of the zone's IOC data (for change detection)
%%   update_time        — timestamp of last completed AXFR update
%%   ixfr_update_time   — timestamp of last completed IXFR update
%%   ixfr_nz_update_time — timestamp of last non-zero IXFR update
%%   pid                — PID of the currently running update process (or undefined)
%%   ioc_count          — number of IOC indicators in the zone
%%   userid             — user restriction identifier (optional)
%%   max_ioc            — per-zone maximum IOC count limit (optional)
%%   key_groups         — list of key group names authorized for this zone
%%   rule_count         — number of DNS rules generated from IOCs
%%   track_sources      — per-feed source-attribution setting:
%%                        `undefined | auto | true | false`. `undefined` ⇒ use the
%%                        server global default (#srv.track_sources). `auto` ⇒ track
%%                        only for multi-source feeds; `true`/`false` ⇒ force on/off.
%%                        Left `undefined` by default so every existing #rpz{...}
%%                        literal keeps compiling and unconfigured feeds inherit the
%%                        global default.
-record(rpz, {rpzid, zone, zone_str, soa_timers, cache, wildcards, notify, action, akeys, ioc_type, axfr_time, ixfr_time, sources, status, serial, serial_new, serial_ixfr, notifylist, whitelist, ioc_md5, update_time, ixfr_update_time, ixfr_nz_update_time, pid, ioc_count, userid, max_ioc, key_groups, rule_count, track_sources = undefined}).

%% IOC Source definition. Represents a feed/data source that provides
%% indicators of compromise. Stored in cfg_table as {[source, Name], Source}.
%%
%%   name             — unique source name string
%%   axfr_url         — URL or path for full data download (HTTP/HTTPS/file:/shell:)
%%   ixfr_url         — URL or path for incremental data download (optional)
%%   regex            — compiled regex for extracting IOCs from raw source data
%%   ioc_count        — number of IOCs last retrieved from this source
%%   userid           — user restriction identifier (optional)
%%   max_ioc          — per-source maximum IOC count limit (optional)
%%   hotcache_time    — custom hot cache TTL override for this source (seconds)
%%   hotcacheixfr_time — custom IXFR hot cache TTL override (seconds)
%%   ioc_type         — IOC type: <<"mixed">>, <<"ip">>, <<"fqdn">>
%%   keep_in_cache    — boolean; if true, source data is preloaded into hot cache
%%   pid              — PID of the currently running download process (or undefined)
-record(source, {name, axfr_url, ixfr_url, regex, ioc_count, userid, max_ioc, hotcache_time, hotcacheixfr_time, ioc_type, keep_in_cache, pid}).

%%%===================================================================
%%% User Restriction Record
%%%===================================================================

%% Per-user resource limits for multi-tenant deployments.
%%   userid  — unique user identifier
%%   max_ioc — maximum number of IOC entries this user can have
%%   max_wl  — maximum number of whitelist entries this user can have
-record(user, {userid,max_ioc,max_wl}).

%%%===================================================================
%%% TCP Send Timeout
%%%===================================================================

%% Timeout in milliseconds for TCP send operations. If a send does not
%% complete within this window, the connection is considered stalled.
%% Used as a socket option for accepted TCP connections.
-define(TCP_SEND_TIMEOUT,5000).

%%%===================================================================
%%% Rate Limiting Constants
%%%
%%% Controls DNS query rate limiting to mitigate abuse and DDoS.
%%% Rate limit state is stored in the ?RATE_LIMIT_TABLE ETS table,
%%% keyed by client IP (or {IP, QName, QType} in current code).
%%% Checked in ioc2rpz:parse_dns_request/3 via ioc2rpz_fun:check_rate_limit/1.
%%%===================================================================

%% ETS table name for storing rate limit entries.
-define(RATE_LIMIT_TABLE, rate_limits).

%% Sliding window duration in milliseconds for rate limit tracking.
%% Requests within this window are counted against the limit.
-define(RATE_LIMIT_WINDOW, 60000).

%% Maximum number of DNS requests allowed per client IP within
%% a single ?RATE_LIMIT_WINDOW period. Requests exceeding this
%% threshold are refused with ?REFUSED response code.
%% Used for the GRANULAR rate-limit bucket keyed by {IP, QName, QType}
%% (provisioned zone + supported QTYPE, and recognized management requests).
-define(MAX_REQUESTS_PER_WINDOW, 6).

%%%===================================================================
%%% Source-Attribution Mask Constants (IOC Source Attribution, R7)
%%%===================================================================

%% Fixnum bit budget for a per-zone positional source mask. Bit `i' of the mask
%% corresponds to the `i'-th source in a zone's #rpz.sources list. While the
%% number of sources stays within this budget the mask is represented as a
%% single small Erlang integer (fixnum); above it, ioc2rpz_fun:mask_repr_for/1
%% selects a binary-bitmap representation so larger feeds can still be tracked
%% (design §8). 63 keeps the integer a fixnum on 64-bit systems (one tag bit).
-define(MaskFixnumBits, 63).

%% Maximum number of requests allowed within a single ?RATE_LIMIT_WINDOW for
%% the AGGREGATE per-IP bucket keyed by {IP}. This bucket counts requests that
%% do not map to a provisioned zone + supported QTYPE (unknown/unprovisioned
%% zones, unsupported query types, wrong class, unrecognized management names).
%% Kept separate from ?MAX_REQUESTS_PER_WINDOW so abusive query-name-variation
%% traffic can be limited independently of legitimate per-zone/per-type traffic.
%% Selected by rate-limit key shape in ioc2rpz_fun:check_rate_limit/1.
-define(MAX_UNKNOWN_REQUESTS_PER_WINDOW, 1).
