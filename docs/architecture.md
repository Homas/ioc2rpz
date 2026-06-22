# ioc2rpz Architecture

## System Overview

ioc2rpz is a custom DNS server written in Erlang/OTP that converts Indicators of Compromise (IOCs) from threat intelligence feeds into DNS Response Policy Zones (RPZ). It enables DNS-based threat blocking by serving RPZ zones to DNS resolvers, which can then block or redirect queries for malicious domains and IPs.

The server supports multiple protocols:
- DNS over UDP (port 53)
- DNS over TCP (port 53)
- DNS over TLS / DoT (port 853)
- DNS over HTTPS / DoH (port 443)
- REST management API (port 8443)

IOC sources can be fetched from HTTP/HTTPS/FTP URLs, local files, or shell command output. Sources are parsed using configurable regex patterns, stored in ETS tables, and served as DNS zone transfers (AXFR/IXFR) with TSIG authentication.

Version: 1.3.0.1
License: Apache 2.0

## OTP Supervision Tree

```
ioc2rpz_app (application)
└── ioc2rpz_sup (supervisor, one_for_one, intensity=60, period=3600)
    ├── ioc2rpz_db_sup (gen_server — ETS table heir process)
    ├── ioc2rpz_tcp_sup_v6 (supervisor via ioc2rpz_proc_sup)
    │   └── simple_one_for_one (intensity=1000, period=60): ioc2rpz workers (TCP accept, transient)
    │       └── 5 pre-spawned listeners via empty_listeners/1
    ├── ioc2rpz_udp_sup_v6 (supervisor via ioc2rpz_proc_sup)
    │   └── one_for_one: ioc2rpz_udp worker
    ├── ioc2rpz_tls_sup_v6 (supervisor via ioc2rpz_proc_sup) [if cert configured]
    │   └── simple_one_for_one (intensity=1000, period=60): ioc2rpz workers (TLS accept, transient)
    │       └── 5 pre-spawned listeners via empty_listeners/1
    └── ioc2rpz_rest_tls_sup_v6 (supervisor via ioc2rpz_proc_sup) [if cert configured]
        └── one_for_one: Cowboy HTTPS listener (REST API + DoH routes)
```

### Startup Sequence

1. `ioc2rpz_app:start/2` reads environment variables (IPv4, IPv6, config file path, DB directory)
2. `ioc2rpz_sup:start_ioc2rpz_sup/1` starts the top-level supervisor
3. `ioc2rpz_sup:init/1`:
   - Starts `ioc2rpz_db_sup` (ETS table heir)
   - Calls `ioc2rpz_db:init_db/3` to create ETS tables
   - Creates the `rate_limits` ETS table
   - Reads configuration via `read_config3/1`
   - Starts `inets` and `ssl` applications
   - Loads hot sources into cache
   - Triggers initial zone updates via `update_all_zones(false)`
   - Sets up periodic timers:
     - `load_hotsources/1` every 60 seconds
     - `update_all_zones/1` every 60 seconds
     - `ioc2rpz_fun:cleanup_rate_limit_table/0` every `?RATE_LIMIT_WINDOW` (10 seconds) — removes expired `rate_limits` entries
     - `ioc2rpz_db:cleanup_hotcache/0` every `?HotCacheTime` (900 seconds) — removes expired `rpz_hotcache_table` packet entries
   - Returns child specs for TCP, UDP, TLS, and REST supervisors

### Listener Pool Model

TCP and TLS supervisors use `simple_one_for_one` strategy with intensity `{1000, 60}` (up to 1000 worker restarts per 60 seconds) so that bursts of transient accept/handshake failures are absorbed without crashing the supervisor. On startup, `empty_listeners/1` spawns 5 worker processes per pool. Each worker calls `gen_server:cast(self(), accept)` in `init/1`, which triggers the accept loop. When a connection is accepted, the worker spawns a replacement listener via `ioc2rpz_proc_sup:start_socket/1` before processing the request.

Workers use a `transient` restart type: a worker that exits normally (the expected one-connection-per-worker lifecycle, where the replacement listener is spawned via `start_socket/1` on accept) is not restarted and produces no supervisor report, while a worker that exits abnormally (a genuine crash) is automatically restarted by the supervisor. This keeps the pool at its target size over long-running operation without logging or re-spawning on every completed connection.

Accept calls use a 30-second timeout (`gen_tcp:accept/2` and `ssl:transport_accept/2`). On `{error, timeout}` the worker re-casts `accept` to itself and stays alive instead of stopping, preventing a worker from blocking indefinitely on a listen socket in a bad state. The timeout applies only to waiting for a new connection; it does not bound an already-accepted connection or an in-progress zone transfer.

## Module Responsibilities

### ioc2rpz_app.erl — Application Entry Point
- Implements the OTP `application` behavior
- Reads environment variables: `ipv4`, `ipv6`, `conf_file`, `db_dir`, `cd`
- Starts the main supervisor

### ioc2rpz_sup.erl — Main Supervisor & Configuration
- Implements the OTP `supervisor` behavior
- Reads and parses the Erlang-term configuration file via `file:consult/1`
- Populates `cfg_table` ETS with parsed configuration (keys, sources, RPZ zones, server settings)
- Manages zone update scheduling (AXFR full updates, IXFR incremental updates)
- Implements configuration reload without downtime (`reload_config3/1`)
- Spawns zone update processes (`update_zone_full/1`, `update_zone_inc/1`)
- Sends DNS NOTIFY after zone updates
- Validates configuration entries (keys, server, whitelists, sources, RPZ zones)

### ioc2rpz_proc_sup.erl — Protocol Supervisors
- Implements the OTP `supervisor` behavior
- Creates listening sockets for TCP, TLS, and UDP
- Configures Cowboy HTTPS for REST API and DoH endpoints
- Manages the `simple_one_for_one` worker pools for TCP/TLS
- Provides `start_socket/1` and `empty_listeners/1` for pool management

### ioc2rpz.erl — TCP/TLS DNS Worker
- Implements `gen_server` behavior for TCP and TLS connection handling
- Accept loop via `handle_cast(accept, ...)` for both TCP and TLS
- DNS request parsing and validation (`parse_dns_request/3`)
- TSIG signature validation (`validate_REQ/9`)
- Zone transfer serving (AXFR, IXFR)
- SOA query handling
- DNS management commands over DNS (reload-cfg, update-rpz, terminate)
- IOC-to-RPZ record conversion (`mrpz_from_ioc/2`, `mrpz_from_ioc/4`)
- DNS packet construction and sending (`send_dns_tcp/3`, `send_dns_tls/3`, `send_dns_udp/5`)
- Rate limiting enforcement via `ioc2rpz_fun:check_rate_limit/1`
- DNS NOTIFY message sending
- Sample zone serving (`send_sample_zone/9`)

### ioc2rpz_udp.erl — UDP DNS Worker
- Implements `gen_server` behavior for UDP DNS
- Opens a UDP socket on port 53 with `{active, true}` for continuous reception
- Spawns a new process for each incoming UDP packet via `spawn(ioc2rpz, parse_dns_request, ...)`
- Delegates all DNS processing to `ioc2rpz:parse_dns_request/3`

### ioc2rpz_conn.erl — IOC Source Connector
- Fetches IOC data from multiple source types:
  - `file:` — Local filesystem
  - `shell:` — Shell command output via `os:cmd/1`
  - `http://`, `https://`, `ftp://` — Remote feeds via `httpc:request/4`
- Parses IOC data using configurable regex patterns
- Supports parallel processing for large feeds (`p_clean_feed/4`)
- Handles IOC expiration dates captured by regex groups
- Retries failed downloads (configurable via `?Src_Retry`, default 3)
- Cleans and normalizes IOC entries (lowercase, deduplication)

### ioc2rpz_db.erl — Database / Cache Layer
- Manages ETS-based storage for zone data
- Supports two backends: ETS (default) and Mnesia (experimental)
- AXFR zone packet storage and retrieval (`read_db_pkt/1`, `write_db_pkt/2`)
- IXFR individual record storage (`read_db_record/3`, `write_db_record/3`)
- Zone metadata persistence (`saveZones/0`, `loadZones/0`)
- Hot cache packet management; periodic eviction of expired packet entries via `cleanup_hotcache/0`
- Record lookup for live zone serving (`lookup_db_record/2`)

### ioc2rpz_db_sup.erl — Database Process (ETS Heir)
- Implements `gen_server` behavior
- Acts as the ETS table heir process — receives table ownership if the creating process dies
- Handles `ETS-TRANSFER` messages to maintain table continuity

### ioc2rpz_fun.erl — Utility Functions
- Logging: standard messages (`logMessage/2`) and CEF format (`logMessageCEF/2`)
- CEF event message definitions (`msg_CEF/1`) for security event logging
- DNS utilities: IP conversion, domain name handling, query type/class names
- Rate limiting: `check_rate_limit/1` (and `check_rate_limit/2`) using the `rate_limits` ETS table; periodic cleanup via `cleanup_rate_limit_table/0`
- Binary/string conversions, base64url decoding
- JSON string escaping for REST responses (`json_escape/1`)
- TLS cipher suite selection (`get_cipher_suites/1`)
- IP ACL matching (`ip_in_list/2`)
- Local action parsing for RPZ responses

### ioc2rpz_rest.erl — REST Management API
- Implements `cowboy_rest` behavior
- Basic authentication using TSIG keys with IP ACL
- Endpoints:
  - `GET /api/v1/stats/serv` — Server statistics
  - `GET /api/v1/stats/rpz` — RPZ zone statistics
  - `GET /api/v1/stats/source` — Source statistics
  - `POST /api/v1/update/all_rpz` — Force update all zones
  - `POST /api/v1/update/:rpz` — Force update specific zone
  - `POST /api/v1/mgmt/reload_cfg` — Reload configuration
  - `POST /api/v1/mgmt/update_tkeys` — Update TSIG keys
  - `POST /api/v1/cache/sources/clear/all` — Clear all source caches
  - `POST /api/v1/cache/sources/clear/:source` — Clear specific source cache
  - `POST /api/v1/cache/sources/load/all` — Reload all sources
  - `POST /api/v1/mgmt/terminate` — Graceful shutdown
  - `GET /api/v1/feed/:rpz` — Pull RPZ feed
  - `GET /api/v1/ioc/:ioc` — Check if IOC is in any zone
- Returns JSON and plain text responses

### ioc2rpz_doh.erl — DNS-over-HTTPS Handler
- Implements `cowboy_rest` behavior
- Handles GET requests with base64url-encoded DNS messages in `?dns=` query parameter
- Handles POST requests with `application/dns-message` content type
- Delegates DNS processing to `ioc2rpz:parse_dns_request/3`
- Returns `application/dns-message` responses

## Data Flow

### IOC Source Ingestion → Zone Update

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│  IOC Sources     │     │  ioc2rpz_conn    │     │  ioc2rpz_db     │
│                  │     │                  │     │                 │
│  HTTP/HTTPS ─────┼────▶│  get_ioc/3       │     │                 │
│  Local files ────┼────▶│  clean_feed/3    │────▶│  write_db_record│
│  Shell commands ─┼────▶│  p_clean_feed/4  │     │  (rpz_ixfr_tbl) │
│  FTP ────────────┼────▶│                  │     │                 │
└─────────────────┘     └──────────────────┘     └────────┬────────┘
                                                          │
                         ┌──────────────────┐             │
                         │  ioc2rpz_sup     │             │
                         │                  │◀────────────┘
                         │  update_zone_full│
                         │  update_zone_inc │
                         │                  │
                         └────────┬─────────┘
                                  │
                         ┌────────▼─────────┐     ┌─────────────────┐
                         │  ioc2rpz         │     │  ioc2rpz_db     │
                         │                  │     │                 │
                         │  send_zone_live  │────▶│  write_db_pkt   │
                         │  send_packets    │     │  (rpz_axfr_tbl) │
                         │  mrpz_from_ioc   │     │                 │
                         └──────────────────┘     └─────────────────┘
```

1. **Scheduler** (`timer:apply_interval` every 60s) calls `update_all_zones(false)`
2. For each expired zone, spawns `update_zone_full/1` or `update_zone_inc/1`
3. **Full update (AXFR)**: Fetches all IOCs from configured sources via `ioc2rpz_conn:get_ioc/3`
4. IOCs are parsed with regex, normalized to lowercase, deduplicated
5. IOCs stored in `rpz_ixfr_table` with serial and expiration timestamps
6. DNS packets generated (SOA + NS + IOC records) and stored compressed in `rpz_axfr_table`
7. Zone serial and metadata updated in `cfg_table`
8. DNS NOTIFY sent to configured secondary servers
9. **Incremental update (IXFR)**: Only fetches new/changed IOCs, merges with existing records

### DNS Query Processing

```
┌──────────┐     ┌──────────────────┐     ┌─────────────────┐
│  Client   │     │  Listener        │     │  ioc2rpz        │
│           │     │                  │     │                 │
│  UDP ─────┼────▶│  ioc2rpz_udp    │────▶│                 │
│  TCP ─────┼────▶│  ioc2rpz (tcp)  │────▶│ parse_dns_req   │
│  TLS ─────┼────▶│  ioc2rpz (tls)  │────▶│                 │
│  DoH ─────┼────▶│  ioc2rpz_doh    │────▶│                 │
└──────────┘     └──────────────────┘     └────────┬────────┘
                                                    │
                  ┌──────────────────┐              │
                  │  Processing      │◀─────────────┘
                  │                  │
                  │  1. Rate limit   │
                  │  2. TSIG verify  │
                  │  3. Zone lookup  │
                  │  4. Serve data   │
                  └────────┬─────────┘
                           │
              ┌────────────┼────────────┐
              ▼            ▼            ▼
        ┌──────────┐ ┌──────────┐ ┌──────────┐
        │   SOA    │ │   AXFR   │ │   IXFR   │
        │ response │ │ transfer │ │ transfer │
        │          │ │ from     │ │ from     │
        │ cfg_table│ │ axfr_tbl │ │ ixfr_tbl │
        └──────────┘ └──────────┘ └──────────┘
```

1. Client connects via UDP/TCP/TLS/DoH
2. `parse_dns_request/3` validates the DNS packet structure
3. Rate limiting checked via an intelligent (hybrid) key from `rl_key/5`: granular `{IP, QName, QType}` for provisioned zones (SOA/AXFR/IXFR) and recognized management (CHAOS/TXT) requests, aggregate `{IP}` for everything else (unknown zone / unsupported qtype / unrecognized name) to prevent query-name-variation bypass
4. TSIG signature validated if present (for zone transfers)
5. Zone looked up in `cfg_table`
6. Response served:
   - **SOA**: Returns zone serial, SOA timers, NS record
   - **AXFR**: Streams pre-built packets from `rpz_axfr_table`
   - **IXFR**: Generates incremental transfer from `rpz_ixfr_table`
   - **Non-cacheable zones**: Built live from sources on each request

### Hot Cache Flow

Sources marked with `keep_in_cache=true` are pre-loaded into `rpz_hotcache_table` on startup and refreshed periodically. This avoids re-downloading frequently used feeds for non-cacheable (live) zones.

## ETS Table Inventory

### cfg_table
- **Type**: `ordered_set`, public, named
- **Purpose**: Stores all parsed configuration — server settings, TSIG keys, sources, RPZ zone definitions
- **Created in**: `ioc2rpz_db:init_db/3`
- **Heir**: `ioc2rpz_db_sup` process

| Key Pattern | Value | Description |
|---|---|---|
| `srv` | `{srv, NSBin, EmailBin, MKeysList, ACL, CertRec, SrvRec}` | Server configuration |
| `{cfg_file, Filename}` | — | Path to active config file |
| `{db_dir, Dir}` | — | Database directory path |
| `{[key, KeyNameBin], KeyName, Alg, KeyData}` | — | TSIG key definition |
| `{[key_group, GroupName, KeyNameBin], KeyNameBin}` | — | Key group membership |
| `{[source, SourceName], #source{}}` | — | IOC source definition |
| `{[rpz, ZoneBin], ZoneBin, #rpz{}}` | — | RPZ zone definition and runtime state |

### rpz_axfr_table
- **Type**: `ordered_set`, public, named
- **Purpose**: Stores pre-built DNS packets for full zone transfers (AXFR)
- **Created in**: `ioc2rpz_db:init_db/3`
- **Heir**: `ioc2rpz_db_sup` process
- **Persistence**: Optionally saved to disk via `tab2file`

| Key Pattern | Value | Description |
|---|---|---|
| `{rpz, ZoneBin, Serial, PktNumber, PID}` | Compressed binary DNS packet | Zone transfer packet |
| `{axfr_rpz_cfg, ZoneBin}` | Zone metadata tuple | Zone info: serial, SOA timers, cache flag, wildcards, sources, MD5, update time, IOC count, rule count |

### rpz_ixfr_table
- **Type**: `duplicate_bag`, public, named
- **Purpose**: Stores individual IOC records for incremental zone transfers (IXFR) and zone rebuilding
- **Created in**: `ioc2rpz_db:init_db/3`
- **Heir**: `ioc2rpz_db_sup` process
- **Persistence**: Optionally saved to disk via `tab2file`

| Key Pattern | Value | Description |
|---|---|---|
| `{ioc, ZoneBin, IOCBin, IoCType}` | `{Serial, ExpirationTime}` | Individual IOC record with serial and expiry |
| `{ixfr_rpz_cfg, ZoneBin}` | Zone metadata tuple | IXFR zone info: serial, serial_ixfr, update time, nz_update_time |

### rpz_hotcache_table
- **Type**: `ordered_set`, public, named
- **Purpose**: Caches frequently accessed IOC source data and pre-built zone packets for fast serving
- **Created in**: `ioc2rpz_db:init_db/3`
- **Heir**: `ioc2rpz_db_sup` process

| Key Pattern | Value | Description |
|---|---|---|
| `{SourceName, axfr\|ixfr}` | `{IOCList, Timestamp, Metadata}` | Cached source IOC data |
| `{pkthotcache, ZoneBin, PktNumber}` | `{PacketData, Timestamp, Metadata}` | Cached zone packets |

### rate_limits
- **Type**: `set`, public, named
- **Purpose**: Tracks per-client request counts for DNS rate limiting
- **Created in**: `ioc2rpz_sup:init/1`
- **Cleanup**: Expired entries are swept periodically by `ioc2rpz_fun:cleanup_rate_limit_table/0`

The key is chosen per request by `ioc2rpz:rl_key/5` (intelligent/hybrid scheme):

| Key Pattern | Value | Description |
|---|---|---|
| `{IP, QName, QType}` | `{LastRequestTime, RequestCount}` | **Granular** bucket for provisioned zones (class `IN` + `SOA`/`AXFR`/`IXFR`) and recognized management requests (class `CHAOS` + `TXT`). Limited by `?MAX_REQUESTS_PER_WINDOW`. |
| `{IP}` | `{LastRequestTime, RequestCount}` | **Aggregate** per-IP bucket for everything else (unknown/unprovisioned zone, unsupported QTYPE, wrong class, unrecognized management name). Limited by `?MAX_UNKNOWN_REQUESTS_PER_WINDOW`. Prevents query-name-variation bypass. |

`ioc2rpz_fun:check_rate_limit/1` selects the threshold by key shape: a 1-tuple `{IP}` uses the aggregate limit, a 3-tuple `{IP, QName, QType}` uses the granular limit.

### stat_table
- **Type**: `ordered_set`, public, named
- **Purpose**: Stores server and query statistics
- **Created in**: `ioc2rpz_db:init_db/3`
- **Heir**: `ioc2rpz_db_sup` process

### rate_limits
- **Type**: `set`, public, named (created via `ets:new(?RATE_LIMIT_TABLE, ...)`)
- **Purpose**: Tracks DNS query rates per client for rate limiting
- **Created in**: `ioc2rpz_sup:init/1`

| Key Pattern | Value | Description |
|---|---|---|
| `{IP, QName, QType}` | `{LastRequestTime, RequestCount}` | Per-query rate tracking |

- **Window**: 10 seconds (`?RATE_LIMIT_WINDOW`)
- **Max requests**: 1 per window (`?MAX_REQUESTS_PER_WINDOW`)

## Configuration File Format

The configuration file uses Erlang term syntax (parsed via `file:consult/1`). Each line is a valid Erlang term terminated with a period.

### Server Configuration

```erlang
{srv, {NameServer, Email, ManagementKeys, ACL}}.
```

| Field | Type | Description |
|---|---|---|
| `NameServer` | string | NS record FQDN (e.g., `"ns1.rpz-proxy.com"`) |
| `Email` | string | SOA email in DNS format (e.g., `"support.rpz-proxy.com"`) |
| `ManagementKeys` | list | TSIG key names and/or `{groups, [GroupNames]}` for REST API auth |
| `ACL` | list of strings | IP addresses allowed to access management interfaces |

### TLS Certificate

```erlang
{cert, {CertFile, KeyFile, CACertFile}}.
```

| Field | Type | Description |
|---|---|---|
| `CertFile` | string | Path to PEM certificate file |
| `KeyFile` | string | Path to PEM private key file |
| `CACertFile` | string | Path to CA certificate file (empty string if not used) |

### TSIG Keys

```erlang
%% Basic key
{key, {KeyName, Algorithm, Base64Key}}.

%% Key with group membership
{key, {KeyName, Algorithm, Base64Key, [GroupNames]}}.
```

| Field | Type | Description |
|---|---|---|
| `KeyName` | string | Unique key identifier |
| `Algorithm` | string | `"md5"`, `"sha256"`, or `"sha512"` |
| `Base64Key` | string | Base64-encoded key data |
| `GroupNames` | list of strings | Optional key group memberships |

### Key Groups

```erlang
{key_group, {GroupName, [KeyNames]}}.
```

### IOC Sources

```erlang
%% Minimal (4 fields)
{source, {Name, AXFR_URL, IXFR_URL, Regex}}.

%% With user/count (6 fields)
{source, {Name, AXFR_URL, IXFR_URL, Regex, UserID, MaxCount}}.

%% With cache times (8 fields)
{source, {Name, AXFR_URL, IXFR_URL, Regex, UserID, MaxCount, HotCacheTime, HotCacheTimeIXFR}}.

%% Full (10 fields)
{source, {Name, AXFR_URL, IXFR_URL, Regex, UserID, MaxCount, HotCacheTime, HotCacheTimeIXFR, IoCType, KeepInCache}}.
```

| Field | Type | Default | Description |
|---|---|---|---|
| `Name` | string | — | Unique source identifier |
| `AXFR_URL` | string | — | URL for full feed. Schemes: `file:`, `shell:`, `http://`, `https://`, `ftp://` |
| `IXFR_URL` | string | — | URL for incremental feed. Use `"[:AXFR:]"` to reuse AXFR URL. Supports `[:FTimestamp:]` and `[:ToTimestamp:]` placeholders |
| `Regex` | string | — | Erlang regex to extract IOC from each line. Use `""` or `none` for default. First capture group = IOC, optional second = expiration |
| `UserID` | string | `""` | User restriction identifier |
| `MaxCount` | integer | `0` | Maximum IOC count (0 = unlimited) |
| `HotCacheTime` | integer | `900` | Hot cache TTL in seconds |
| `HotCacheTimeIXFR` | integer | `0` | Hot cache TTL for IXFR data |
| `IoCType` | string | `"mixed"` | IOC type: `"fqdn"`, `"ip"`, or `"mixed"` |
| `KeepInCache` | boolean | `false` | If `true`, source is pre-loaded and kept in hot cache |

### Whitelists

Same tuple structure as sources but without IXFR_URL:

```erlang
{whitelist, {Name, AXFR_URL, Regex}}.
{whitelist, {Name, AXFR_URL, Regex, UserID, MaxCount}}.
{whitelist, {Name, AXFR_URL, Regex, UserID, MaxCount, HotCacheTime, HotCacheTimeIXFR}}.
{whitelist, {Name, AXFR_URL, Regex, UserID, MaxCount, HotCacheTime, HotCacheTimeIXFR, IoCType, KeepInCache}}.
```

### RPZ Zones

```erlang
{rpz, {Zone, Refresh, Retry, Expiration, NegTTL, Cache, Wildcards, Action,
       AuthKeys, IoCType, AXFR_Time, IXFR_Time, Sources, NotifyList, Whitelists}}.
```

| Field | Type | Description |
|---|---|---|
| `Zone` | string | Zone FQDN (e.g., `"malware.ioc2rpz"`) |
| `Refresh` | integer | SOA refresh timer (seconds) |
| `Retry` | integer | SOA retry timer (seconds) |
| `Expiration` | integer | SOA expiration timer (seconds) |
| `NegTTL` | integer | SOA negative TTL (seconds) |
| `Cache` | string | `"true"` or `"false"` — whether to cache zone in ETS |
| `Wildcards` | string | `"true"` or `"false"` — whether to generate wildcard RPZ rules |
| `Action` | string or list | RPZ action (see below) |
| `AuthKeys` | list | TSIG key names and/or `{groups, [GroupNames]}` authorized for zone transfers |
| `IoCType` | string | `"fqdn"`, `"ip"`, or `"mixed"` |
| `AXFR_Time` | integer | Full zone refresh interval (seconds) |
| `IXFR_Time` | integer | Incremental refresh interval (seconds, 0 = disabled) |
| `Sources` | list of strings | Source names to include in this zone |
| `NotifyList` | list of strings | IP addresses to send DNS NOTIFY after updates |
| `Whitelists` | list of strings | Whitelist names to apply |

#### RPZ Actions

| Action | Format | Description |
|---|---|---|
| `"nodata"` | string | Return NODATA (empty answer) |
| `"nxdomain"` | string | Return NXDOMAIN |
| `"passthru"` | string | Allow query (RPZ passthru) |
| `"drop"` | string | Drop the query silently |
| `"tcp-only"` | string | Force TCP retry |
| `"blockns"` | string | Block nameserver |
| `[{"redirect_domain", "example.com"}]` | list | Redirect to specified domain |
| `[{"redirect_ip", "1.2.3.4"}]` | list | Redirect to specified IP |
| `[{"local_a", "1.2.3.4"}]` | list | Return local A record |
| `[{"local_aaaa", "fe80::1"}]` | list | Return local AAAA record |
| `[{"local_cname", "www.example.com"}]` | list | Return local CNAME record |
| `[{"local_txt", "text value"}]` | list | Return local TXT record |

Multiple local actions can be combined in a single list.

### Include Files

```erlang
{include, "path/to/additional_config.conf"}.
```

Included files are merged into the main configuration. All tuple types are supported in included files.

## Key Constants (ioc2rpz.hrl)

| Macro | Value | Description |
|---|---|---|
| `?Port` | 53 | DNS port |
| `?PortTLS` | 853 | DoT port |
| `?PortDoH` | 443 | DoH port |
| `?PortREST` | 8443 | REST API port |
| `?TTL` | 900 | Default record TTL (seconds) |
| `?DNSPktMax` | 16383 | Maximum DNS packet size for label compression |
| `?Compression` | 6 | zlib compression level for cached zone packets |
| `?ZoneRefTime` | 60000 | Zone refresh check interval (milliseconds) |
| `?TCPTimeout` | 3000 | TCP timeout (milliseconds) |
| `?HotCacheTime` | 900 | Default hot cache TTL (seconds) |
| `?Src_Retry` | 3 | Source download retry count |
| `?Src_Retry_TimeOut` | 3 | Retry delay (seconds) |
| `?RATE_LIMIT_WINDOW` | 10000 | Rate limit window (milliseconds) |
| `?MAX_REQUESTS_PER_WINDOW` | 1 | Max requests per IP per window |
| `?ShellMaxRespSize` | 2 GB | Maximum shell command response size |
| `?SourcePullTimeout` | 300000 | Source download timeout (milliseconds) |
| `?TLSVersion` | `'tlsv1.2-1.3'` | Supported TLS versions |
| `?DBStorage` | `ets` | Storage backend (ets or mnesia) |

## Dependencies

- **Erlang/OTP**: kernel, stdlib, inets, ssl, sasl
- **Cowboy**: HTTP/HTTPS server for REST API and DoH endpoints
- **crypto**: Used for TSIG HMAC computation and hashing
