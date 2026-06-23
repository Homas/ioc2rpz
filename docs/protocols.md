# ioc2rpz Protocol Support

## DNS Query Handling

ioc2rpz listens on multiple transport protocols for DNS queries. All transports share the same query processing pipeline: rate limiting, TSIG validation, zone lookup, and response generation.

### UDP (Port 53)

UDP is used for lightweight DNS queries, primarily SOA lookups. The `ioc2rpz_udp` module opens a UDP socket with `{active, true}` and spawns a new process for each incoming packet.

- SOA queries are the primary use case over UDP
- Responses exceeding 512 bytes are truncated by the server, which sets the TC (truncation) bit in the response header per RFC 1035 §4.2.1, prompting the client to retry over TCP (see `send_dns_udp/5`)
- Management commands are not supported over UDP

```bash
# SOA query over UDP
dig @127.0.0.1 zone.ioc2rpz SOA -y hmac-sha256:keyname:base64key
```

### TCP (Port 53)

TCP handles zone transfers (AXFR/IXFR), SOA queries, and management commands. A pool of 5 pre-spawned accept workers handles incoming connections via `simple_one_for_one` supervisor strategy.

- Zone transfers (AXFR/IXFR) require TCP
- Management commands require TCP (or DoT)
- Default TCP timeout: 3000ms (`?TCPTimeout`)
- Accept calls use a 30-second timeout; on timeout the worker re-enters the accept loop instead of blocking indefinitely (the timeout applies only to waiting for a new connection, not to an in-progress transfer)

```bash
# AXFR zone transfer over TCP
dig @127.0.0.1 zone.ioc2rpz AXFR +tcp -y hmac-sha256:keyname:base64key

# IXFR incremental transfer
dig @127.0.0.1 zone.ioc2rpz IXFR=12345 +tcp -y hmac-sha256:keyname:base64key
```

### DNS over TLS / DoT (Port 853)

DoT encrypts DNS traffic using TLS. The TLS listener starts automatically when a `cert` record is present in the configuration. A pool of 5 TLS accept workers handles connections.

- Supported TLS versions: 1.2 and 1.3 (`?TLSVersion = 'tlsv1.2-1.3'`)
- Supports AXFR, IXFR, SOA queries, and management commands
- TLS PIN is not supported
- DNS NOTIFY messages are sent unencrypted (plain TCP)
- Accept calls use a 30-second timeout; on timeout the worker re-enters the accept loop instead of blocking indefinitely (the timeout applies only to the accept phase, not to the TLS handshake or an in-progress transfer)
- Certificates auto-refresh when files are replaced on disk (up to ~2 minute delay due to Erlang SSL caching)

```bash
# SOA query over DoT
kdig @127.0.0.1 -p 853 zone.ioc2rpz SOA +tls -y hmac-sha256:keyname:base64key

# AXFR over DoT
dig @127.0.0.1 -p 853 zone.ioc2rpz AXFR +tls +tcp -y hmac-sha256:keyname:base64key
```

Certificate configuration:

```erlang
{cert, {"cfg/cert.pem", "cfg/key.pem", "cfg/cacerts.pem"}}.
```

### DNS over HTTPS / DoH (Port 443)

DoH provides DNS resolution over HTTPS, handled by the `ioc2rpz_doh` module using Cowboy. The endpoint is `/dns-query`.

Supported methods:

- GET with base64url-encoded DNS message in `?dns=` query parameter
- POST with `Content-Type: application/dns-message` body

Responses use `Content-Type: application/dns-message`. A POST request with an empty body (or any request without a decodable DNS message) returns HTTP 400 Bad Request rather than a 500 / internal error.

POST request bodies are limited to 4096 bytes; a request whose body exceeds this limit receives HTTP 413 Payload Too Large and is not processed. This cap applies to POST bodies only — GET requests carry the query in the `dns` query-string parameter, which is bounded separately by Cowboy's request-line and header size limits. 4096 bytes is well above the size of any DNS query (including EDNS0 and TSIG).

```bash
# DoH GET request (base64url-encoded DNS query)
curl -H "Accept: application/dns-message" \
  "https://127.0.0.1:443/dns-query?dns=AAABAAABAAAAAAAAA3d3dwdleGFtcGxlA2NvbQAAAQAB" -k

# DoH POST request
curl -X POST -H "Content-Type: application/dns-message" \
  --data-binary @dns_query.bin \
  "https://127.0.0.1:443/dns-query" -k
```

## Zone Transfer Support

### AXFR (Full Zone Transfer)

AXFR serves the complete RPZ zone. Pre-built zone packets are stored compressed in the `rpz_axfr_table` ETS table. On request, packets are decompressed and wrapped with SOA/NS/TSIG records.

- Requires TCP or DoT transport
- TSIG authentication required (unless zone has empty key list)
- Zone update interval configured per-RPZ via `AXFR_Time` (seconds)
- Maximum DNS packet size: 16383 bytes (`?DNSPktMax`)
- Compression level: 6 (`?Compression`, zlib 0-9)

### IXFR (Incremental Zone Transfer)

IXFR serves only the changes since a given serial. Individual IOC records are stored in `rpz_ixfr_table` with serial numbers and expiration timestamps.

- Requires TCP or DoT transport
- TSIG authentication required
- Incremental update interval configured per-RPZ via `IXFR_Time` (seconds, 0 = disabled)
- Falls back to AXFR if requested serial is older than `serial_ixfr`
- IXFR cache is flushed after each full AXFR update

### TSIG Authentication

Zone transfers are authenticated using TSIG (RFC 2845). Supported algorithms:

| Algorithm | Config Value |
|-----------|-------------|
| HMAC-MD5 | `"md5"` |
| HMAC-SHA256 | `"sha256"` |
| HMAC-SHA512 | `"sha512"` |

Keys are configured as Erlang terms:

```erlang
{key, {"keyname", "sha256", "base64encodedkey=="}}.
{key, {"keyname", "sha256", "base64encodedkey==", ["group1", "group2"]}}.
```

RPZ zones reference keys by name or group:

```erlang
%% Direct key references
["dnsproxykey_1", "dnsproxykey_2"]

%% With key groups
["dnsproxykey_1", {groups, ["customers", "public"]}]
```

Generate TSIG keys with `dnssec-keygen`:

```bash
dnssec-keygen -a HMAC-SHA256 -b 256 -n USER my-tsig-key
```

## DNS Management Commands

Management is supported over TCP and DoT using DNS queries with class `CHAOS` and type `TXT`. A designated management TSIG key is required.

| Command | RR Name | Action |
|---------|---------|--------|
| Server status | `ioc2rpz-status` | Returns current server status |
| Reload config | `ioc2rpz-reload-cfg` | Reloads configuration file |
| Update TSIG keys | `ioc2rpz-update-tkeys` | Reloads TSIG keys only |
| Update all zones | `ioc2rpz-update-all-rpz` | Forces full refresh of all RPZ zones |
| Update one zone | `<zone_name>` | Forces full refresh of a specific zone |
| Shutdown | `ioc2rpz-terminate` | Graceful server shutdown |
| Sample zone | `sample-zone.ioc2rpz` (class IN, type AXFR) | Returns a sample RPZ zone |
| Sample zone SOA | `sample-zone.ioc2rpz` (class IN, type SOA) | Returns the sample zone's SOA (no longer NOTAUTH) |

```bash
# Check server status
dig +tcp -y hmac-sha256:mgmtkey:base64key== @127.0.0.1 ioc2rpz-status TXT -c CHAOS

# Reload configuration
dig +tcp -y hmac-sha256:mgmtkey:base64key== @127.0.0.1 ioc2rpz-reload-cfg TXT -c CHAOS

# Force update a specific zone
dig +tcp -y hmac-sha256:mgmtkey:base64key== @127.0.0.1 dga.ioc2rpz TXT -c CHAOS
```

Management over DNS can be disabled by setting `MGMToDNS` to `false` in `include/ioc2rpz.hrl`.

## REST API

The REST API runs on port 8443 over HTTPS (requires `cert` configuration). It uses Cowboy's `cowboy_rest` behavior.

### Authentication

- HTTP Basic authentication using TSIG key name as username and base64-encoded key as password
- IP-based ACL restricts access to configured addresses
- Both conditions must be satisfied
- All endpoints accept both GET and POST methods

```bash
curl -u "keyname:base64key==" --insecure https://127.0.0.1:8443/api/v1/stats/serv
```

### Response Formats

Set via `Accept` header:
- `application/json` (default)
- `text/plain`

```bash
# JSON response (default)
curl -u "keyname:key==" -k https://127.0.0.1:8443/api/v1/stats/serv

# Plain text response
curl -u "keyname:key==" -k -H "Accept: text/plain" https://127.0.0.1:8443/api/v1/stats/serv
```

### Endpoints

The API version segment is optional (e.g., `/api/v1/...` or `/api/v1.0/...`).

#### Statistics

`GET|POST /api/v1/stats/serv` — Server statistics (node name, total rules, memory usage)

```json
{
  "node_name": "ioc2rpz@hostname",
  "srv_total_rules": 15000,
  "hot_cache_mem": "12.5 Mb",
  "axfr_table_mem": "45.2 Mb",
  "ixfr_table_mem": "8.1 Mb"
}
```

`GET|POST /api/v1/stats/rpz` — RPZ zone statistics

```json
{
  "rpz": [
    {
      "name": "malware.ioc2rpz",
      "rule_count": 5000,
      "ioc_count": 4500,
      "serial": 1709000000,
      "serial_ixfr": 1708990000,
      "update_time": 1709000000,
      "ixfr_update_time": 1708995000,
      "ixfr_nz_update_time": 1708995000
    }
  ]
}
```

`GET|POST /api/v1/stats/source` — Source statistics

```json
{
  "sources": [
    {"name": "sample_fqdn", "ioc_count": 150}
  ]
}
```

#### Zone Management

`GET|POST /api/v1/update/all_rpz` — Force full refresh of all RPZ zones

```json
{"status": "ok", "msg": "All RPZ zones will be updated"}
```

`GET|POST /api/v1/update/:rpz_name` — Force full refresh of a specific zone

```json
{"status": "ok", "msg": "RPZ malware.ioc2rpz will be updated"}
```

Error (HTTP 520): `{"status": "error", "msg": "RPZ nonexistent.zone not found"}`

#### Server Management

`GET|POST /api/v1/mgmt/reload_cfg` — Reload configuration file

```json
{"status": "ok", "msg": "Configuration reloaded"}
```

Error (HTTP 520): `{"status": "error", "msg": "Configuration reload error"}`

`GET|POST /api/v1/mgmt/update_tkeys` — Reload TSIG keys from configuration

```json
{"status": "ok", "msg": "TSIG keys were updated"}
```

Error (HTTP 520): `{"status": "error", "msg": "TSIG keys update error"}`

`GET|POST /api/v1/mgmt/terminate` — Graceful server shutdown

```json
{"status": "ok", "msg": "Terminating"}
```

#### Cache Management

`GET|POST /api/v1/cache/sources/clear/all` — Remove all sources from hot cache

```json
{"status": "ok", "msg": "All sources were removed from the hotcache"}
```

`GET|POST /api/v1/cache/sources/clear/:source_name` — Remove a specific source from hot cache

```json
{"status": "ok", "msg": "sample_fqdn source was removed from the hot cache"}
```

`GET|POST /api/v1/cache/sources/load/all` — Reload all sources into hot cache

```json
{"status": "ok", "msg": "All sources will loaded to the hot cache"}
```

#### Feed & IOC Queries

`GET|POST /api/v1/feed/:rpz_name` — Get indicators from an RPZ feed

Optional query parameter: `?type=fqdn|ip|both` (default: `both`)

```json
{
  "status": "ok",
  "rpz": "malware.ioc2rpz",
  "iocs": ["baddomain.com", "evil.example.org"]
}
```

Error (HTTP 520): `{"status": "error", "msg": "RPZ malware.ioc2rpz not found"}`

`GET|POST /api/v1/ioc/:ioc` — Check if an indicator is blocked by any RPZ feed

Optional query parameter: `?tkey=keyname` to limit search to zones accessible by that key.

```json
{
  "ioc": "baddomain.com",
  "tkey": "",
  "data": [
    {
      "ioc": "baddomain.com",
      "feeds": [
        {
          "feed": "malware.ioc2rpz",
          "wildcard": "true",
          "type": "fqdn",
          "rpz_serial": 1709000000,
          "ioc_expiration": 0
        }
      ]
    }
  ]
}
```

Error: `{"status": "error", "ioc": "nonexistent.com"}`

### Error Responses

Authentication failure (HTTP 401):

```json
{"status": "error", "msg": "Authentication failed"}
```

Zone/resource not found (HTTP 520):

```json
{"status": "error", "msg": "RPZ nonexistent.zone not found"}
```

Unsupported endpoint (HTTP 200, should be 501):

```json
{"status": "error", "msg": "Unsupported request"}
```

## DNS NOTIFY

After a zone update (AXFR or IXFR), ioc2rpz sends DNS NOTIFY messages (RFC 1996) to configured secondary servers. This prompts secondaries to check the zone SOA and initiate a transfer if the serial has changed.

- NOTIFY is sent over UDP (unencrypted, even when DoT is enabled)
- Target IPs are configured per-RPZ in the `NotifyList` field

```erlang
{rpz, {"zone.ioc2rpz", 7200, 3600, 2592000, 7200, "true", "true", "nxdomain",
       ["dnsproxykey_1"], "mixed", 86400, 3600,
       ["source1"],
       ["10.0.0.1", "10.0.0.2"],  %% NOTIFY targets
       []}}.
```

## Rate Limiting

DNS queries are rate-limited using an **intelligent (hybrid) key** stored in an ETS table (`rate_limits`). The key is chosen per request by `ioc2rpz:rl_key/5` so that legitimate multi-zone traffic and abusive query-name-variation traffic are treated differently:

| Request class | Condition | Rate-limit key | Threshold macro |
|---|---|---|---|
| Provisioned zone, supported QTYPE | class `IN` + `SOA`/`AXFR`/`IXFR` for a zone in `cfg_table` (incl. virtual `sample-zone.ioc2rpz`) | `{client_IP, query_name, query_type}` (granular) | `?MAX_REQUESTS_PER_WINDOW` |
| Recognized management request | class `CHAOS` + `TXT` with a known management command (`ioc2rpz-status`, `ioc2rpz-reload-cfg`, `ioc2rpz-update-tkeys`, `ioc2rpz-terminate`, `ioc2rpz-update-all-rpz`) or a provisioned zone name (force-AXFR) | `{client_IP, query_name, query_type}` (granular) | `?MAX_REQUESTS_PER_WINDOW` |
| Everything else | non-existent/unprovisioned zone, unsupported QTYPE, wrong class, or unrecognized CHAOS/TXT name | `{client_IP}` (aggregate per-IP) | `?MAX_UNKNOWN_REQUESTS_PER_WINDOW` |

The granular bucket means a legitimate secondary polling/transferring several zones (e.g. `rpz1`, `rpz2`, `rpz3`) plus management from one IP is counted independently per zone+type and is not starved. The aggregate bucket means an attacker cannot multiply their effective limit by varying the query name (random subdomains, junk CHAOS/TXT names) — all such traffic shares a single per-IP counter.

| Parameter | Value | Macro |
|-----------|-------|-------|
| Window | 10 seconds | `?RATE_LIMIT_WINDOW` (10000 ms) |
| Max requests per window (granular: known zone + type / management) | 1 | `?MAX_REQUESTS_PER_WINDOW` |
| Max requests per window (aggregate: unknown zone / unsupported type) | 1 | `?MAX_UNKNOWN_REQUESTS_PER_WINDOW` |

The threshold is selected by key shape in `ioc2rpz_fun:check_rate_limit/1` (a 1-tuple `{IP}` uses the aggregate limit; a 3-tuple `{IP, QName, QType}` uses the granular limit). When the rate limit is exceeded, the server returns a DNS `REFUSED` response and logs a CEF event (code 429).

Rate limiting applies to all DNS query transports (UDP, TCP, TLS, DoH).

## Supported DNS Record Types

### Query Types

| Type | Description | Usage |
|------|-------------|-------|
| SOA | Start of Authority | Zone serial checks, SOA queries |
| AXFR | Full zone transfer | Complete RPZ download |
| IXFR | Incremental zone transfer | Delta RPZ updates |
| TXT | Text record | Management commands (class CHAOS) |
| A | IPv4 address | RPZ redirect responses |
| AAAA | IPv6 address | RPZ redirect responses |
| CNAME | Canonical name | RPZ redirect responses |

### RPZ Actions

RPZ actions define how matching DNS queries are handled by the consuming DNS resolver. Actions are configured per-zone.

| Action | Config Value | RPZ CNAME Target | Description |
|--------|-------------|------------------|-------------|
| NXDOMAIN | `"nxdomain"` | `*.` | Returns NXDOMAIN (domain does not exist) |
| NODATA | `"nodata"` | `*.` (with NODATA encoding) | Returns empty answer (domain exists, no records) |
| Passthru | `"passthru"` | `rpz-passthru.` | Allows the query (exemption rule) |
| Drop | `"drop"` | `rpz-drop.` | Silently drops the query |
| TCP-Only | `"tcp-only"` | `rpz-tcp-only.` | Forces client to retry over TCP |
| Block NS | `"blockns"` | Nameserver blocking | Blocks the authoritative nameserver |
| Redirect (domain) | `{"redirect_domain", "example.com"}` | Target FQDN | Redirects to a specified domain (alias for `local_cname`) |
| Redirect (IP) | `{"redirect_ip", "1.2.3.4"}` | Target IP | Redirects to a specified IP (alias for `local_a`/`local_aaaa`) |

### Local Record Actions

Multiple local records can be combined in a single RPZ zone:

```erlang
[
  {"local_a", "127.0.0.1"},
  {"local_a", "127.0.0.2"},
  {"local_aaaa", "fe80::1"},
  {"local_cname", "www.example.com"},
  {"local_txt", "Blocked by policy"}
]
```

| Local Action | Record Type | Description |
|-------------|-------------|-------------|
| `local_a` | A | Returns an IPv4 address |
| `local_aaaa` | AAAA | Returns an IPv6 address |
| `local_cname` | CNAME | Returns a CNAME redirect |
| `local_txt` | TXT | Returns a text record |

### IOC Types

Each RPZ zone is configured with an IOC type that determines what kind of indicators it contains:

| Type | Config Value | Description |
|------|-------------|-------------|
| FQDN | `"fqdn"` | Domain name indicators only |
| IP | `"ip"` | IPv4/IPv6 address indicators only |
| Mixed | `"mixed"` | Both domain and IP indicators |

When `wildcards` is set to `"true"`, wildcard RPZ rules (e.g., `*.malware.com`) are automatically generated for FQDN indicators.

## Port Summary

| Port | Protocol | Service | Condition |
|------|----------|---------|-----------|
| 53 | UDP | DNS queries (SOA) | Always |
| 53 | TCP | DNS queries, zone transfers, management | Always |
| 853 | TCP+TLS | DoT (same as TCP but encrypted) | Requires `cert` config |
| 443 | TCP+TLS | DoH (`/dns-query` endpoint) | Requires `cert` config |
| 8443 | TCP+TLS | REST API | Requires `cert` config |

## References

- [RFC 1035 — Domain Names: Implementation and Specification](https://tools.ietf.org/html/rfc1035)
- [RFC 1995 — Incremental Zone Transfer in DNS](https://tools.ietf.org/html/rfc1995)
- [RFC 1996 — DNS NOTIFY](https://tools.ietf.org/html/rfc1996)
- [RFC 2845 — TSIG Authentication](https://tools.ietf.org/html/rfc2845)
- [RFC 4635 — HMAC SHA TSIG Algorithm Identifiers](https://tools.ietf.org/html/rfc4635)
- [RFC 5966 — DNS Transport over TCP](https://tools.ietf.org/html/rfc5966)
- [RFC 6891 — EDNS(0)](https://tools.ietf.org/html/rfc6891)
- [RFC 7858 — DNS over TLS](https://tools.ietf.org/html/rfc7858)
- [DNS RPZ Draft](https://tools.ietf.org/html/draft-ietf-dnsop-dns-rpz-00)
