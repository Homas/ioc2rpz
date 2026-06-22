# ioc2rpz Deployment & Operations Guide

## Build Instructions

ioc2rpz requires Erlang/OTP 24.0+ and rebar3.

### Compile

```bash
rebar3 compile
```

### Run Tests

```bash
rebar3 eunit
```

### Build a Release

```bash
rebar3 release
```

The release is output to `_build/default/rel/ioc2rpz/`. Start it with:

```bash
_build/default/rel/ioc2rpz/bin/ioc2rpz foreground
```

### Development Shell

```bash
rebar3 shell
```

This loads `config/sys.config.src` and `config/vm.args` automatically.

## Docker Deployment

The included `Dockerfile` builds on `erlang:alpine`. It runs `rebar3 eunit` and `rebar3 release` during the image build.

### Build the Image

```bash
docker build -t ioc2rpz .
```

### Run the Container

```bash
docker run -d --name ioc2rpz \
  --restart always \
  --log-driver=syslog \
  --mount type=bind,source=/path/to/cfg,target=/opt/ioc2rpz/cfg \
  --mount type=bind,source=/path/to/db,target=/opt/ioc2rpz/db \
  -p 53:53/tcp -p 53:53/udp -p 853:853/tcp -p 8443:8443/tcp \
  ioc2rpz
```

### Volumes

| Container Path | Purpose |
|---|---|
| `/opt/ioc2rpz/cfg` | Configuration file (`ioc2rpz.conf`) and TLS certificates |
| `/opt/ioc2rpz/db` | ETS database files persisted across restarts |

### Environment Variables

| Variable | Default | Description |
|---|---|---|
| `NODE_NAME` | `ioc2rpz` | Erlang node short name |
| `IO2Cookie` | `ioc2rpz` | Erlang distributed cookie |
| `IPv4` | — | IPv4 bind address |
| `IPv6` | — | IPv6 bind address |
| `CONF` | — | Path to configuration file |
| `DB` | `/opt/ioc2rpz/db` | Database directory |
| `CD` | `/opt/ioc2rpz` | Working directory |

You can pass a custom config file path via `-e CONF=./cfg/ioc2rpz2.conf`.

## Runtime Configuration

### vm.args

Located at `config/vm.args`. Controls Erlang VM settings:

```erlang
-sname ${NODE_NAME}       %% Node short name
-setcookie ${IO2Cookie}   %% Distributed Erlang cookie
```

For TLS distribution between Erlang nodes, uncomment the `-proto_dist inet_tls` and SSL options.

### sys.config.src

Located at `config/sys.config.src`. Application environment:

```erlang
[{ioc2rpz,[
  {ipv4,"${IPv4}"},
  {ipv6,"${IPv6}"},
  {conf_file,"${CONF}"},
  {db_dir,"${DB}"},
  {cd,"${CD}"}
]}].
```

These values are substituted from environment variables at release boot time.

### ioc2rpz.conf

The main configuration file (default: `./cfg/ioc2rpz.conf`). It is an Erlang term file loaded via `file:consult/1`. Key sections:

- `{srv, {...}}` — Server NS record, admin email, management keys, ACL
- `{cert, {...}}` — TLS certificate and key paths for DoT/REST
- `{key, {...}}` — TSIG keys for zone transfer authentication
- `{whitelist, {...}}` — Domain whitelists
- `{source, {...}}` — IOC feed sources (file, HTTP/HTTPS, shell)
- `{rpz, {...}}` — RPZ zone definitions

See `cfg/ioc2rpz.conf` for a fully commented example.

## Port Requirements

| Port | Protocol | Service | Description |
|---|---|---|---|
| 53 | UDP | DNS | SOA queries |
| 53 | TCP | DNS | AXFR, IXFR, SOA, management commands |
| 853 | TCP | DoT | DNS over TLS (same operations as TCP/53) |
| 8443 | TCP | HTTPS | REST API and DNS over HTTPS (DoH) |

All ports are required for full functionality. DoT (853) and REST/DoH (8443) are only active when a TLS certificate is configured.

## Certificate Setup

### DoT and REST API Certificates

Configure certificates in `ioc2rpz.conf`:

```erlang
{cert, {"cfg/ioc2rpz_dot.crt", "cfg/ioc2rpz_dot.key", ""}}.
```

The three fields are:
1. Certificate file path (PEM format)
2. Private key file path (PEM format)
3. CA certificate file path (empty string if not needed)

The same certificate is used for DoT (port 853), REST API (port 8443), and DoH.

### Generating Self-Signed Certificates (Development)

```bash
openssl req -x509 -newkey rsa:2048 -keyout cfg/ioc2rpz_dot.key \
  -out cfg/ioc2rpz_dot.crt -days 365 -nodes \
  -subj "/CN=ioc2rpz"
```

### Let's Encrypt Certificates (Production)

[Let's Encrypt](https://letsencrypt.org/) provides free, automated TLS certificates. Use [certbot](https://certbot.eff.org/) or any ACME client to obtain them.

#### Initial Setup

Install certbot and request a certificate for your server's FQDN:

```bash
# Install certbot (example for Debian/Ubuntu)
sudo apt install certbot

# Obtain a certificate using standalone mode (temporarily binds port 80)
sudo certbot certonly --standalone -d ns1.rpz-proxy.com
```

Certificates are saved to `/etc/letsencrypt/live/ns1.rpz-proxy.com/`. Copy or symlink them to your ioc2rpz cfg directory:

```bash
cp /etc/letsencrypt/live/ns1.rpz-proxy.com/fullchain.pem cfg/ioc2rpz_dot.crt
cp /etc/letsencrypt/live/ns1.rpz-proxy.com/privkey.pem cfg/ioc2rpz_dot.key
```

Then reference them in `ioc2rpz.conf`:

```erlang
{cert, {"cfg/ioc2rpz_dot.crt", "cfg/ioc2rpz_dot.key", ""}}.
```

#### Automatic Renewal

Let's Encrypt certificates expire after 90 days. Set up a cron job or systemd timer to renew and copy the updated files:

```bash
# /etc/cron.d/ioc2rpz-cert-renew
0 3 * * * root certbot renew --quiet --deploy-hook "cp /etc/letsencrypt/live/ns1.rpz-proxy.com/fullchain.pem /opt/ioc2rpz/cfg/ioc2rpz_dot.crt && cp /etc/letsencrypt/live/ns1.rpz-proxy.com/privkey.pem /opt/ioc2rpz/cfg/ioc2rpz_dot.key"
```

The `--deploy-hook` only runs when a certificate is actually renewed.

#### Docker with Let's Encrypt

When running in Docker, mount the certificate directory from the host:

```bash
docker run -d --name ioc2rpz \
  --restart always \
  --mount type=bind,source=/etc/letsencrypt/live/ns1.rpz-proxy.com,target=/opt/ioc2rpz/ssl,readonly \
  --mount type=bind,source=/path/to/cfg,target=/opt/ioc2rpz/cfg \
  --mount type=bind,source=/path/to/db,target=/opt/ioc2rpz/db \
  -p 53:53/tcp -p 53:53/udp -p 853:853/tcp -p 8443:8443/tcp \
  ioc2rpz
```

Update `ioc2rpz.conf` to point to the mounted path:

```erlang
{cert, {"ssl/fullchain.pem", "ssl/privkey.pem", ""}}.
```

#### DNS-01 Challenge (No Port 80 Required)

If port 80 is unavailable, use the DNS-01 challenge method instead:

```bash
sudo certbot certonly --manual --preferred-challenges dns -d ns1.rpz-proxy.com
```

This asks you to create a `_acme-challenge` TXT record. For automation, use a certbot DNS plugin matching your DNS provider (e.g., `certbot-dns-cloudflare`, `certbot-dns-route53`).

### Certificate Renewal (General)

Erlang automatically picks up replaced certificate files within approximately 2 minutes due to internal caching. Replace the files in-place without restarting the service. Do not let certificates expire — renew before expiration for uninterrupted service.

To pick up renewed certificates immediately, trigger a configuration reload (via the REST API or the `ioc2rpz-reload-cfg` DNS management command). On reload, the server compares a fingerprint of the certificate files; if they changed, it restarts the DoT and REST HTTPS listeners (`ioc2rpz_tls_sup_v6`, `ioc2rpz_rest_tls_sup_v6`) so the new certificate takes effect without waiting for the SSL cache to expire and without a full service restart. Existing connections continue until they close naturally.

### TLS Version

The default TLS version is configured in `include/ioc2rpz.hrl`:

```erlang
-define(TLSVersion, 'tlsv1.2-1.3').
```

## Monitoring and Health Checks

### REST API

The REST API (port 8443) provides server and RPZ statistics. Authentication uses TSIG management keys configured in the `srv` tuple.

```bash
# Server statistics
curl -u admin:password https://localhost:8443/api/v1/stats/serv -k

# RPZ zone statistics (indicator counts, serials, update times)
curl -u admin:password https://localhost:8443/api/v1/stats/rpz -k
```

### Erlang Shell Checks

If running in an interactive shell or attached to a running node:

```erlang
%% Check supervisor children are alive
supervisor:which_children(ioc2rpz_sup).
supervisor:count_children(ioc2rpz_tls_sup_v6).
supervisor:count_children(ioc2rpz_tcp_sup_v6).

%% Check ETS table sizes (memory usage)
ets:info(cfg_table, size).
ets:info(rpz_hotcache_table, size).
ets:info(rate_limits, size).

%% Check zone status
ets:match(cfg_table, {[rpz, '$1'], '$2'}).
```

### DNS Health Check

```bash
# Basic SOA query over UDP
dig @localhost -p 53 your-zone.rpz SOA +short

# Zone transfer over TCP
dig @localhost -p 53 your-zone.rpz AXFR +tcp -k keyfile

# DoT query
dig @localhost -p 853 +tls your-zone.rpz SOA

# Sample zone (built-in test zone)
dig @localhost sample-zone.ioc2rpz AXFR +tcp
```

## Log Messages Reference

ioc2rpz logs to stdout (Erlang group leader). Messages use two formats: plain text and CEF (Common Event Format).

### CEF Event Codes

| Code | Severity | Event | Description |
|---|---|---|---|
| 101 | Low | Bad DNS packet | Malformed DNS packet received |
| 102 | Low | Bad DNS request | Unparseable DNS request |
| 103 | Medium | Refused | DNS request refused |
| 104 | Medium | TSIG key not found | Request used unknown TSIG key |
| 105 | Medium | TSIG Bad MAC | TSIG signature verification failed |
| 106 | Medium | TSIG Bad time | TSIG timestamp out of range |
| 107 | Medium | Other TSIG error | Unclassified TSIG error |
| 108 | Medium | Wrong TSIG position | TSIG record in unexpected position |
| 109 | Low | Received DNS response | Unexpected DNS response received |
| 120 | Medium | RPZ not found | Requested RPZ zone does not exist |
| 121 | Low | RPZ not ready | RPZ zone is still loading/updating |
| 130 | Low | RPZ transfer error | Error during zone transfer |
| 131 | Low | RPZ transfer closed | Remote closed connection during transfer |
| 135 | High | REST MGMT denied | REST management request denied by ACL |
| 136 | High | MGMT request failed | Management request processing failed |
| 137 | High | Unsupported request | Unknown REST API endpoint |
| 138 | High | Zone not found | REST API referenced nonexistent zone |
| 201 | Low | RPZ transfer success | Zone transfer completed |
| 202 | Low | DNS Query | Standard DNS query processed |
| 221 | Low | DNS Notify | Notify sent to secondary server |
| 222 | Medium | DNS Notify error | Failed to send notify |
| 230 | High | MGMT request | Management operation executed |
| 301 | High | MGMT request denied | DNS management command denied |
| 429 | High | Too many requests | Rate limit exceeded |
| 501 | High | Possible DDoS | CVE-2004-0789 pattern detected |

### Common Log Messages

| Message Pattern | Meaning |
|---|---|
| `ioc2rpz <proc> started` | Listener process started (tcp_sup, tls_sup, udp_sup, etc.) |
| `Source: <name>, size: <size>, MD5: <hash>` | IOC source downloaded |
| `Source: <name>, got <N> indicators` | IOC parsing complete |
| `Error downloading feed <url> reason <reason>` | Source download failed (retries follow) |
| `Error reading file <path> reason <reason>` | Local file source read failed |
| `Unexpected response code <code>` | HTTP source returned non-200 status |
| `Bad IOC: <value>` | IOC entry failed regex parsing |
| `DB_sup got <table> table ownership` | ETS table ownership transferred |

## Troubleshooting

### Port Already in Use

If the server fails to start with `eaddrinuse`, another process is using port 53, 853, or 8443. Check with:

```bash
lsof -i :53
lsof -i :853
lsof -i :8443
```

Ensure `{reuseaddr, true}` is set on listen sockets (applied in the hardening fixes) to allow quick restarts.

### DoT Not Accepting Connections

Check that:
1. A valid certificate is configured in `ioc2rpz.conf` via the `{cert, ...}` tuple
2. TLS supervisor children are alive: `supervisor:count_children(ioc2rpz_tls_sup_v6).`
3. Port 853 is not blocked by a firewall
4. Certificate files are readable by the process

### Zone Transfer Fails

- Verify the TSIG key name and secret match between client and server
- Check that the RPZ zone name is correctly configured
- Look for CEF 104/105 events in logs (key not found / bad MAC)
- Ensure the client IP is in the zone's allowed transfer list

### Source Download Failures

- Check network connectivity to the source URL
- Look for `Error downloading feed` log messages
- The server retries 3 times with 3-second intervals (`?Src_Retry`, `?Src_Retry_TimeOut`)
- For HTTPS sources, ensure the remote server's TLS certificate is valid

### Source Removed From Config

After a configuration reload, if an RPZ zone still references a source that was removed from the config file, the zone update logs a warning (`Error: source <name> not found in config (removed?). Skipping.`) and continues building the zone from the remaining sources instead of crashing the update process. The reload-time validator (`validateCFGRPZ/3`) also logs the specific missing source and whitelist names and the affected RPZ zone (`RPZ <zone> was not loaded. Missing sources: [...]. Missing whitelists: [...]`). To resolve, either restore the source definition or remove the stale reference from the RPZ's `Sources` list.

### High Memory Usage

- Check ETS table sizes in the Erlang shell (see Monitoring section)
- `rate_limits` is swept every 10 seconds (`?RATE_LIMIT_WINDOW`) by `ioc2rpz_fun:cleanup_rate_limit_table/0`, removing expired per-client entries
- `rpz_hotcache_table` packet entries are swept every 900 seconds (`?HotCacheTime`) by `ioc2rpz_db:cleanup_hotcache/0`, removing expired cached zone packets
- Large IOC sources consume memory proportional to indicator count
- Consider reducing `?HotCacheTime` (default 900s) if hot cache grows too large

### Configuration Reload

Trigger a configuration reload without restarting:

```bash
# Via REST API
curl -u admin:password https://localhost:8443/api/v1/mgmt/reload -k

# Via DNS management (if enabled)
dig @localhost ioc2rpz-reload-cfg CH TXT +tcp
```

After reload, check logs for validation errors. Zones referencing removed sources will log warnings.
