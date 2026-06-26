#  ioc2rpz™ makes your threat intelligence actionable
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-24%2B-blue.svg)](https://www.erlang.org/)
[![Docker Hub](https://img.shields.io/docker/pulls/pvmdel/ioc2rpz.svg)](https://hub.docker.com/r/pvmdel/ioc2rpz)

## Table of Contents

- [Short Summary](#short-summary)
- [Overview](#overview)
- [ioc2rpz™ is a place where threat intelligence meets DNS](#ioc2rpz-is-a-place-where-threat-intelligence-meets-dns)
- [Architecture Overview](#architecture-overview)
- [How to use ioc2rpz™](#how-to-use-ioc2rpz)
- [ioc2rpz™ web interface](#ioc2rpz-web-interface)
- [Protocol Support](#protocol-support)
  - [Port Summary](#port-summary)
  - [UDP (Port 53)](#udp-port-53)
  - [TCP (Port 53)](#tcp-port-53)
  - [DNS over TLS / DoT (Port 853)](#dns-over-tls--dot-port-853)
  - [DNS over HTTPS / DoH (Port 443/8443)](#dns-over-https--doh-port-4438443)
  - [Rate Limiting](#rate-limiting)
  - [DNS NOTIFY](#dns-notify)
- [ioc2rpz™ vs ISC BIND vs other DNS](#ioc2rpz-vs-isc-bind-vs-other-dns)
- [Installation](#installation)
- [Docker container](#docker-container)
- [Environment Variables](#environment-variables)
- [Docker Compose](#docker-compose)
- [ioc2rpz™ on AWS](#ioc2rpz-on-aws)
- [Certificate Setup](#certificate-setup)
- [Building from Source](#building-from-source)
- [ioc2rpz™ management](#ioc2rpz-management)
  - [via DNS](#via-dns)
  - [via REST](#via-rest)
- [Monitoring & Health Checks](#monitoring--health-checks)
- [Troubleshooting](#troubleshooting)
- [Configuration file](#configuration-file)
- [Predefined configuration values](#predefined-configuration-values---includeioc2rpzhrl)
- [How the AXFR and IXFR caches are updated](#how-the-axfr-full-and-ixfr-incremental-caches-are-updated)
- [Hot cache](#hot-cache)
- [How to try ioc2rpz™](#how-to-try-ioc2rpz-or-sample-and-free-rpz-feeds-hosted-by-ioc2rpz)
- [Some free threat intelligence feeds](#some-free-threat-intelligence-feeds)
- [Further Documentation](#further-documentation)
- [References](#references)
- [Support / Supporters](#do-you-want-to-support-to-the-project)
- [Contact us](#contact-us)
- [License](#license)

## Short summary
ioc2rpz™: The DNS Security Solution - ioc2rpz™ is a powerful DNS server that transforms threat indicators into actionable Response Policy Zone (RPZ) feeds. It automates the update process, ensuring your network is protected against the latest threats, including malicious domains and IP addresses. By converting IOC feeds into RPZs, ioc2rpz™ acts as a crucial link between threat intelligence and DNS security, compatible with RPZ-supporting DNS servers like ISC Bind or PowerDNS.

## Overview

DNS is the control plane of the Internet. Usually DNS is used for good but:
- It can be used to track users locations and their behaviour;
- Malware uses DNS to command and control, exfiltrate data or redirect traffic;
- According with 2016 Cisco annual security report, 91.3% of malware use DNS;
- Advertisements companies usually use separate and obscure domains to show ads;
- Free DNS services (e.g. 1.1.1.1, 8.8.8.8, 9.9.9.9 etc) can help you to address some concerns but you can not define your own protection settings or ad filters.

<p align="center"><img src="https://github.com/Homas/ioc2rpz/blob/master/DNS_Malware.png"></p>

ISC Bind is a de facto a standard of a nameserver. With introduction of Response Policy Zones in the ISC BIND 9.8 it is became a simple task to monitor and contain malware on DNS layer. RPZ is supported on PowerDNS recursor 4.0.0 and later releases. Knot DNS is also partially supports RPZ.

In comparing with traditional network protection solutions a DNS server can handle millions of indicators without performance impact but there were no automated and efficient way to maintain response policy zones on primary DNS servers.

Usually indicators of compromise are distributed in plain text but in different formats and only a few providers of IOCs make them available via RPZ.

ioc2rpz™ is a custom DNS server which automatically converts indicators (e.g. malicious FQDNs, IPs) from various sources into RPZ feeds and automatically maintains/updates them. The feeds can be distributed to any open source and/or commercial DNS servers which support RPZ, e.g. ISC Bind, PowerDNS. You can run your own DNS server with RPZ filtering on a router, desktop, server and even Raspberry Pi. System memory is the only limitation.

With ioc2rpz™ you can define your own feeds, actions and prevent undesired communications.

## ioc2rpz™ is a place where threat intelligence meets DNS

ioc2rpz™ transforms IOC feeds into response policy zones (RPZ). You can mix feeds to generate a single RPZ or multiple RPZs. Trusted domains and IPs can be whitelisted. ioc2rpz™ supports expiration of indicators and accordingly rebuilds zones.  
![Alt ioc2rpz™](https://github.com/Homas/ioc2rpz/blob/master/IOC2RPZ.jpg)
The current release supports: local files, files/requests via http/https/ftp and shell scripts to access other resource types. You can use any file format if you can write a REGEX to extract indicators and indicators are separated by newline or/and return carriage chars (/n, /r, /r/n).

## Architecture Overview

ioc2rpz is built on Erlang/OTP with a supervision tree that ensures fault tolerance and automatic recovery. See [docs/architecture.md](docs/architecture.md) for full details.

```
ioc2rpz_app (application)
└── ioc2rpz_sup (supervisor)
    ├── ioc2rpz_db_sup     — ETS table heir process
    ├── ioc2rpz_tcp_sup    — TCP listener pool (5 workers)
    ├── ioc2rpz_udp_sup    — UDP listener
    ├── ioc2rpz_tls_sup    — TLS/DoT listener pool (5 workers) [if cert configured]
    └── ioc2rpz_rest_sup   — Cowboy HTTPS (REST API + DoH) [if cert configured]
```

Key modules:

| Module | Responsibility |
|--------|---------------|
| `ioc2rpz.erl` | TCP/TLS DNS worker — accept, parse, validate, respond |
| `ioc2rpz_udp.erl` | UDP DNS listener — SOA queries |
| `ioc2rpz_conn.erl` | IOC source fetching (HTTP/HTTPS/file/shell) |
| `ioc2rpz_db.erl` | ETS-based zone and packet cache |
| `ioc2rpz_sup.erl` | Main supervisor, configuration loading, zone scheduling |
| `ioc2rpz_rest.erl` | REST management API (Cowboy) |
| `ioc2rpz_doh.erl` | DNS-over-HTTPS handler |

## How to use ioc2rpz™
You can use ioc2rpz™ with any DNS server which supports Response Policy Zones e.g. recent versions of ISC BIND, PowerDNS and any commertial DNS server based on these products (e.g. Infoblox, Blue Cat, Efficient IP). A sample bind's configuration file (named.conf) is provided in the cfg folder.
<p align="center"><a href="http://www.youtube.com/watch?feature=player_embedded&v=bvhyMFa_mBM" target="_blank"><img src="https://github.com/Homas/ioc2rpz/blob/master/ioc2rpz_demo.png"></a></p>

## ioc2rpz™ web interface
[ioc2rpz.gui](https://github.com/Homas/ioc2rpz.gui) is a Management Web interface which is developed as a separate project. It is not required to run ioc2rpz™.

## Protocol Support

ioc2rpz™ listens on multiple transport protocols. All transports share the same query processing pipeline: rate limiting, TSIG validation, zone lookup, and response generation. See [docs/protocols.md](docs/protocols.md) for full protocol documentation.

### Port Summary

| Port | Protocol | Service | Condition |
|------|----------|---------|-----------|
| 53 | UDP | DNS queries (SOA only) | Always |
| 53 | TCP | DNS queries, AXFR/IXFR zone transfers, management | Always |
| 853 | TCP+TLS | DoT — same as TCP but encrypted | Requires `cert` config |
| 443/8443 | TCP+TLS | DoH (`/dns-query`) and REST API | Requires `cert` config |

### UDP (Port 53)

UDP is used for lightweight DNS queries, primarily SOA lookups. Responses exceeding 512 bytes (or the EDNS0 advertised buffer size) set the TC (truncation) bit per RFC 1035 §4.2.1, prompting clients to retry over TCP. Management commands are not supported over UDP.

```bash
dig @127.0.0.1 zone.ioc2rpz SOA -y hmac-sha256:keyname:base64key
```

### TCP (Port 53)

TCP handles zone transfers (AXFR/IXFR), SOA queries, and management commands. A pool of 5 pre-spawned accept workers handles incoming connections.

```bash
# AXFR zone transfer
dig @127.0.0.1 zone.ioc2rpz AXFR +tcp -y hmac-sha256:keyname:base64key

# IXFR incremental transfer
dig @127.0.0.1 zone.ioc2rpz IXFR=12345 +tcp -y hmac-sha256:keyname:base64key
```

### DNS over TLS / DoT (Port 853)

ioc2rpz™ supports RPZ distribution over DoT. The TLS listener starts automatically on port 853 when a `cert` record is present in the configuration. DoT supports the same operations as TCP (AXFR, IXFR, SOA, management).

- Supported TLS versions: 1.2 and 1.3 (`?TLSVersion = 'tlsv1.2-1.3'`)
- Connection reuse supported per RFC 7858 §3.4 (multiple queries per TLS session, 30-second idle timeout)
- TLS PIN is not supported
- DNS NOTIFY messages are sent unencrypted (plain UDP)
- Certificates auto-refresh when files are replaced on disk (up to ~2 minute delay due to Erlang SSL caching)

For certificate setup, see [Certificate Setup](#certificate-setup).

```bash
# SOA query over DoT
kdig @127.0.0.1 -p 853 zone.ioc2rpz SOA +tls -y hmac-sha256:keyname:base64key

# AXFR over DoT
dig @127.0.0.1 -p 853 zone.ioc2rpz AXFR +tls +tcp -y hmac-sha256:keyname:base64key
```

### DNS over HTTPS / DoH (Port 443/8443)

DoH provides DNS resolution over HTTPS via the `/dns-query` endpoint. Supported methods:

- **GET** with base64url-encoded DNS message in `?dns=` query parameter
- **POST** with `Content-Type: application/dns-message` body (max 4096 bytes; larger payloads receive HTTP 413)

Responses use `Content-Type: application/dns-message`. DoH carries the full DNS wire-format message and is processed through the same path as Do53/DoT, so it inherits the same TSIG authentication; zone transfers (AXFR/IXFR) remain TCP-only and are not served over DoH.

```bash
# DoH GET request
curl -H "Accept: application/dns-message" \
  "https://127.0.0.1:443/dns-query?dns=AAABAAABAAAAAAAAA3d3dwdleGFtcGxlA2NvbQAAAQAB" -k

# DoH POST request
curl -X POST -H "Content-Type: application/dns-message" \
  --data-binary @dns_query.bin "https://127.0.0.1:443/dns-query" -k
```

### Rate Limiting

DNS queries are rate-limited using an intelligent (hybrid) key so legitimate multi-zone clients are not penalized while query-name-variation abuse is blocked:

- **Provisioned zone + supported QTYPE** (`SOA`/`AXFR`/`IXFR`, class `IN`) and **recognized management commands** (class `CHAOS`/`TXT`) are tracked per `{client_IP, query_name, query_type}` — so a secondary polling/transferring several zones (e.g. `rpz1`, `rpz2`, `rpz3`) plus management from one IP is counted independently per zone+type.
- **Everything else** (unknown/unprovisioned zone, unsupported query type, wrong class, or an unrecognized management name) is aggregated per `{client_IP}`, so an attacker cannot bypass the limit by varying the query name.

When the limit is exceeded, the server returns a DNS `REFUSED` response.

| Parameter | Default | Macro |
|-----------|---------|-------|
| Window | 10 seconds | `?RATE_LIMIT_WINDOW` (10000 ms) |
| Max requests per window (granular: known zone+type / management) | 1 | `?MAX_REQUESTS_PER_WINDOW` |
| Max requests per window (aggregate: unknown zone / unsupported type) | 1 | `?MAX_UNKNOWN_REQUESTS_PER_WINDOW` |

Rate limiting applies to all DNS query transports (UDP, TCP, TLS, DoH). The window and threshold are configurable via macros in `include/ioc2rpz.hrl`.

### DNS NOTIFY

After a zone update (AXFR or IXFR), ioc2rpz™ sends DNS NOTIFY messages ([RFC 1996](https://tools.ietf.org/html/rfc1996)) over UDP to IP addresses configured in the RPZ `NotifyList`. This prompts secondary DNS servers to check the zone SOA serial and initiate a transfer if the serial has changed.

```erlang
%% NotifyList in RPZ config — last field before whitelists
{rpz, {"zone.ioc2rpz", ..., ["source1"], ["10.0.0.1", "10.0.0.2"], []}}.
```

## ioc2rpz™ vs ISC BIND vs other DNS:
- ioc2rpz™ was built to handle RPZ distribution only;
- ioc2rpz™ supports DoT (DNS over TLS) so nobody can easily eavesdrop on your RPZs/indicators;
- ioc2rpz™ supports DoH (DNS over HTTPS) for modern DNS clients;
- ioc2rpz™ supports as many RPZs as you need;
- ioc2rpz™ supports live/non cached zones. It creates zones by an incoming request;
- indicators can be pulled from different sources and via different protocols (e.g. via REST API calls);
- RPZs are automatically updated;
- IOC expiration time is used to remove expired indicators in a timely manner;
- Performance and zone transfer time/size/packets optimizations.

## Installation
The easiest way to deploy the service is using docker containers on the docker hub. [Deployment on Docker How-To](https://github.com/Homas/ioc2rpz/wiki/Deployment-on-Docker) you can find in ioc2rpz™ wiki.

## Docker container
ioc2rpz™ is available on the Docker Hub. Just look for ioc2rpz™.
Prerequisites:
- ioc2rpz™ doesn't contain a configuration file, you need to mount /opt/ioc2rpz/cfg to a directory on a host system with the configuration file (ioc2rpz.conf);
- ioc2rpz™ uses 53/udp (SOA requests only), 53/tcp (AXFR, IXFR, SOA, MGMT), 853/tcp (AXFR, IXFR, SOA, MGMT) and 8443/tcp (REST API, DoH) ports. The ports should be exposed to a host system;
- ioc2rpz™ saves ETS database into files for faster boot. You may mount /opt/ioc2rpz/db to a directory on a host system to preserve DB over restarts;
You can start ioc2rpz™ with the following command:
```
sudo docker run -d --name ioc2rpz --log-driver=syslog --restart always --mount type=bind,source=/home/ioc2rpz/cfg,target=/opt/ioc2rpz/cfg --mount type=bind,source=/home/ioc2rpz/db,target=/opt/ioc2rpz/db -p53:53 -p53:53/udp -p853:853 -p8443:8443 pvmdel/ioc2rpz

```
where /home/ioc2rpz/cfg, /home/ioc2rpz/db directories on a host system.  
You can pass a custom configuration file name via``-e`` parameter. E.g. ``-e CONF=./cfg/ioc2rpz2.conf``

## Environment Variables

The following environment variables are used by ioc2rpz™, primarily relevant for Docker deployments. They are defined in `config/sys.config.src` and `config/vm.args`.

| Variable | Default | Description |
|----------|---------|-------------|
| `IPv4` | — | IPv4 bind address |
| `IPv6` | — | IPv6 bind address |
| `CONF` | — | Path to configuration file (e.g. `./cfg/ioc2rpz.conf`) |
| `DB` | `/opt/ioc2rpz/db` | Database directory for ETS persistence |
| `CD` | `/opt/ioc2rpz` | Working directory |
| `NODE_NAME` | `ioc2rpz` | Erlang node short name |
| `IO2Cookie` | `ioc2rpz` | Erlang distributed cookie |

Pass environment variables to Docker with `-e`:
```bash
docker run -d --name ioc2rpz -e CONF=./cfg/custom.conf -e NODE_NAME=mynode ...
```

## Docker Compose
You can deploy ioc2rpz™ and ioc2rpz.gui using docker compose. The canonical docker-compose.yml file can be found in [ioc2rpz.dc](https://github.com/Homas/ioc2rpz.dc) repository.

Below is an example `docker-compose.yml` showing ioc2rpz with the web UI and a Let's Encrypt certbot sidecar:

```yaml
version: "3.8"
services:
  ioc2rpz:
    image: pvmdel/ioc2rpz
    container_name: ioc2rpz
    restart: always
    logging:
      driver: syslog
    ports:
      - "53:53/tcp"
      - "53:53/udp"
      - "853:853/tcp"
      - "8443:8443/tcp"
    volumes:
      - ./cfg:/opt/ioc2rpz/cfg
      - ./db:/opt/ioc2rpz/db
      - letsencrypt-certs:/opt/ioc2rpz/ssl:ro

  ioc2rpz-gui:
    image: pvmdel/ioc2rpz.gui
    container_name: ioc2rpz-gui
    restart: always
    ports:
      - "443:443"
    depends_on:
      - ioc2rpz

  certbot:
    image: certbot/certbot
    container_name: certbot
    volumes:
      - letsencrypt-certs:/etc/letsencrypt
      - letsencrypt-www:/var/www/certbot
    entrypoint: "/bin/sh -c 'trap exit TERM; while :; do certbot renew --quiet; sleep 12h; done'"

volumes:
  letsencrypt-certs:
  letsencrypt-www:
```

Update your `ioc2rpz.conf` to reference the mounted certificate path:
```erlang
{cert, {"ssl/live/ns1.rpz-proxy.com/fullchain.pem", "ssl/live/ns1.rpz-proxy.com/privkey.pem", ""}}.
```

## ioc2rpz™ on AWS
You can run ioc2rpz™ and ioc2rpz.gui on AWS. For relatively small deployments (several hundreds thousands indicators) even free tier is enough.
The video below shows how to setup ioc2rpz™ and ioc2rpz.gui on AWS using ECS.
<p align="center"><a href="http://www.youtube.com/watch?feature=player_embedded&v=C-y4p5TXt8s" target="_blank"><img src="https://github.com/Homas/ioc2rpz/blob/master/ioc2rpz_aws_setup.png"></a></p>

## Certificate Setup

TLS certificates are required for DoT (port 853), REST API (port 8443), and DoH. The same certificate is used for all TLS services. See [docs/deployment.md](docs/deployment.md) for full details.

### Self-Signed Certificate (Development)

```bash
openssl req -x509 -newkey rsa:2048 -keyout cfg/ioc2rpz_dot.key \
  -out cfg/ioc2rpz_dot.crt -days 365 -nodes -subj "/CN=ioc2rpz"
```

### Let's Encrypt (Production)

```bash
# Obtain certificate
sudo certbot certonly --standalone -d ns1.rpz-proxy.com

# Copy to ioc2rpz cfg directory
cp /etc/letsencrypt/live/ns1.rpz-proxy.com/fullchain.pem cfg/ioc2rpz_dot.crt
cp /etc/letsencrypt/live/ns1.rpz-proxy.com/privkey.pem cfg/ioc2rpz_dot.key
```

### Automatic Renewal

```bash
# /etc/cron.d/ioc2rpz-cert-renew
0 3 * * * root certbot renew --quiet --deploy-hook "cp /etc/letsencrypt/live/ns1.rpz-proxy.com/fullchain.pem /opt/ioc2rpz/cfg/ioc2rpz_dot.crt && cp /etc/letsencrypt/live/ns1.rpz-proxy.com/privkey.pem /opt/ioc2rpz/cfg/ioc2rpz_dot.key"
```

Certificates are also reloaded explicitly during a configuration reload (`ioc2rpz-reload-cfg`): when ioc2rpz detects that the certificate files changed, it restarts the TLS listeners with the new certificate immediately, instead of waiting for the ~2 minute Erlang SSL cache. Add an `ioc2rpz-reload-cfg` step to your renewal hook to apply new certificates without downtime.

### Docker Volume Mounting

When running in Docker, mount the certificate directory from the host:
```bash
docker run -d --name ioc2rpz \
  --mount type=bind,source=/etc/letsencrypt/live/ns1.rpz-proxy.com,target=/opt/ioc2rpz/ssl,readonly \
  ...
```

Reference in `ioc2rpz.conf`:
```erlang
{cert, {"ssl/fullchain.pem", "ssl/privkey.pem", ""}}.
```

Erlang automatically picks up replaced certificate files within ~2 minutes. Certificates are also explicitly reloaded during config reload (`ioc2rpz-reload-cfg`). Do not let certificates expire — renew before expiration for uninterrupted service.

## Building from Source

> **Note:** Building from source is intended for **development and testing**. For production, **Docker is the recommended deployment method** — see [Docker Compose](#docker-compose) and the [ioc2rpz.dc](https://github.com/Homas/ioc2rpz.dc) repository.

### Prerequisites

- **Erlang/OTP 24 or newer** (`erl -version` to check) and a matching [rebar3](https://www.rebar3.org).
- A C toolchain (for building dependencies) and `git`.

### Build & Run

```bash
# 1. Clone the repository
git clone https://github.com/Homas/ioc2rpz.git
cd ioc2rpz

# 2. Build a release
rebar3 release

# 3. Edit the configuration (see the minimal example below)
$EDITOR cfg/ioc2rpz.conf

# 4. Start the server (foreground console, or 'start' for background)
_build/default/rel/ioc2rpz/bin/ioc2rpz console
# or: _build/default/rel/ioc2rpz/bin/ioc2rpz start
```

By default ioc2rpz™ reads its configuration from `./cfg/ioc2rpz.conf`, listens on all network interfaces, and writes its DB backup to `./db`. Compile-time defaults (ports, paths, timers) live in [`include/ioc2rpz.hrl`](include/ioc2rpz.hrl).

### Minimal Configuration

A minimal `cfg/ioc2rpz.conf` with one TSIG key, one file source, and one RPZ zone:

```erlang
%% Server NS record, admin mailbox, management key(s), and management ACL
{srv,{"ns1.example.com","hostmaster.example.com",["mgmtkey"],["127.0.0.1","::1"]}}.

%% A TSIG key (name, algorithm, base64 secret) used for management / zone transfers
{key,{"mgmtkey","sha256","5Yvt70eJnf95+LJeI8H3TgKGeVparmMB7udA0pv/JRE="}}.

%% An IOC source: a local file parsed as a full (AXFR) feed of domains
{source,{"sample","file:cfg/small_ioc.txt","[:AXFR:]","^([0-9A-Za-z\.\-]+\\.[0-9A-Za-z\.\-]+)$","",0,0,0,"mixed",true}}.

%% An RPZ zone built from the source, served with the nxdomain action
{rpz,{"rpz.example.com",86400,3600,2592000,7200,"true","true","nxdomain",["mgmtkey"],"mixed",604800,86400,["sample"],[],[]}}.
```

See [docs/configuration.md](docs/configuration.md) for the authoritative field-by-field reference (source/RPZ tuple layout, SOA timers, key groups, certificates, etc.). To enable DoT (port 853), DoH, and the HTTPS REST API, add a `{cert,{...}}` entry — see [Certificate Setup](#certificate-setup).

### Development Shell (development/testing only)

The following commands are for local development and testing — not for production use:

```bash
# Compile only (no release)
rebar3 compile

# Run the EUnit test suite
rebar3 eunit
# Run tests for a single module
rebar3 eunit --module=ioc2rpz_fun

# Start an interactive shell with the application and all deps loaded
rebar3 shell
```

In `rebar3 shell` you can exercise the running system directly, for example:

```erlang
%% Inspect the listener pools and ETS tables
supervisor:which_children(ioc2rpz_sup).
ets:info(cfg_table, size).
ets:info(rpz_hotcache_table, size).

%% Trigger a configuration reload / forced zone update
ioc2rpz_sup:reload_config3(reload).
ioc2rpz_sup:update_all_zones(true).
```

## ioc2rpz™ management
### via DNS
ioc2rpz™ supports management over DNS/TCP or DoT. It is recommended to use DoT or REST API over DNS/TCP. The current version of ioc2rpz™ does not support a separate management IP/interface. In any case it is highly recommended to create a designated TSIG key (or keys) which will be used for management only. You can turn off management over DNS.  
Supported actions:
- ioc2rpz™ current status. Request ``ioc2rpz-status``, class ``CHAOS``, record ``TXT``. e.g.:  
```
dig +tcp -y dnsmkey_1:ayVnL+h2QKMszRVohrngagcEuIpN3RkecXKdwSa5WsHD5N4Y5R3NUMGM W8sIGv36gPkAtWtgarqKzN9tmHqEnA== @127.0.0.1 ioc2rpz-status TXT -c CHAOS
```
- Reload configuration file. RR Name ``ioc2rpz-reload-cfg``, RR Class ``CHAOS``, RR Type ``TXT``
- Update TSIG keys. RR Name ``ioc2rpz-update-tkeys``, RR Class ``CHAOS``, RR Type ``TXT``
- Full refresh of all zones. RR Name ``ioc2rpz-update-all-rpz``, RR Class ``CHAOS``, RR Type ``TXT``
- Full refresh a zone. RR Name ``zone_name``, RR Class ``CHAOS``, RR Type ``TXT``. E.g. full refresh of ``dga.ioc2rpz`` can be invoked by:
```
dig +tcp -y dnsmkey_1:ayVnL+h2QKMszRVohrngagcEuIpN3RkecXKdwSa5WsHD5N4Y5R3NUMGM W8sIGv36gPkAtWtgarqKzN9tmHqEnA== @127.0.0.1 dga.ioc2rpz TXT -c CHAOS
```
- Stop ioc2rpz™. RR Name ``ioc2rpz-terminate``, RR Class ``CHAOS``, RR Type ``TXT``
- Request a sample zone. RR Name ``sample-zone.ioc2rpz``, RR Class ``IN``, RR Type ``AXFR``
### via REST
REST API (port 8443/tcp) is the preferred management interface. For security reasons all management traffic must be encrypted and the REST API is not started if there is no SSL certificate. All endpoints accept both GET and POST methods.

Basic HTTP authentication is used to authenticate requests. Management TSIG keys are used for request authentication. A TSIG key name is used as the HTTP username and TSIG key as the password. Access to the REST API is restricted with the ACL defined in the `srv` record.

The API version segment supports both `v1` and `v1.0` (e.g., `/api/v1/stats/serv` or `/api/v1.0/stats/serv`).

The REST API supports JSON (default) and plain text output based on the `Accept` header:
- `application/json` — JSON output (default)
- `text/plain` — plain text output

```bash
# Example: plain text output
curl -u "keyname:base64key==" -k -H "Accept: text/plain" https://127.0.0.1:8443/api/v1/mgmt/update_tkeys
```

Text responses use the format:
```
status: ok
msg: TSIG keys were updated
```

#### Path Parameters

| Parameter | Description |
|-----------|-------------|
| `:rpz_name` | RPZ zone name (e.g. `dga.ioc2rpz`) |
| `:source_name` | IOC source name (e.g. `sample_fqdn`) |
| `:ioc` | Indicator to look up (FQDN or IP, e.g. `baddomain.com`) |

#### Statistics Endpoints

`/api/v1/stats/serv` — Server statistics (node name, total rules, memory usage)

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/stats/serv
```
```json
{
  "srv": {
    "node_name": "ioc2rpz@hostname",
    "srv_total_rules": 15000,
    "hot_cache_mem": "12.5 Mb",
    "axfr_table_mem": "45.2 Mb",
    "ixfr_table_mem": "8.1 Mb"
  },
  "rpz": [...],
  "sources": [...]
}
```

`/api/v1/stats/rpz` — RPZ zone statistics

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/stats/rpz
```
```json
{
  "rpz": [
    {
      "name": "malware.ioc2rpz",
      "status": "ready",
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

The `status` field (`ready`, `updating`, `forceAXFR`, `notready`) indicates whether the reported counts/serial are current; when `updating` or `forceAXFR` they reflect the last completed update. Counts and serial are preserved across a configuration reload rather than resetting to zero.

`/api/v1/stats/source` — Source statistics

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/stats/source
```
```json
{
  "sources": [
    {"name": "sample_fqdn", "ioc_count": 150}
  ]
}
```

#### Management Endpoints

`/api/v1/mgmt/reload_cfg` — Reload configuration file

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/mgmt/reload_cfg
```
Success: `{"status":"ok","msg":"Configuration reloaded"}`
Error (HTTP 520): `{"status":"error","msg":"Configuration reload error"}`

`/api/v1/mgmt/update_tkeys` — Reload TSIG keys from configuration

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/mgmt/update_tkeys
```
Success: `{"status":"ok","msg":"TSIG keys were updated"}`
Error (HTTP 520): `{"status":"error","msg":"TSIG keys update error"}`

`/api/v1/mgmt/terminate` — Graceful server shutdown

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/mgmt/terminate
```
Success: `{"status":"ok","msg":"Terminating"}`

#### Zone Update Endpoints

`/api/v1/update/all_rpz` — Force full refresh of all RPZ zones

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/update/all_rpz
```
Success: `{"status":"ok","msg":"All RPZ zones will be updated"}`

`/api/v1/update/:rpz_name` — Force full refresh of a specific zone

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/update/dga.ioc2rpz
```
Success: `{"status":"ok","msg":"RPZ dga.ioc2rpz will be updated"}`
Error (HTTP 520): `{"status":"error","msg":"RPZ dga.ioc2rpz not found"}`

#### Cache Management Endpoints

`/api/v1/cache/sources/clear/all` — Remove all sources from hot cache

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/cache/sources/clear/all
```
Success: `{"status":"ok","msg":"All sources were removed from the hotcache"}`

`/api/v1/cache/sources/clear/:source_name` — Remove a specific source from hot cache

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/cache/sources/clear/sample_fqdn
```
Success: `{"status":"ok","msg":"sample_fqdn source was removed from the hot cache"}`

`/api/v1/cache/sources/load/all` — Reload all sources into hot cache

```bash
curl -u "keyname:base64key==" -k https://127.0.0.1:8443/api/v1/cache/sources/load/all
```
Success: `{"status":"ok","msg":"All sources will loaded to the hot cache"}`

#### Feed & IOC Query Endpoints

`/api/v1/feed/:rpz_name` — Get indicators from an RPZ feed

Query parameter: `?type=fqdn|ip|both` (default: `both`)

```bash
curl -u "keyname:base64key==" -k "https://127.0.0.1:8443/api/v1/feed/malware.ioc2rpz?type=fqdn"
```
Success:
```json
{
  "status": "ok",
  "rpz": "malware.ioc2rpz",
  "iocs": ["baddomain.com", "evil.example.org"]
}
```
Error (HTTP 520): `{"status":"error","msg":"RPZ malware.ioc2rpz not found"}`

`/api/v1/ioc/:ioc` — Check if an indicator is blocked by any RPZ feed

Query parameter: `?tkey=keyname` — limit search to zones accessible by that key (optional)

```bash
curl -u "keyname:base64key==" -k "https://127.0.0.1:8443/api/v1/ioc/baddomain.com?tkey=dnsproxykey_1"
```
Success:
```json
{
  "ioc": "baddomain.com",
  "tkey": "dnsproxykey_1",
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
Error: `{"status":"error", "ioc": "nonexistent.com"}`

#### Unsupported Endpoints

Any unrecognized path returns HTTP 200 with:
```json
{"status":"error","msg":"Unsupported request"}
```

## Monitoring & Health Checks

### REST API Stats

```bash
# Server statistics
curl -u "keyname:key==" -k https://127.0.0.1:8443/api/v1/stats/serv

# RPZ zone statistics (indicator counts, serials, update times)
curl -u "keyname:key==" -k https://127.0.0.1:8443/api/v1/stats/rpz

# Source statistics
curl -u "keyname:key==" -k https://127.0.0.1:8443/api/v1/stats/source
```

### Erlang Shell Checks

If running in an interactive shell or attached to a running node:

```erlang
%% Check supervisor children
supervisor:which_children(ioc2rpz_sup).
supervisor:count_children(ioc2rpz_tls_sup_v6).
supervisor:count_children(ioc2rpz_tcp_sup_v6).

%% Check ETS table sizes (memory usage)
ets:info(cfg_table, size).
ets:info(rpz_hotcache_table, size).
ets:info(rate_limits, size).
```

### DNS Health Checks

```bash
# SOA query over UDP
dig @localhost -p 53 your-zone.rpz SOA +short

# Zone transfer over TCP
dig @localhost -p 53 your-zone.rpz AXFR +tcp -y hmac-sha256:keyname:base64key

# DoT query
dig @localhost -p 853 +tls your-zone.rpz SOA

# Sample zone (built-in test zone)
dig @localhost sample-zone.ioc2rpz AXFR +tcp
```

See [docs/deployment.md](docs/deployment.md) for full monitoring and log reference.

## Troubleshooting

| Issue | Solution |
|-------|----------|
| **Port already in use** | Check with `lsof -i :53` / `lsof -i :853`. `{reuseaddr, true}` is set on listen sockets for quick restarts. |
| **DoT not accepting connections** | Verify `{cert, ...}` is configured. Check TLS workers: `supervisor:count_children(ioc2rpz_tls_sup_v6).` Check port 853 is not firewalled. |
| **Zone transfer fails (TSIG mismatch)** | Verify key name and secret match between client and server. Look for CEF 104/105 events in logs. |
| **Source download failures** | Check network connectivity. Look for `Error downloading feed` in logs. Server retries 3 times with 3s delay. For HTTPS sources, ensure valid TLS certificate. |
| **High memory usage** | Check ETS table sizes in Erlang shell. `rate_limits` and `rpz_hotcache_table` are periodically cleaned. Consider reducing `?HotCacheTime`. |

See [docs/deployment.md](docs/deployment.md) for detailed troubleshooting.

## Configuration file
The configuration is an Erlang file. Every configuration option is an Erlang term so the configuration must comply with Erlang syntax. The server validates configuration entries on load/reload: TSIG key encoding, certificate file existence, regex compilation, and URL syntax. Invalid entries are logged and skipped. See [docs/configuration.md](docs/configuration.md) for the full configuration reference.

ioc2rpz™ supports the following configuration parameters:
- a single **srv** record (required);
- a single **cert** record (optional);
- zero or more **key** records (optional);
- zero or more **whitelist** records (optional);
- one or more **source** records (minimum one source is required);
- one or more **rpz** records (minimum one rpz is required).
### **srv** record
**srv** record is used to define server default values. It consists of:
- NS server name used in SOA record;
- an email address for SOA record (in SOA format);
- list of management TSIG keys (names only). Please refer [the management section](#ioc2rpz-management) for the details.
- list of ACL IP addresses for REST API access control.

Sample **srv** record:  
```
{srv,{"ns1.example.com","support.email.example.com",["dnsmkey_1","dnsmkey_2","dnsmkey_3"],["acl_ip1","acl_ip2"]}}.
```
### **cert** record
**cert** record is used to define a certificate and a private key for DNS over TLS, REST API, and DoH communications. For certificate generation and management, see [Certificate Setup](#certificate-setup).

It consists of:
- path to a file containing a certificate;
- path to a file containing a private PEM-encoded key;
- path to a file with PEM-encoded CA certificates.  

Sample **cert** record:  
```
{cert,{"cfg/cert.pem", "cfg/key.pem",	"cfg/cacerts.pem"}}.
```
### **include** record
**include** record allows to split ioc2rpz™ configuration into multiple files.

Sample **include** record:  
```
{include,"cfg/tkeys.include.cfg"}.
```
### **key** record
TSIG keys are used for authentication and authorization. It is recommended to use different TSIG keys for ioc2rpz™ management and zones transfers.  
**key** record consist of:
- TSIG key name;
- algorithm. ``md5``, ``sha256`` and ``sha512`` are supported;
- the key;
- (optional) list of key groups it belongs to.

Sample **key** records:  
```
{key,{"key_name_1","md5","ayVnL+h2QKMszRVohrngagcEuIpN3RkecXKdwSa5WsHD5N4Y5R3NUMGM W8sIGv36gPkAtWtgarqKzN9tmHqEnA=="}}.
{key,{"key_name_2","sha256","5Yvt70eJnf95+LJeI8H3TgKGeVparmMB7udA0pv/JRE="}}.
{key,{"key_name_3","sha512","03uuaGl9kqfenjRgIeCv6e29lVvMwviB1+cDX1I0jcVOcTU4jWFwRkfo3ULRMD+NGDfwzYvXkJ94FNEaAW4vzw==",["customers","public"]}}.
```
dnssec-keygen utility can be used to generate TSIG keys:
```bash
# MD5 (512-bit)
dnssec-keygen -a HMAC-MD5 -b 512 -n USER tsig-key

# SHA-256 (256-bit)
dnssec-keygen -a HMAC-SHA256 -b 256 -n USER tsig-key

# SHA-512 (512-bit)
dnssec-keygen -a HMAC-SHA512 -b 512 -n USER tsig-key
```
Please refer "dnssec-keygen" documentation for details. Keys can be assigned to groups for group-based authorization in RPZ zone access control.

### **key_group** record
Key groups provide group-based authorization for zone transfers and management. Instead of listing individual keys in `rpz` or `srv` records, you can assign keys to named groups and reference the group.

There are two ways to assign keys to groups:
1. Inline via the `key` record's optional 4th field: `{key, {Name, Alg, Secret, [Groups]}}`
2. Explicitly via a `key_group` record (shown below)

```erlang
{key_group, {GroupName, [KeyName1, KeyName2, ...]}}.
```

| Field     | Type            | Description |
|-----------|-----------------|-------------|
| GroupName | string          | Name of the group (e.g. `"customers"`) |
| Keys      | list of strings | List of TSIG key names belonging to this group |

Sample **key_group** records:
```erlang
{key_group, {"customers", ["dnsproxykey_1", "dnsproxykey_2"]}}.
{key_group, {"public", ["dnsproxykey_3"]}}.
```

Groups are referenced in `rpz` and `srv` records using the `{groups, ["group1", "group2"]}` tuple in the keys list:
```erlang
{rpz, {"zone.ioc2rpz", 7202, 3600, 2592000, 7200, "true", "true", "nxdomain",
       ["dnsproxykey_1", {groups, ["customers", "public"]}],
       "fqdn", 86400, 3600, ["source1"], [], []}}.
```

### **whitelist** record
Whitelists are used to prevent possible errors and blocking trusted domains and IP addresses. The whitelisted IOCs are removed from response policy zones. ioc2rpz™ does check only exact match, so it will not split or discard a network if a whitelisted IP address is included into a blocked subnet and vice versa. A whitelist is a text file or a feed of text data. Indicators should be separated by newline characters (/n,/r or both /n/r).  Whitelists must contain valid FQDNs and/or IP addresses. ioc2rpz™ supports unlimited count of indicators.  
**whitelists** record consist of:
- whitelist name;
- whitelist path. URLs(http/https/ftp) and local files are supported. Prefix "file:" is used for local files;
- REGEX which is used to extract indicators. A regular expression must be included in double quotes. If you specify an empty REGEX (`""`), a default REGEX will be used (`"^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$"`). `none` is used if no REGEX is required (the source already provides data in the required format).

Sample **whitelist** record:
```
{whitelist,{"whitelist_1","file:cfg/whitelist1.txt",none}}.
```
### **source** record
A source is a feed of malicious indicators. FQDNs, IPv4 and IPv6-addresses are supported. A source is a text file or a feed of text data. Indicators should be separated by newline/carriage return characters (/n,/r or both /r/n). ioc2rpz™ supports unlimited count of indicators.  
**source** record consist of:
- source name;
- source path for full source transfer (AXFR). URLs(http/https/ftp), local files and scripts are supported. Prefix **file:** is used for local files. Prefix **shell:** is used to execute a local script/command on a host/container which should return indicators and optional expiration date to STDOUT;
- source path for incremental source transfer (IXFR). AXFR,IXFR paths support keywords to shorten URLs and provide zone update timestamps:
  - **[:AXFR:]** - full AXFR path. Can be used only in IXFR paths;
  - **[:FTimestamp:]** - timestamp when the source was last time updated  (e.g. 1507946281)
  - **[:ToTimestamp:]** - current timestamp;
- REGEX which is used to extract indicators and their expiration time. The first match is an indicator, the second match is an expiration time. Expiration time is an optional parameter. A regular expression must be included in double quotes. If you specify an empty REGEX (`""`), a default REGEX will be used (`"^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$"`). `none` is used if no REGEX is required (the source already provides data in the required format).
Optional parameters (all or none must be used):
- UserID (used internally).
- Maximum # of IoCs.
- Full source update, hot cache time (in seconds).
- Incremental source update, hot cache time (in seconds).

HTTPS source downloads verify the remote server's TLS certificate. Sources with invalid or self-signed certificates will fail to download. For self-signed certs, use a `shell:` source with `curl --insecure`.

**Local file restrictions:** `file:` paths containing `..` (parent-directory traversal) are rejected for security; use a path without `..` (within the working/data directory).

**Shell command restrictions:** each pipeline segment's executable must be an absolute path (e.g. `/usr/bin/curl`) or a bare-name safe text utility (`sort`, `uniq`, `grep`, `sed`, `awk`, `gawk`, etc.); destructive commands and shells (`rm`, `bash`, `sh`, etc.) are blocked, and command substitution (`$(...)`, backticks) and output redirection (`>`, `>>`) are rejected. Rejected commands are not executed and are logged via CEF event 151 (executed commands via 150). See [docs/configuration.md](docs/configuration.md#shell-command-restrictions) for the full ruleset.

If a source returns fewer than 50% of its previous indicator count, the update is rejected and previous data is retained. This prevents degraded feeds from reducing RPZ coverage. Configurable via `?SOURCE_MIN_IOC_RATIO` macro.

Sample **source** record:
```
%% Local file source — indicators without expiration
{source,{"sample_fqdn","file:cfg/sample_ioc_fqdn.txt","[:AXFR:]",none}}.

%% Local file source — indicators with expiration timestamps (tab-separated)
{source,{"sample_expire","file:cfg/sample_ioc_expire.txt","[:AXFR:]","^([A-Za-z0-9][A-Za-z0-9\-\._]+)\t([0-9TZ:\-]+)$"}}.

%% Shell source — fetch RPZ via AXFR and extract CNAMEs
{source,{"base.rpz1","shell:/usr/bin/dig -y KEYNAME:TSIGKEY @127.0.0.1 base.rpz.ioc2rpz.local axfr | /bin/grep -e CNAME | /bin/grep -v '*.' | /usr/bin/awk -F '.base.rpz' '{print $1}'","",none}}.
```
Source **shell:** is used to extend ioc2rpz™ connectivity options which are natively a bit limited. The ioc2rpz™ container includes dig, grep, awk and python. E.g. you can mix different RPZ feeds or fetch data from a database.

**Shell Command Restrictions:** All executables must use absolute paths (e.g. `/usr/bin/curl` not `curl`). Destructive commands (`rm`, `bash`, `sh`, etc.) are blocked. Command substitution (`$(...)`, backticks) and output redirection (`>`, `>>`) are rejected. See [docs/configuration.md](docs/configuration.md) for full details.

**Local File Security:** File paths containing `..` (parent directory traversal) are rejected for security.

### **rpz** record
RPZ term defines a response policy zone.  
**rpz** record consist of:
- rpz name;
- SOA refresh time in seconds;
- SOA update retry time in seconds;
- SOA expiration time in seconds;
- SOA NXDomain TTL in seconds;
- Cache. Possible values: ``true`` or ``false``. ``true`` defines that the RPZ should be cached, ``false`` - non cached, live zone sources are downloaded and an RPZ generated by AXFR request. "Live" zones do not support incremental zone transfer. if RPZ feed is not cached anyway it is temporary stored into a hot cache. In case if a request timeouts from a client, we will be able to respond next time. AXFR time will be used to determine cache life;
- Wildcards. Possible values: ``true`` or ``false``. Defines if wildcard rules should be generated;
- Action. See the RPZ Actions table below;
- List of TSIG keys and key groups;
- Type of IOCs used in the RPZ: ``mixed``, ``fqdn``, ``ip``. It is used for optimization.
- Full zone update time in seconds (AXFR Time). Full Zone update and rebuild if MD5 for IOCs is different;
- Incremental zone update time  (IXFR Time). Sources should support incremental updates. "0" means no incremental zone updates;
- List of the sources;
- List of DNS servers (IP addresses) which should be notified on an RPZ updates (see [DNS NOTIFY](#dns-notify));
- List of whitelists.  

#### RPZ Actions

| Action | Config Value | Description |
|--------|-------------|-------------|
| NXDOMAIN | `"nxdomain"` | Return NXDOMAIN (domain does not exist) |
| NODATA | `"nodata"` | Return empty answer (domain exists, no records) |
| Passthru | `"passthru"` | Allow the query (exemption rule) |
| Drop | `"drop"` | Silently drop the query |
| TCP-Only | `"tcp-only"` | Force client to retry over TCP |
| Block NS | `"blockns"` | Block the authoritative nameserver |
| Redirect (domain) | `{"redirect_domain","example.com"}` | Redirect to a specified domain (alias for `local_cname`) |
| Redirect (IP) | `{"redirect_ip","127.0.0.1"}` | Redirect to a specified IP (alias for `local_a`/`local_aaaa`) |
| Local A | `{"local_a","127.0.0.1"}` | Return a custom IPv4 address |
| Local AAAA | `{"local_aaaa","fe80::1"}` | Return a custom IPv6 address |
| Local CNAME | `{"local_cname","www.example.com"}` | Return a CNAME redirect |
| Local TXT | `{"local_txt","Text Record"}` | Return a TXT record |

Multiple local actions can be combined in a list: `[{"local_a","127.0.0.1"},{"local_aaaa","fe80::1"},{"local_txt","Blocked"}]`

Sample **rpz** record:
```
{rpz,{"zone_name",soa_refresh, soa_update_retry,soa_expire,soa_nxdomain_ttl,"cache","wildcards","action",["key1","key2"],"Zone_type",AXFT_Time, IXFR_Time,["source1","source2"],["notify_ip1","notify_ip2"],["whitelist_1","whitelist_2"]}}.

{rpz,{"zone_name",soa_refresh, soa_update_retry,soa_expire,soa_nxdomain_ttl,"cache","wildcards","action",["key1","key2",{groups,["group1","group2"]}],"Zone_type",AXFT_Time, IXFR_Time,["source1","source2"],["notify_ip1","notify_ip2"],["whitelist_1","whitelist_2"]}}.

{rpz,{"mixed.ioc2rpz",7202,3600,2592000,7200,"true","true","passthru",["dnsproxykey_1","dnsproxykey_2"],"mixed",86400,3600,["sample_fqdn","sample_expire","sample_ip"],[],["whitelist_1","whitelist_2"]}}.

{rpz,{"mixed.ioc2rpz",7202,3600,2592000,7200,"true","true","passthru",["dnsproxykey_1","dnsproxykey_2",{groups,["public","ip2"]}],"mixed",86400,3600,["sample_fqdn","sample_expire","sample_ip"],[],["whitelist_1","whitelist_2"]}}.
```

<details>
<summary><strong>Sample configuration file</strong> (click to expand)</summary>

```erlang
{srv,{"ns1.rpz-proxy.com","support.rpz-proxy.com",["dnsmkey_3"],["127.0.0.1","10.42.0.10"]}}.
{cert,{"cfg/ioc2rpz_dot.crt", "cfg/ioc2rpz_dot.key",	""}}.

{key,{"dnsproxykey_1","md5","apXqLsDs90H213eV6LS9ryYp5tY8YTpkttOkRCve7dp1Zeob3SGAbaVU9BShpsW25MmR8mTiX5OY0Qetv977Yw=="}}.
{key,{"dnsproxykey_2","sha512","03uuaGl9kqfenjRgIeCv6e29lVvMwviB1+cDX1I0jcVOcTU4jWFwRkfo3ULRMD+NGDfwzYvXkJ94FNEaAW4vzw=="}}.
{key,{"dnsmkey_3","sha512","03uuaGl9kqfenjRgIeCv6e29lVvMwviB1+cDX1I0jcVOcTU4jWFwRkfo3ULRMD+NGDfwzYvXkJ94FNEaAW4vzw=="}}.

{whitelist,{"whitelist_1","file:cfg/sample_whitelist.txt",none}}.
{whitelist,{"whitelist_2","file:cfg/whitelist2.txt",""}}.

{source,{"sample_fqdn","file:cfg/sample_ioc_fqdn.txt","[:AXFR:]",none}}.
{source,{"sample_expire","file:cfg/sample_ioc_expire.txt","[:AXFR:]","^([A-Za-z0-9][A-Za-z0-9\\-\\._]+)\\t([0-9TZ:\\-]+)$"}}.
{source,{"sample_ip","file:cfg/sample_ioc_ip.txt","[:AXFR:]",none}}.

{rpz,{"localdata.ioc2rpz",7202,3600,2592000,7200,"false","true",[{"local_aaaa","fe80::1"},{"local_a","127.0.0.1"},{"local_a","127.0.0.2"},{"local_a","127.0.0.3"},{"local_a","127.0.0.4"},{"local_cname","www.example.com"},{"local_txt","Text Record www.example.com"},{"local_txt","Text Record 2"}],["dnsproxykey_1", "dnsproxykey_2"],"mixed",30,30,["sample_fqdn"],[],["whitelist_1","whitelist_2"]}}.
{rpz,{"dga.ioc2rpz",7202,3600,2592000,7200,"true","true","nodata",["dnsproxykey_1","dnsproxykey_2"],"fqdn",172800,3600,["sample_expire"],[],[]}}.
{rpz,{"mixed.ioc2rpz",7202,3600,2592000,7200,"true","true","passthru",["dnsproxykey_1", "dnsproxykey_2"],"mixed",86400,3600,["sample_fqdn","sample_expire"],[],["whitelist_1","whitelist_2"]}}.
{rpz,{"ip-block.ioc2rpz",7202,3600,2592000,7200,"false","true","nxdomain",["dnsproxykey_1","dnsproxykey_2"],"ip",172800,0,["sample_ip"],[],[]}}.
```

</details>

## Predefined configuration values - include/ioc2rpz.hrl
include/ioc2rpz.hrl contains pre-compiled parameters.

Standard parameters:
- ``MGMToDNS`` (true/false) - enabled management over DNS/TCP;
- ``DBStorage`` (ets) - defines DB storage for AXFR and IXFR caches. Current version supports ETS only;
- ``SaveETS`` (true/false) - defines if ETS AXFR/IXFR tables should be saved on disk;
- ``Port`` (numerical value, 1 - 65535) - defines a DNS port on which service is running;
- ``PortTLS`` (numerical value, 1 - 65535) - defines a DoT port on which service is running;
- ``PortREST`` (numerical value, 1 - 65535) - defines a HTTPs port on which service is running;
- ``TTL`` (numerical value, in seconds) - default TTL for DNS records/RPZ rules.
- ``DefConf`` (string) - default configuration file;
- ``DefDB`` (string) - default database path;
- ``logTS`` - if defined timestamp is added in log messages;
- ``debug`` - if defined debug log messages are printed;
- ``TLSVersion`` ('tlsv1.2-1.3') - supported TLS versions for DoT and REST API;

Optimization parameters:
- ``DNSPktMax`` (numerical value, 100 - 65535) - maximum packet size. Recommended values:
  - 16384 - minimal zone transfer size;
  - 65535 - minimal count of DNS packets;
- ``Compression`` (numerical value, 0 - 9) - Compression level (0 - no compression, 9 - highest compression). AXFR cache and tables on a disk store compressed data;
- ``ZoneRefTime`` (numerical value, in milliseconds) - defines zone refresh check interval;
- ``TCPTimeout`` (numerical value, in milliseconds) - defines TCP session timeout;
- ``HotCacheTime`` (numerical value, in seconds) - Hot cache time for IOCs, Rules, Packets. Live zones are stored in a hot cache;
- ``HotCacheTimeIXFR`` (numerical value, in seconds) - Hot cache time for IXFR IOCs in a hot cache. By default IXFR indicators are cached for a minute (even if it set to 0) because current serial is always rounded to a previous minute;
- ``RATE_LIMIT_WINDOW`` (numerical value, in milliseconds, default 10000) - rate limit window duration per IP;
- ``MAX_REQUESTS_PER_WINDOW`` (numerical value, default 1) - maximum DNS requests per IP per rate limit window;
- ``ShellMaxRespSize`` (numerical value, default 2 GiB) - maximum response size for shell command sources;
- ``SourcePullTimeout`` (numerical value, in milliseconds, default 300000) - timeout for source downloads (5 minutes);

## How the AXFR (full) and IXFR (incremental) caches are updated
- AXFR cache always contains prebuilt zones without SOA/NS/TSIG records. Prebuilt means all records are splitted by packets and labels were shortened/zipped.
- If a server receives an AXFR request it retrieves packets from the AXFR cache, adds SOA/NS records and TSIG if needed.
- AXFR zones update should be considered as a clean up procedure, which should periodically take place. Just to be sure that there is no desynchronization between the sources and the cache.
- For large zones, AXFR updates should be scheduled infrequently to minimize impact on a server's performance and amount of transferred data to all clients.
- All changes if it is possible should be done via incremental zone updates. In that case the AXFR cache will be rebuilt only in case if a zone was updated.
- [TODO] Due to an optimization, only last packet will be rebuilt for new IOCs and relevant and accordant packets for the expired IOCs.
- IXFR cache contains only IOCs and expiration dates. [TODO] and packets ID's (to make it possible rebuild the zone fast).
- RPZ record contains current zone Serial and Serial_IXFR. Serial_IXFR serve as a minimum incremental zone serial which is available for an incremental zone transfer.
- IXFR cache is flushed after full zone update (AXFR). Serial_IXFR = Serial. Clients will receive full zone update in any case, this is why it is important to have AXFR zone transfer infrequently.
- When IXFR cache is updated, AXFR cache must be rebuilt.
- If a zone does not support IXFR updates -> it doesn't saved in the IXFR table.
- Live zones are not cached in the AXFR, IXFR caches but the sources (IOCs) can be cached in the hot cache.

## Hot cache
All IOCs, Rules, Packets including live RPZs are stored in the hot cache. Pre-compiled parameters ``HotCacheTime``, ``HotCacheTimeIXFR`` define storage time.

## How to try ioc2rpz™ (or sample and free RPZ feeds hosted by ioc2rpz™)
### Disclaimer
The author assumes no responsibility or liability for any errors or omissions in the content of these RPZ feeds. The feeds are provided on an "as is" basis with no guarantees of completeness, accuracy, usefulness or timelines to demonstrate ioc2rpz™ technology only. The RPZ feeds service distribution may be interrupted or stopped w/o any advance notice. The author is not liable for any direct or indirect damages caused by using this service.

### RPZ Feeds
You may test ioc2rpz™ technology if you register on the [ioc2rpz™ community](https://ioc2rpz.net) with the following feeds:
- [notracking](https://github.com/notracking/hosts-blocklists);
- [Phishtank](https://www.phishtank.com/).

### Sample bind configuration
```
options {
  #This is just options for RPZs. Add other options as required
  recursion yes;
  response-policy {
    ####FQDN only zones
    ####Mixed zones
    zone "phishtank.ioc2rpz" policy nxdomain;
    ####IP only zones
  } qname-wait-recurse no break-dnssec yes;
};

key "ioc2rpz-YOUR-UNIQUE-KEY-NAME"{
  algorithm hmac-sha256; secret "ioc2rpz-YOUR-UNIQUE-KEY";
};

zone "phishtank.ioc2rpz" {
  type slave;
  file "/var/cache/bind/phishtank.ioc2rpz";
  masters {94.130.30.123  key "ioc2rpz-YOUR-UNIQUE-KEY-NAME";};
};
```
### Sample PowerDNS configuration
```
rpzMaster("94.130.30.123", "phishtank.ioc2rpz", {defpol=Policy.NXDOMAIN, tsigname="ioc2rpz-YOUR-UNIQUE-KEY-NAME", tsigalgo="hmac-sha256", tsigsecret="ioc2rpz-YOUR-UNIQUE-KEY"})
```
### Sample Infoblox configuration (import file)
```
header-responsepolicyzone,fqdn*,zone_format*,rpz_policy,substitute_name,view,zone_type,external_primaries,grid_secondaries,priority
responsepolicyzone,phishtank.ioc2rpz,FORWARD,Nxdomain,,default,responsepolicy,srv_1/94.130.30.123/FALSE/FALSE/TRUE/ioc2rpz-YOUR-UNIQUE-KEY-NAME/ioc2rpz-YOUR-UNIQUE-KEY/HMAC-SHA256,infoblox.localdomain/False/False/False,0
```

### Sample DIG (to get SOA)
```
dig  @94.130.30.123 -y hmac-sha256:ioc2rpz-YOUR-UNIQUE-KEY-NAME:ioc2rpz-YOUR-UNIQUE-KEY phishtank.ioc2rpz SOA

kdig @94.130.30.123 -y hmac-sha256:ioc2rpz-YOUR-UNIQUE-KEY-NAME:ioc2rpz-YOUR-UNIQUE-KEY phishtank.ioc2rpz SOA +tls
```  

## Some free threat intelligence feeds
- [Netlab](http://data.netlab.360.com)
- [awesome-threat-intelligence list on GitHub](https://github.com/hslatman/awesome-threat-intelligence)

You can find other IOC feeds on the wiki-page: https://github.com/Homas/ioc2rpz/wiki/IOC-Sources.

## Further Documentation

For detailed documentation, see the `docs/` directory:

- [docs/architecture.md](docs/architecture.md) — OTP supervision tree, module responsibilities, ETS tables, data flow
- [docs/configuration.md](docs/configuration.md) — Full configuration reference with all tuple types and options
- [docs/deployment.md](docs/deployment.md) — Build instructions, Docker deployment, certificates, monitoring, troubleshooting
- [docs/protocols.md](docs/protocols.md) — Protocol support (UDP/TCP/DoT/DoH), REST API, TSIG, rate limiting, DNS NOTIFY

## References
- [RFC-6895 Domain Name System (DNS) IANA Considerations](https://tools.ietf.org/html/rfc6895)
- [RFC-1035 Domain Names - Implementation and Specification](https://tools.ietf.org/html/rfc1035)
- [RFC-1995 Incremental Zone Transfer in DNS](https://tools.ietf.org/html/rfc1995)
- [DNS Response Policy Zones (RPZ)](https://tools.ietf.org/html/draft-ietf-dnsop-dns-rpz-00) + [vixie](https://tools.ietf.org/html/draft-vixie-dns-rpz-02)
- [RFC-2845 Secret Key Transaction Authentication for DNS (TSIG)](https://tools.ietf.org/html/rfc2845)
- [RFC-2104 HMAC: Keyed-Hashing for Message Authentication](https://tools.ietf.org/html/rfc2104)
- [RFC-4635 HMAC SHA TSIG Algorithm Identifiers](https://tools.ietf.org/html/rfc4635)
- [RFC-5966 DNS Transport over TCP - Implementation Requirements](https://tools.ietf.org/html/rfc5966)
- [RFC-1996 A Mechanism for Prompt Notification of Zone Changes (DNS NOTIFY)](https://tools.ietf.org/html/rfc1996)
- [Extension Mechanisms for DNS (EDNS(0))](https://tools.ietf.org/html/rfc6891) + [EDNS Option Codes](https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-11)
- [RFC-7873 Domain Name System (DNS) Cookies](https://tools.ietf.org/html/rfc7873)
- [RFC-7858 Specification for DNS over Transport Layer Security (TLS)](https://tools.ietf.org/html/rfc7858)
- [Cowboy Web Server](https://ninenines.eu)
- [Rebar3](https://www.rebar3.org)

<details>
<summary><strong>CEF Event Codes Reference</strong> (click to expand)</summary>

| Code | Severity | Event | Description |
|------|----------|-------|-------------|
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
| 130 | Low | RPZ transfer error | Error during zone transfer (RPZ transfer only) |
| 131 | Low | RPZ transfer closed | Remote closed connection during transfer (RPZ transfer only) |
| 140 | High | REST Basic auth failed | REST API basic authentication failed |
| 141 | High | REST Auth failed | REST API authorization failed |
| 145 | High | REST MGMT denied | REST management request denied by ACL |
| 146 | High | MGMT request failed | Management request processing failed |
| 147 | High | Unsupported request | Unknown REST API endpoint |
| 148 | High | Zone not found | REST API referenced nonexistent zone |
| 150 | Low | Shell command executed | Shell source command executed (info) |
| 151 | High | Shell command rejected | Shell source command rejected (security) |
| 201 | Low | RPZ transfer success | Zone transfer completed |
| 202 | Low | DNS Query | Standard DNS query processed |
| 221 | Low | DNS Notify | Notify sent to secondary server |
| 222 | Medium | DNS Notify error | Failed to send notify |
| 230 | High | MGMT request | Management operation executed |
| 301 | High | MGMT request denied | DNS management command denied |
| 429 | High | Too many requests | Rate limit exceeded |
| 501 | High | Possible DDoS | CVE-2004-0789 pattern detected |

</details>

# Do you want to support to the project?
You can support ioc2rpz™ project and ioc2rpz™ community (https://ioc2rpz.net)  via [GitHub Sponsor](https://github.com/sponsors/Homas) (recurring payments). To make one time donation you can use [PayPal](https://paypal.me/ioc2rpz) or Zelle (our email: zelle [at] ioc2rpz [.] net).

# Supporters
Shout out to **craSH** and **rrbone** who support my projects on [GitHub Sponsor](https://github.com/sponsors/Homas).

# Contact us
You can contact us by email: feedback(at)ioc2rpz[.]net or in [Telegram](https://t.me/ioc2rpz).

# License
Copyright 2017 - 2025 Vadim Pavlov ioc2rpz[at]gmail[.]com

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License.
You may obtain a copy of the License at  

    http://www.apache.org/licenses/LICENSE-2.0  

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
