# ioc2rpz Configuration Reference

This document provides a complete reference for the ioc2rpz configuration file. The configuration uses Erlang term syntax — each entry is an Erlang tuple terminated with a period (`.`). Comments start with `%`.

## Table of Contents

- [Configuration File Location](#configuration-file-location)
- [Configuration Tuple Types](#configuration-tuple-types)
  - [srv — Server Record](#srv--server-record)
  - [cert — TLS Certificate](#cert--tls-certificate)
  - [include — Include Files](#include--include-files)
  - [key — TSIG Keys](#key--tsig-keys)
  - [key_group — Key Groups](#key_group--key-groups)
  - [whitelist — Whitelists](#whitelist--whitelists)
  - [source — IOC Sources](#source--ioc-sources)
  - [rpz — Response Policy Zones](#rpz--response-policy-zones)
- [Source Types](#source-types)
- [Regex and Feed Format](#regex-and-feed-format)
- [RPZ Configuration Options](#rpz-configuration-options)
- [TLS / DoT Configuration](#tls--dot-configuration)
- [REST API Configuration](#rest-api-configuration)
- [Management Commands](#management-commands)
- [Compile-Time Parameters](#compile-time-parameters)
- [Environment Variables](#environment-variables)

---

## Configuration File Location

By default ioc2rpz reads its configuration from `./cfg/ioc2rpz.conf`. This can be changed via the `DefConf` macro in `include/ioc2rpz.hrl` or by passing a custom path through the `CONF` environment variable in `config/sys.config.src`.

**File permissions:** On startup and on every configuration reload (including `include`d files), ioc2rpz checks the configuration file's permissions and logs a warning if the file is world-writable. A world-writable config can be silently tampered with by any local user. The check is advisory and non-fatal — parsing continues — but you should restrict access with `chmod o-w cfg/ioc2rpz.conf`.

---

## Configuration Tuple Types

### srv — Server Record

Defines global server settings. Exactly one `srv` record is required.

```erlang
{srv, {NS_Name, Email, MgmtKeys, ACL}}.
```

| Field     | Type            | Description |
|-----------|-----------------|-------------|
| NS_Name   | string          | NS server name used in SOA records (e.g. `"ns1.example.com"`) |
| Email     | string          | SOA email in DNS format — use dots, not `@` (e.g. `"admin.example.com"`) |
| MgmtKeys  | list            | List of TSIG key names authorized for management operations. Supports key groups via `{groups, ["group_name"]}` |
| ACL       | list of strings | IP addresses allowed to access the REST API (e.g. `["127.0.0.1", "::1"]`) |

**Example:**
```erlang
{srv, {"ns1.rpz-proxy.com", "support.rpz-proxy.com",
       ["dnsmkey_1", {groups, ["mgmt"]}],
       ["127.0.0.1", "::1"]}}.
```

---

### cert — TLS Certificate

Defines the TLS certificate for DoT (port 853) and REST API (port 8443). Optional — if omitted, DoT and REST API are not started.

```erlang
{cert, {CertFile, KeyFile, CACertFile}}.
```

| Field      | Type   | Description |
|------------|--------|-------------|
| CertFile   | string | Path to the PEM-encoded certificate file |
| KeyFile    | string | Path to the PEM-encoded private key file |
| CACertFile | string | Path to PEM-encoded CA certificates. Use `""` if not needed |

**Example:**
```erlang
{cert, {"cfg/ioc2rpz_dot.crt", "cfg/ioc2rpz_dot.key", ""}}.
```

---

### include — Include Files

Splits configuration across multiple files. The included file uses the same Erlang term syntax.

```erlang
{include, FilePath}.
```

**Example:**
```erlang
{include, "cfg/tkeys.include.cfg"}.
```

---

### key — TSIG Keys

Defines TSIG keys for authentication and authorization of zone transfers and management operations. Zero or more `key` records are allowed.

```erlang
{key, {Name, Algorithm, Secret}}.
{key, {Name, Algorithm, Secret, Groups}}.
```

| Field     | Type            | Description |
|-----------|-----------------|-------------|
| Name      | string          | Unique key name |
| Algorithm | string          | One of `"md5"`, `"sha256"`, `"sha512"` |
| Secret    | string          | Base64-encoded key data |
| Groups    | list of strings | (Optional) Key group memberships for group-based authorization |

**Examples:**
```erlang
%% Key without groups
{key, {"dnsproxykey_1", "md5",
       "apXqLsDs90H213eV6LS9ryYp5tY8YTpkttOkRCve7dp1Zeob3SGAbaVU9BShpsW25MmR8mTiX5OY0Qetv977Yw=="}}.

%% Key with groups
{key, {"dnsmkey_1", "sha256",
       "5Yvt70eJnf95+LJeI8H3TgKGeVparmMB7udA0pv/JRE=",
       ["mgmt", "public"]}}.
```

Generate keys with `dnssec-keygen`:
```bash
dnssec-keygen -a HMAC-MD5 -b 512 -n USER tsig-key
dnssec-keygen -a HMAC-SHA256 -b 256 -n USER tsig-key
dnssec-keygen -a HMAC-SHA512 -b 512 -n USER tsig-key
```

---

### key_group — Key Groups

Defines named groups of TSIG keys for group-based authorization. Zero or more `key_group` records are allowed. Groups can also be assigned inline via the `key` record's optional 4th field.

```erlang
{key_group, {GroupName, KeyNames}}.
```

| Field     | Type            | Description |
|-----------|-----------------|-------------|
| GroupName | string          | Unique group name (e.g. `"customers"`) |
| KeyNames  | list of strings | List of TSIG key names belonging to this group |

**Examples:**
```erlang
{key_group, {"customers", ["dnsproxykey_1", "dnsproxykey_2"]}}.
{key_group, {"public", ["dnsproxykey_3"]}}.
```

Groups are referenced in `rpz` and `srv` records using the `{groups, ["group1"]}` tuple:
```erlang
["dnsproxykey_1", {groups, ["customers", "public"]}]
```

Note: Keys can also be assigned to groups inline via the `key` record's 4th field — see [key — TSIG Keys](#key--tsig-keys).

---

### whitelist — Whitelists

Defines whitelists of trusted domains/IPs that are excluded from RPZ zones. Zero or more `whitelist` records are allowed.

```erlang
{whitelist, {Name, Path, Regex}}.
```

| Field | Type   | Description |
|-------|--------|-------------|
| Name  | string | Unique whitelist name |
| Path  | string | Feed location. Supports the same source types as `source` records: `http://`, `https://`, `ftp://`, `file:`, `shell:`. See [Source Types](#source-types) for details and authentication |
| Regex | string or `none` | Regex to extract indicators. See [Regex Behavior](#regex-behavior) for details on `none`, `""`, and custom patterns |

**Examples:**
```erlang
%% Local file, no regex needed (one indicator per line)
{whitelist, {"whitelist_1", "file:cfg/whitelist1.txt", none}}.

%% Local file, default regex
{whitelist, {"whitelist_2", "file:cfg/whitelist2.txt", ""}}.

%% Remote whitelist with custom regex
{whitelist, {"whitelist_remote", "https://example.com/whitelist.txt",
             "^([A-Za-z0-9][A-Za-z0-9\\-\\._]+)$"}}.

%% Whitelist from a shell command
{whitelist, {"whitelist_shell", "shell:/usr/bin/cat /etc/trusted_domains.txt", none}}.
```

---

### source — IOC Sources

Defines threat intelligence feeds. One or more `source` records are required.

```erlang
%% Minimal form (4 fields)
{source, {Name, AXFR_URL, IXFR_URL, Regex}}.

%% Extended form (6 fields)
{source, {Name, AXFR_URL, IXFR_URL, Regex, UserID, MaxIOC}}.

%% Full form (10 fields)
{source, {Name, AXFR_URL, IXFR_URL, Regex, UserID, MaxIOC,
          HotCacheTime, HotCacheTimeIXFR, IOC_Type, KeepInCache}}.
```

| Field            | Type    | Description |
|------------------|---------|-------------|
| Name             | string  | Unique source name |
| AXFR_URL         | string  | Full update URL. Supports `http://`, `https://`, `ftp://`, `file:`, `shell:` prefixes. See [Source Types](#source-types) for details and authentication |
| IXFR_URL         | string  | Incremental update URL. Use `""` for no IXFR. Supports keywords (see below) |
| Regex            | string or `none` | Regex to extract indicators. First capture = indicator, second capture = expiration time (optional). See [Regex Behavior](#regex-behavior) |
| UserID           | string  | Internal user ID. Use `""` or `"0"` if not needed |
| MaxIOC           | integer | Maximum number of IOCs. `0` = unlimited |
| HotCacheTime     | integer | AXFR hot cache time in seconds. `0` = use global default |
| HotCacheTimeIXFR | integer | IXFR hot cache time in seconds. `0` = use global default |
| IOC_Type         | string  | `"mixed"`, `"fqdn"`, or `"ip"` |
| KeepInCache      | boolean | `true` to keep source data in hot cache |

**IXFR URL Keywords:**

| Keyword          | Expands to |
|------------------|------------|
| `[:AXFR:]`       | The full AXFR URL |
| `[:FTimestamp:]`  | Timestamp of last source update (Unix epoch) |
| `[:ToTimestamp:]` | Current timestamp (Unix epoch) |

---

### rpz — Response Policy Zones

Defines RPZ zones. One or more `rpz` records are required.

```erlang
{rpz, {ZoneName, SOA_Refresh, SOA_Retry, SOA_Expire, SOA_NXDomain_TTL,
       Cache, Wildcards, Action, Keys,
       IOC_Type, AXFR_Time, IXFR_Time,
       Sources, NotifyList, Whitelists}}.
```

| Field            | Type    | Description |
|------------------|---------|-------------|
| ZoneName         | string  | RPZ zone name (e.g. `"dga.ioc2rpz"`) |
| SOA_Refresh      | integer | SOA refresh timer in seconds |
| SOA_Retry        | integer | SOA retry timer in seconds |
| SOA_Expire       | integer | SOA expiration timer in seconds |
| SOA_NXDomain_TTL | integer | SOA negative TTL in seconds |
| Cache            | string  | `"true"` = cached zone, `"false"` = live zone (rebuilt on each request) |
| Wildcards        | string  | `"true"` = generate wildcard rules, `"false"` = exact match only |
| Action           | string or tuple or list | RPZ action (see [RPZ Actions](#rpz-actions)) |
| Keys             | list    | TSIG key names and/or key groups for authorization |
| IOC_Type         | string  | `"mixed"`, `"fqdn"`, or `"ip"` |
| AXFR_Time        | integer | Full zone update interval in seconds |
| IXFR_Time        | integer | Incremental update interval in seconds. `0` = no IXFR |
| Sources          | list    | List of source names to include in this zone |
| NotifyList       | list    | IP addresses to send DNS NOTIFY on zone updates |
| Whitelists       | list    | Whitelist names to apply to this zone |

**Example:**
```erlang
{rpz, {"dga.ioc2rpz", 7202, 3600, 2592000, 7200,
       "true", "true", "nodata",
       ["dnsproxykey_1", "dnsproxykey_2"],
       "fqdn", 172800, 3600,
       ["sample_fqdn", "sample_expire"],
       [], []}}.
```

---

## Source Types

All source types below apply to both `source` and `whitelist` records.

### HTTP / HTTPS

Fetches indicators from a remote URL. ioc2rpz uses Erlang's `httpc` client with a `Mozilla` User-Agent header, cookies enabled, and a configurable timeout (`SourcePullTimeout`, default 5 minutes). Failed downloads are retried up to 3 times (`Src_Retry`) with a 3-second delay between attempts.

```erlang
{source, {"feed_name",
           "https://example.com/feed.txt",
           "[:AXFR:]",
           "^([A-Za-z0-9][A-Za-z0-9\\-\\._]+)$"}}.
```

**Basic authentication** is supported by embedding credentials in the URL:

```erlang
{source, {"authed_feed",
           "https://user:password@example.com/protected/feed.txt",
           "[:AXFR:]", none}}.
```

For feeds that require custom headers, API tokens, or more complex authentication, use a `shell:` source with `curl` instead (see below).

**TLS certificate verification:** HTTPS source downloads verify the remote server's TLS certificate against the system CA trust store (`verify_peer`), including hostname verification. Sources served with an invalid, expired, or self-signed certificate — or whose certificate does not match the hostname — will fail to download (the connection is rejected at the TLS layer and the download is retried, then logged as a download error). For development/testing against a host with a self-signed certificate, use a plain `http://` URL or a `shell:` source with `curl --insecure` instead.

### Local File

Reads indicators from a file on the local filesystem. Prefix the path with `file:`.

```erlang
{source, {"local_feed", "file:cfg/sample_ioc_fqdn.txt", "[:AXFR:]", none}}.
```

**Security note:** File paths containing `..` (parent-directory traversal) segments are rejected — a source such as `file:../../etc/passwd` will not be read and is logged as an error. Use a path within the server's working directory or its configured data directory (for example `cfg/...` or an absolute path under your ioc2rpz config directory).

### FTP

Fetches indicators via FTP. Basic authentication is supported via URL credentials:

```erlang
{source, {"ftp_feed", "ftp://user:password@ftp.example.com/indicators.txt", "", none}}.
```

### Shell Command

Executes a shell command via `os:cmd/1` and reads indicators from stdout. Prefix with `shell:`. Pipes (`|`), quoting, and text-processing filters are supported, so multi-stage feed pipelines work as expected. For security, the command is validated before execution (see [Shell Command Restrictions](#shell-command-restrictions) below).

```erlang
{source, {"shell_feed",
           "shell:/usr/bin/curl -s https://example.com/feed.csv | /bin/grep -v '^#' | /usr/bin/awk -F',' '{print $2}'",
           "", none}}.
```

Shell sources are useful when you need to:
- Authenticate with API tokens or custom headers (`curl -H "Authorization: Bearer TOKEN"`)
- Transform or filter data before ioc2rpz processes it (pipe through `grep`, `awk`, `sed`, etc.)
- Fetch data from non-HTTP sources (databases, RPZ transfers via `dig`, etc.)
- Chain multiple commands together

**Examples:**

```erlang
%% Fetch with API key header
{source, {"api_feed",
           "shell:/usr/bin/curl -s -H 'Authorization: Bearer MY_API_KEY' https://api.example.com/indicators",
           "", none}}.

%% Transfer an RPZ from another DNS server and extract domains
{source, {"rpz_transfer",
           "shell:/usr/bin/dig -y KEYNAME:TSIGKEY @10.0.0.1 base.rpz.example.com axfr | /bin/grep CNAME | /usr/bin/awk -F '.base.rpz' '{print $1}'",
           "", none}}.

%% Fetch from a database
{source, {"db_feed",
           "shell:/usr/bin/mysql -u user -pPASS -h db.example.com -N -e 'SELECT domain FROM blocklist' threatdb",
           "", none}}.

%% Combine multiple feeds
{source, {"combined",
           "shell:/usr/bin/curl -s https://feed1.example.com/list.txt https://feed2.example.com/list.txt",
           "", none}}.
```

The maximum response size for shell sources is controlled by the `ShellMaxRespSize` macro (default: 2 GB). The ioc2rpz Docker container includes `dig`, `grep`, `awk`, `curl`, and `python`.

#### Shell Command Restrictions

For security, every `shell:` command is validated by `ioc2rpz_fun:validate_shell_cmd/1` before it is passed to `os:cmd/1`. A command that fails validation is **not executed**; it is logged as a CEF security warning (event code 151) and the source is skipped. Successful executions are logged at info level (CEF event code 150).

The command is split into pipeline segments on unquoted `|`, and the following rules apply:

- **Executables must be absolute paths or allowlisted utilities.** Each segment's executable (its first token) must either be an absolute path (e.g. `/usr/bin/curl`, not `curl`) **or** the bare name of a safe text-processing utility: `sort`, `uniq`, `grep`, `egrep`, `fgrep`, `sed`, `awk`, `gawk`, `cut`, `tr`, `head`, `tail`, `cat`, `comm`, `wc`, `tee`. These coreutils may be used by bare name because their location varies across distributions.
- **Destructive commands and shells are blocked** even when given an absolute path: `rm`, `mkfs`, `dd`, `chmod`, `chown`, `shutdown`, `reboot`, `kill`, `killall`, `mv`, `eval`, `exec`, `source`, `bash`, `sh`, `zsh`, `csh`, `ksh`. (Interpreters such as `php`, `python`, `python3`, `perl`, `ruby`, and `node` are allowed when invoked with an absolute path, since they are commonly used for feed processing.)
- **Command substitution and output redirection are rejected:** backticks, `$(...)`, and unquoted `>` / `>>`.
- **Allowed:** pipes (`|`), single/double quotes, backslashes, parentheses inside awk/gawk/sed programs, `&` inside quoted URL parameters, and other standard text-processing characters — all of which are essential for real feed pipelines. A `|` (or `\|`) inside quotes is treated as literal, not a pipeline separator.

**Example of a valid command** (absolute fetcher plus bare-name filters):

```erlang
{source, {"phishtank",
           "shell:/usr/bin/curl -sL https://example.com/feed.csv | /usr/bin/gawk 'match($0,/p/,a) {print a[1]}' | sort | uniq | grep '^[a-zA-Z0-9\\-\\.]*$'",
           "[:AXFR:]", "^([0-9A-Za-z\\.\\-]+)$"}}.
```

**Examples that are rejected:** `curl http://x | sort` (relative `curl`), `/bin/rm -rf /tmp` (blocked command), `/usr/bin/curl http://x | /bin/bash` (shell interpreter), `/usr/bin/curl http://x > /tmp/out` (output redirection), `/usr/bin/curl http://x/$(id)` (command substitution).

### Incremental Updates with Keywords

```erlang
%% IXFR uses the same URL as AXFR
{source, {"feed", "https://example.com/feed.txt", "[:AXFR:]", none}}.

%% IXFR with timestamp parameters
{source, {"feed", "https://example.com/feed.txt",
           "https://example.com/feed.txt?from=[:FTimestamp:]&to=[:ToTimestamp:]",
           none}}.
```

---

## Regex and Feed Format

Both `source` and `whitelist` records use the same `Regex` field and feed processing logic. This section covers how indicators are extracted from feed data.

### Feed Format

Feed files (whether local, remote, or from shell output) contain one indicator per line. Lines are separated by newline characters (`\n`, `\r`, or `\r\n`). Empty lines are ignored.

```
google.com
example.com
192.168.1.0/24
```

### Regex Behavior

The `Regex` field controls how indicators are extracted from each line. The behavior depends on the value:

| Value | Behavior |
|-------|----------|
| `none` | No regex applied. Each non-empty line is used as-is, one indicator per line. Expiration dates are not supported |
| `""` (empty string) | The default regex is applied (see below). Extracts the first FQDN-like token from each line, stripping trailing non-hostname characters. Expiration dates are not supported |
| `"<pattern>"` | A custom PCRE regex. The first capture group `(...)` extracts the indicator. An optional second capture group extracts the expiration timestamp (sources only — whitelists ignore expiration) |

### Default Regex

When `""` (empty string) is specified, the following regex is used:

```
^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$
```

This matches a leading hostname/domain token (starting with an alphanumeric character, followed by alphanumerics, hyphens, dots, or underscores) and discards everything after the first non-hostname character. It is useful for feeds where indicators are mixed with other text on each line.

### Indicator Expiration Dates

Sources (not whitelists) support optional per-indicator expiration dates. To use this feature, provide a custom regex with two capture groups:

1. First capture group — the indicator (FQDN or IP)
2. Second capture group — the expiration timestamp

Supported timestamp formats:
- `YYYY-MM-DDThh:mm:ss` (ISO 8601 with `T` separator)
- `YYYY-MM-DD hh:mm:ss` (space separator)

If the second capture group is empty or the timestamp cannot be parsed, the indicator is treated as non-expiring (expiration = 0). Expired indicators are automatically removed during zone rebuilds.

**Example source with expiration:**
```erlang
%% Feed format: "malware.example.com  2025-06-15T00:00:00"
{source, {"expiring_feed", "file:cfg/ioc_expire.txt", "[:AXFR:]",
           "^([A-Za-z0-9][A-Za-z0-9\\-\\._]+)[\\s|\\t]+([0-9:TZ -.]+)?$"}}.
```

Note: `none` and `""` (default regex) do not support expiration — all indicators are treated as non-expiring.

### Regex Syntax

ioc2rpz uses Erlang's `re` module (PCRE-compatible). Key rules:

- The first capture group `(...)` extracts the indicator (FQDN or IP)
- The optional second capture group extracts the expiration timestamp
- Backslashes must be doubled in Erlang strings (`\\` for a literal `\`)

**Common regex patterns:**

```erlang
%% No regex — one indicator per line, already clean
none

%% Default regex — extract first FQDN-like token from each line
""

%% Extract FQDN from hosts-file format (0.0.0.0 domain.com)
"^0\\.0\\.0\\.0\\h+([A-Za-z0-9\\._\\-]+[A-Za-z])$"

%% Extract domain from CSV (skip header lines starting with "host" or "ip")
"^(?!host)(?!ip)\"?\\'?([A-Za-z0-9][A-Za-z0-9\\-\\._]+)[^A-Za-z0-9\\-\\._]*.*$"

%% Extract IP addresses
"ip=([0-9\\.]+)$"

%% Extract FQDN with expiration date (two capture groups)
"^([A-Za-z0-9][A-Za-z0-9\\-\\._]+)[\\s|\\t]+([0-9:TZ -.]+)?$"

%% Extract domain from URL
"^[0-9]+\\,[^\\/]*\\/\\/([^\\/]+)"
```

### Whitelist Matching

ioc2rpz performs exact-match whitelist checking only. It does not perform subnet matching — if a whitelisted IP is part of a blocked subnet (or vice versa), no splitting or partial matching occurs.

---

## RPZ Configuration Options

### Cache

| Value     | Behavior |
|-----------|----------|
| `"true"`  | Zone is prebuilt and cached. Supports both AXFR and IXFR transfers. Recommended for most use cases |
| `"false"` | Live zone — sources are downloaded and the zone is built on each AXFR request. Does not support IXFR. Source data is temporarily stored in the hot cache |

### Wildcards

| Value     | Behavior |
|-----------|----------|
| `"true"`  | Wildcard rules (`*.domain`) are generated for each FQDN indicator |
| `"false"` | Only exact-match rules are created |

### RPZ Actions

| Action | Description |
|--------|-------------|
| `"nxdomain"` | Return NXDOMAIN for matched queries |
| `"nodata"` | Return NODATA (empty answer) for matched queries |
| `"passthru"` | Allow the query (allow action) |
| `"drop"` | Drop the query silently |
| `"tcp-only"` | Force the client to retry over TCP |
| `{"redirect_ip", "IP"}` | Redirect to a specific IP address. Works for both IPv4 and IPv6 |
| `{"redirect_domain", "FQDN"}` | Redirect to a specific domain (CNAME) |
| `[{Type, Value}, ...]` | Local data — return custom records |

**Local data types:** `"local_a"`, `"local_aaaa"`, `"local_cname"`, `"local_txt"`

**Examples:**
```erlang
%% NXDOMAIN action
{rpz, {"block.ioc2rpz", 7200, 3600, 2592000, 7200,
       "true", "true", "nxdomain",
       ["key1"], "fqdn", 86400, 3600, ["feed1"], [], []}}.

%% Redirect to IP
{rpz, {"redirect.ioc2rpz", 7200, 3600, 2592000, 7200,
       "false", "false", {"redirect_ip", "127.0.0.1"},
       ["key1"], "mixed", 30, 30, ["feed1"], [], []}}.

%% Redirect to domain
{rpz, {"redirect.ioc2rpz", 7200, 3600, 2592000, 7200,
       "false", "true", {"redirect_domain", "example.com"},
       ["key1"], "mixed", 30, 30, ["feed1"], [], []}}.

%% Local data with multiple records
{rpz, {"localdata.ioc2rpz", 7200, 3600, 2592000, 7200,
       "false", "true",
       [{"local_aaaa", "fe80::1"},
        {"local_a", "127.0.0.1"},
        {"local_cname", "www.example.com"},
        {"local_txt", "Blocked by policy"}],
       ["key1", "key2"], "mixed", 30, 30, ["feed1"], [],
       ["whitelist_1"]}}.
```

### SOA Timers

| Field            | Typical Value | Description |
|------------------|---------------|-------------|
| SOA_Refresh      | 7200          | How often secondaries check for updates (seconds) |
| SOA_Retry        | 3600          | Retry interval after a failed refresh (seconds) |
| SOA_Expire       | 2592000       | Zone expiration time — secondaries discard zone after this (seconds) |
| SOA_NXDomain_TTL | 7200          | Negative caching TTL (seconds) |

### AXFR / IXFR Timing

| Field     | Description |
|-----------|-------------|
| AXFR_Time | Interval in seconds between full zone rebuilds. For large zones, use longer intervals (e.g. 86400 = 1 day) to minimize load |
| IXFR_Time | Interval in seconds between incremental updates. Set to `0` to disable IXFR. Sources must support incremental updates |

### IOC Type

| Value     | Description |
|-----------|-------------|
| `"fqdn"`  | Source contains only domain names |
| `"ip"`    | Source contains only IP addresses (IPv4 and/or IPv6) |
| `"mixed"` | Source contains both domains and IPs |

### Notify List

List of IP addresses that receive DNS NOTIFY messages when the zone is updated. This triggers immediate zone transfers on secondary DNS servers.

```erlang
%% Notify two secondary servers
["192.168.1.10", "192.168.1.11"]

%% No notifications
[]
```

### Key Authorization

Keys can be specified individually or via groups:

```erlang
%% Individual keys only
["dnsproxykey_1", "dnsproxykey_2"]

%% Keys with group-based access
["dnsproxykey_1", {groups, ["public", "customers"]}]

%% Groups only
[{groups, ["public"]}]
```

---

## TLS / DoT Configuration

### Enabling DoT

DoT is automatically enabled on port 853 when a `cert` record is present in the configuration. No additional configuration is needed.

```erlang
{cert, {"cfg/ioc2rpz_dot.crt", "cfg/ioc2rpz_dot.key", ""}}.
```

### TLS Version

The default TLS version is controlled by the `TLSVersion` macro in `include/ioc2rpz.hrl`:

```erlang
-define(TLSVersion, 'tlsv1.2-1.3').  %% Supports TLS 1.2 and 1.3
```

Supported values: `'tlsv1.2'`, `'tlsv1.3'`, `'tlsv1.2-1.3'`

### Cipher Suites

Cipher suites are automatically selected based on the configured TLS version using Erlang's `ssl:cipher_suites/2`. No manual cipher configuration is needed.

### Certificate Renewal

Erlang automatically picks up renewed certificate files if they are saved to the same path. There may be a delay of up to 2 minutes due to caching. It is recommended not to let certificates expire to ensure service continuity.

Triggering a configuration reload (REST API or the `ioc2rpz-reload-cfg` DNS management command) picks up renewed certificates immediately: the server detects changed certificate files and restarts the TLS listeners with the new certificate, avoiding the SSL cache delay.

### Ports

Default ports are defined in `include/ioc2rpz.hrl`:

| Port | Protocol | Macro      | Description |
|------|----------|------------|-------------|
| 53   | UDP      | `Port`     | DNS queries (SOA only over UDP) |
| 53   | TCP      | `Port`     | DNS queries, AXFR, IXFR, management |
| 853  | TCP/TLS  | `PortTLS`  | DoT — encrypted DNS queries, transfers, management |
| 443  | TCP/TLS  | `PortDoH`  | DNS over HTTPS |
| 8443 | TCP/TLS  | `PortREST` | REST API |

---

## REST API Configuration

The REST API starts automatically on port 8443 when a `cert` record is present. It uses HTTPS with the same certificate as DoT.

### Authentication

Basic HTTP authentication using management TSIG keys:
- Username = TSIG key name
- Password = TSIG key secret

Access is restricted to IP addresses listed in the `srv` record's ACL field.

### API Endpoints

All endpoints accept both GET and POST methods.

| Endpoint | Description |
|----------|-------------|
| `/api/v1/stats/serv` | Server statistics (node name, total rules, memory) |
| `/api/v1/stats/rpz` | RPZ zone statistics |
| `/api/v1/stats/source` | IOC source statistics |
| `/api/v1/update/all_rpz` | Full refresh of all zones |
| `/api/v1/update/:rpz_name` | Full refresh of a specific zone |
| `/api/v1/mgmt/reload_cfg` | Reload configuration file |
| `/api/v1/mgmt/update_tkeys` | Update TSIG keys |
| `/api/v1/mgmt/terminate` | Shutdown the server |
| `/api/v1/feed/:rpz_name` | Get indicators in a zone. Query param: `?type=fqdn\|ip\|both` |
| `/api/v1/ioc/:ioc` | Check if an indicator is blocked. Query param: `?tkey=keyname` |
| `/api/v1/cache/sources/clear/all` | Clear all sources from hot cache |
| `/api/v1/cache/sources/clear/:source_name` | Clear a specific source from hot cache |
| `/api/v1/cache/sources/load/all` | Load all sources into hot cache |

Path parameters: `:rpz_name` = RPZ zone name, `:source_name` = IOC source name, `:ioc` = indicator (FQDN or IP).

Success responses: `{"status":"ok","msg":"..."}` (HTTP 200)
Error responses: `{"status":"error","msg":"..."}` (HTTP 520)

### Output Format

The REST API supports JSON (default) and plain text. Set the `Accept` header:
- `application/json` — JSON output (default)
- `text/plain` — plain text output

**Example:**
```bash
curl -u "dnsmkey_1:KEY_SECRET" --insecure \
  -H "Accept: text/plain" \
  https://127.0.0.1:8443/api/v1.0/mgmt/reload_cfg
```

---

## Management Commands

### DNS-Based Management

Management over DNS uses TCP or DoT. Commands are sent as DNS queries with class `CHAOS` and type `TXT`, authenticated with a management TSIG key. DNS management can be disabled by setting `MGMToDNS` to `false` in `include/ioc2rpz.hrl`.

| RR Name | Action |
|---------|--------|
| `ioc2rpz-status` | Get server status |
| `ioc2rpz-reload-cfg` | Reload configuration file |
| `ioc2rpz-update-tkeys` | Update TSIG keys |
| `ioc2rpz-update-all-rpz` | Full refresh of all zones |
| `<zone_name>` | Full refresh of a specific zone |
| `ioc2rpz-terminate` | Shutdown the server |

**Examples:**
```bash
# Get server status
dig +tcp -y dnsmkey_1:KEY_SECRET @127.0.0.1 ioc2rpz-status TXT -c CHAOS

# Reload configuration
dig +tcp -y dnsmkey_1:KEY_SECRET @127.0.0.1 ioc2rpz-reload-cfg TXT -c CHAOS

# Force full refresh of a specific zone
dig +tcp -y dnsmkey_1:KEY_SECRET @127.0.0.1 dga.ioc2rpz TXT -c CHAOS

# Force full refresh of all zones
dig +tcp -y dnsmkey_1:KEY_SECRET @127.0.0.1 ioc2rpz-update-all-rpz TXT -c CHAOS

# Shutdown the server
dig +tcp -y dnsmkey_1:KEY_SECRET @127.0.0.1 ioc2rpz-terminate TXT -c CHAOS
```

### REST API Management

See [REST API Configuration](#rest-api-configuration) for the full list of management endpoints.

```bash
# Reload configuration
curl -u "dnsmkey_1:KEY_SECRET" --insecure \
  https://127.0.0.1:8443/api/v1.0/mgmt/reload_cfg

# Full refresh of all zones
curl -u "dnsmkey_1:KEY_SECRET" --insecure \
  https://127.0.0.1:8443/api/v1.0/update/all_rpz

# Check if an indicator is blocked
curl -u "dnsmkey_1:KEY_SECRET" --insecure \
  https://127.0.0.1:8443/api/v1.0/ioc/malware.example.com
```

### Sample Zone

A built-in sample zone `sample-zone.ioc2rpz` is available for testing. It demonstrates all RPZ rule types and can be transferred via AXFR, and also answers SOA queries:

```bash
dig @127.0.0.1 sample-zone.ioc2rpz AXFR +tcp
dig @127.0.0.1 sample-zone.ioc2rpz SOA +tcp
```

---

## Compile-Time Parameters

These are defined in `include/ioc2rpz.hrl` and require recompilation to change.

| Macro | Default | Description |
|-------|---------|-------------|
| `MGMToDNS` | `true` | Enable/disable DNS-based management |
| `DBStorage` | `ets` | Database storage backend (ETS only) |
| `SaveETS` | `false` | Save ETS database to disk for faster restarts |
| `Port` | `53` | DNS port |
| `PortTLS` | `853` | DoT port |
| `PortDoH` | `443` | DNS over HTTPS port |
| `PortREST` | `8443` | REST API port |
| `TTL` | `900` | Default record TTL in seconds |
| `DefConf` | `"./cfg/ioc2rpz.conf"` | Default configuration file path |
| `DefDB` | `"./db"` | Default database directory |
| `TLSVersion` | `'tlsv1.2-1.3'` | TLS version for DoT and REST API |
| `DNSPktMax` | `16383` | Maximum DNS packet size (bytes) |
| `Compression` | `6` | Compression level (0–9) for cached zone data |
| `ZoneRefTime` | `60000` | Zone refresh check interval (milliseconds) |
| `TCPTimeout` | `3000` | TCP session timeout (milliseconds) |
| `HotCacheTime` | `900` | Hot cache TTL for IOCs/records/packets (seconds) |
| `HotCacheTimeIXFR` | `0` | Hot cache TTL for IXFR IOCs (seconds). Minimum effective value is 60s |
| `Src_Retry` | `3` | Number of retries for unavailable sources |
| `Src_Retry_TimeOut` | `3` | Timeout between source retries (seconds) |
| `ShellMaxRespSize` | `2 GB` | Maximum response size for shell sources |
| `SourcePullTimeout` | `300000` | Source download timeout (milliseconds) |
| `RATE_LIMIT_WINDOW` | `10000` | Rate limit window (milliseconds) |
| `MAX_REQUESTS_PER_WINDOW` | `1` | Max requests per window for the granular bucket: provisioned zone + supported QTYPE (`SOA`/`AXFR`/`IXFR`) and recognized management requests, keyed `{IP, QName, QType}` |
| `MAX_UNKNOWN_REQUESTS_PER_WINDOW` | `1` | Max requests per window for the aggregate per-IP bucket: unknown/unprovisioned zone, unsupported QTYPE, wrong class, or unrecognized management name, keyed `{IP}` |

---

## Environment Variables

Defined in `config/sys.config.src` and `config/vm.args`:

| Variable | Description |
|----------|-------------|
| `IPv4` | IPv4 bind address |
| `IPv6` | IPv6 bind address |
| `CONF` | Configuration file path |
| `DB` | Database directory path |
| `CD` | Working directory |
| `NODE_NAME` | Erlang node name |
| `IO2Cookie` | Erlang distributed cookie |
