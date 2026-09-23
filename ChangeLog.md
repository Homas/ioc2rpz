# ioc2rpz change log
[CB] - Changed Behaviour
## 2026-09-23 v1.4.0.6
### Correctness
- A client dropping a zone transfer can no longer empty that zone for every other client. When `ioc2rpz:send_packets/20` failed to send a packet it purged the hot cache AND called `ioc2rpz_db:delete_db_pkt/1`, regardless of which operation had failed. `delete_db_pkt/1` removes the packets of the zone's CURRENT serial — the generation all other AXFR clients are being served from — while the `ixfr` and `send` operations write no cache of their own at all: they only STREAM data built by the zone updater. So one client hanging up mid-IXFR destroyed the whole zone's packet cache. `read_db_pkt/1` then returned `[]`, `send_cached_zone/8` sent no records for an empty cache, and the transfer was still logged as CEF 201 `RPZ transfer success` with `out=0`, so every subsequent client received an empty RPZ and the log showed nothing wrong. It only recovered at the next FULL zone update — an incremental update that finds no new indicators does not rebuild the cache — which is why a feed could stay broken for hours or days. The new `discard_partial_cache/2` drops only what the failed operation itself had written: the half-built AXFR generation for `cache`/`sendNcache`, the partial hot-cache packets for `sendNhotcache`, and nothing for `ixfr`/`send`
- A chunked cache build reports a partial rule/indicator count as an error rather than as the zone's size. The clause that splits a zone into concurrent chunks hard-matched `{ok,NRules,NIOCs}` from each half and added the counters; now that a half can answer `{error,Reason}` (see Observability), the new `sum_chunk_results/2` sums only when BOTH halves succeeded and otherwise propagates the error, so a short count is never written to `#rpz.rule_count` / `#rpz.ioc_count`. The spawned worker's result is still collected when the remaining chunks fail, so a monitored worker cannot be left unreaped
### Observability
- [CB] A log line is now written by a SINGLE `io:fwrite/3` call. The timestamp used to be a separate write issued just before the one carrying the message, and every `io:fwrite/3` is an independent request to the io server, so with several processes logging concurrently the two interleaved and a timestamp could be printed in front of ANOTHER process' line. The same interleaving also split existing multi-call messages (`Found Key ... Good timestamp ... Valid MAC`) across unrelated lines
- [CB] Log lines carry the pid of the logging process (`?logPID`, on by default), right after the timestamp. Every DNS request, zone transfer, source pull and zone update runs in its own process and they all log to one device, so concurrent operations could not be told apart — for example which of two simultaneous transfers to the same client reported a communication error. Set `?logPID` to off to restore the previous format
- `ioc2rpz_sup:update_all_zones_inc(false)` logged through a raw `io:fwrite/3` and was the only log line with no timestamp/pid prefix. It goes through `ioc2rpz_fun:logMessage/2` now
- The cache discard that follows a failed zone transfer is no longer logged as a bare `Communication error. Removing partily cached zone and stopping operations`. The line now names the zone and its serial, the client, the transport, the failure reason, the operation (`ixfr`/`send`/`sendNhotcache`/`cache`), the packet number reached, and the bytes sent, and a second line states exactly what is discarded (hot-cache packet count, or the partially built AXFR packet cache of which serial — or, for `ixfr`/`send`, that nothing is discarded). With several transfers of different zones running concurrently the old line could not be attributed to any of them
- [CB] A zone transfer that fails while streaming packets now logs a terminating CEF event. `ioc2rpz:send_packets/20` ended the failure path with `erlang:exit(self(), normal)`, which killed the worker before its caller could log anything: such a transfer produced NEITHER CEF 201 (success) nor CEF 131 (remote closed) — it simply had no terminating event, and the failure was invisible to anything consuming the CEF stream, including the `out=` byte counter. The error is propagated as `{error,Reason}` instead, so the normal CEF 131 / 130 line is written with the reason, the duration and the bytes actually sent. As a consequence `ioc2rpz:send_zone/4` and `send_zone_live/9` can now return `{error,Reason}` where a failed transfer previously returned `ok` (or never returned at all); `ioc2rpz_sup:update_zone_full_1/1` logs the failure and releases the zone claim, and `rebuild_axfr_zone/1` aborts the update so the next scheduled run retries instead of recording a truncated zone as current
- `ioc2rpz_db:delete_db_pkt/1` logs the zone, serial and number of deleted packets. It drops the packets of the zone's CURRENT serial — the generation AXFR clients are being served from — so a zone that suddenly transfers empty can now be traced back to this call. `delete_old_db_pkt/1` logs its count at debug level, together with how many packets of the current serial it kept
- An empty AXFR packet cache is reported by `ioc2rpz_db:read_db_pkt/1`. `send_cached_zone/8` sends nothing at all for an empty cache and the transfer is still logged as CEF 201 `RPZ transfer success` with `out=0`, so a zone that had lost its cache looked healthy in the log
- `read_db_pkt/1` reports when the packet cache holds packets from more than one builder. The existing safeguard silently discards the losing builder's packets, which yields a zone that transfers with holes; it is now logged with both counts
- A tripwire logs when a chunk of a concurrent cache build reaches the next chunk's packet-number base. The build hands chunk N the base `N*100` while each chunk holds `?IOCperProc` (10000) indicators, so a chunk needing 100 packets or more overwrites the next chunk's packets in `rpz_axfr_table` under a colliding key. Whether a chunk stays under 100 packets depends on the on-wire size of the indicators, i.e. on the feed content, so a feed can cross the limit on any update
- IXFR transfers log the size of the delta (expired/new indicator counts) and the client serial before building it, and the resulting rule/indicator counts now name the client. Without the delta size the log could not show that an "incremental" transfer was carrying the whole zone to every client on every poll
- A request whose authority/additional section cannot be parsed is logged with the source, the question and the raw section before the error propagates. `parse_rr/3` is called unguarded from `process_dns_request/4` (unlike `parse_question/1` and `edns_udp_size/3`, which both wrap it), so a truncated record killed the worker with a `badmatch` whose crash report identified neither the client nor the query
## 2026-08-30 v1.4.0.5
### Availability
- A zone can no longer be wedged permanently by a failed zone-build worker. The parent processes that collect results from spawned chunk workers (`ioc2rpz:w_send_packets/3` for packet generation, `ioc2rpz_conn:w_clean_feed/2` for feed cleanup) waited in a bare `receive` with no monitor and no timeout. If a worker died — a bad user regex, an ETS or socket failure — the parent blocked FOREVER while holding that zone's update claim (`status=updating` with a live owning pid), so `claim_zone_for_update/1` never reclaimed it and the zone answered SERVFAIL to every AXFR/IXFR until the node was restarted. Workers are now spawned with `monitor`, so a worker death is detected immediately, and the wait is bounded by the new `?WorkerRespTimeout` (30 minutes) for a worker that is alive but stuck. Both failure paths abort the zone update and release the claim, so the next scheduled run retries
- Zone updaters now release their claim on ANY failure. `update_zone_full/1` and `update_zone_inc/1` wrap their body in a `try`, log the failure, and reset the zone to `notready` with no owning pid. Previously a crashed updater left `status=updating` behind and was only recoverable because the recorded pid happened to be dead
- [CB] Startup no longer blocks on feed downloads. `update_all_zones(false)` was called synchronously inside `ioc2rpz_sup:init/1`, before the listener child specs were returned, so ports 53/853/443/8443 stayed unbound until the last source had been fetched — up to `?SourcePullTimeout` (5 min) per source, with no health signal in the meantime. Zones are now loaded in a spawned process, concurrently with the listeners coming up. A zone whose first update has not completed yet answers exactly as it did before it was ready
- The REST and DoH listeners are now part of the ioc2rpz supervision tree. They were started by calling `cowboy:start_tls/3` for its side effect inside `ioc2rpz_proc_sup:init/1` while the returned child list was `[]`, so this supervisor had nothing to supervise and ioc2rpz never learned about a listener that gave up. They are now started from a `ranch:child_spec/5` child spec (with the same ALPN preferences and `connection_type` Cowboy would have used, so the running listener is unchanged)
- A missing `srv` configuration row no longer crashes whatever happens to need it. Eight call sites destructured it with a hard `[[...]] = ets:match(cfg_table,{srv,...})`, which raised `badmatch` in the supervisor during startup, in every DNS/DoT connection worker, and in REST requests (on the unauthenticated path, before the ACL check). All of them now read it through the new `ioc2rpz_fun:srv_cfg/0` / `srv_cert/0` and degrade gracefully: startup continues without the TLS/DoT/DoH/REST listeners, DNS requests are answered SERVFAIL, REST requests are denied, and a zone update is aborted with its claim released
### Security
- [CB] Malformed DNS requests can no longer crash the request process, and they now consume rate-limit budget. `extract_label/3` failed a binary match on a truncated label, an over-long label, or a question that ended before QTYPE/QCLASS — and it ran BEFORE `check_rate_limit/3`. A crafted datagram therefore produced unlimited unauthenticated process crashes, each with an error report, without ever being counted against any limit (log/crash amplification). Label length (=< 63) and total name length (=< 255) are now validated per RFC 1035, the parse is wrapped by the new `parse_question/1`, and a malformed request is counted in the aggregate per-IP bucket and answered FORMERR. Once that bucket is exhausted the datagram is dropped with no response and no log line, so a flood cannot be turned into unbounded logging either
- [CB] `?TLSVersion` now constrains the TLS protocol versions, not only the cipher suites. It was passed to `get_cipher_suites/1` alone; `ssl:listen/2` (DoT) and the Cowboy listeners (DoH, REST) never received `{versions, [...]}`, so which versions were actually accepted depended on the ssl application defaults and on there being no cipher suite in common for the versions that were not wanted. The new `ioc2rpz_fun:get_tls_versions/1` expands the same setting into `{versions, [...]}` and it is passed alongside `{ciphers, [...]}` everywhere. With the default `'tlsv1.2-1.3'` the negotiated result is unchanged
- [CB] `shell:` source validation: `tee` was removed from the safe-utility allowlist and added to the blocklist. It is an arbitrary-file-write primitive, so `... | tee /root/.ssh/authorized_keys` passed validation while plain `>` redirection was correctly rejected. The blocklist (matched by basename, so it applies to absolute paths too) additionally covers destructive/filesystem-mutating commands (cp, ln, install, truncate, shred, rsync, mkfifo, mknod, chgrp), process/host control (pkill, halt, poweroff, systemctl, service), more shells (dash, ash, busybox, command), arbitrary-exec wrappers (xargs, find, env, nohup, setsid, sudo, su, doas, ssh, scp, sftp, at, batch, crontab) and network listeners (nc, ncat, netcat, socat, telnet). Interpreters (php, python, perl, ruby, node, ...) are deliberately NOT blocked: invoking a local decoder script by absolute path is a supported way to write a `shell:` source. A source whose command is rejected logs CEF 151 and returns no indicators, so check the log after upgrading if a feed relies on any of the above
- Debug logging is OFF by default (`?debug` in `include/ioc2rpz.hrl` is commented out). It routed every `?logDebugMSG` to stdout with no level and no rotation
- [CB] Log timestamps are ON by default (`?logTS`). Every line, including CEF lines, is now prefixed with `YYYY-MM-DD HH:MM:SS.mmm `. A CEF line carries no event time of its own, so SIEM/CEF consumers had nothing to key on. Log parsers that assume a line starts with `CEF:0|` need updating. Resolution is milliseconds: concurrent zone transfers, source pulls and zone updates routinely log several lines within the same second, and a second-resolution stamp cannot order the events of an incident
- The security implications of `?MGMToDNS` (DNS-based management, still enabled by default) are documented at the macro: every such request is gated on the management ACL, TCP transport, and TSIG validation when a signature is present
### Correctness
- Zone updaters no longer revert concurrent configuration changes. They wrote back a whole `#rpz{}` record built from the snapshot captured when the update was spawned, via `ets:update_element(..., [{3, Zone#rpz{...}}])` — position 3 holds the entire record, so this was a full overwrite from a minutes-old base, not a field update. It reverted any `reload_cfg` change made in the meantime (sources, keys, timers, `forceAXFR`) and clobbered the record that `claim_zone_for_update/1` had just written. Updates now go through the new `update_zone_rec/2`, which applies only the updater-owned fields (status, pid, serials, counts, timestamps) to the CURRENT record under a bounded compare-and-swap
- A configuration reload can no longer be aborted by `exit(undefined, Reason)`. Terminating the updater of a removed zone called `exit/2` on `#rpz.pid` unguarded, and `status=updating` with `pid=undefined` is a state the reload path creates itself (it flips every zone to `updating` before computing the diff). The `badarg` aborted `read_config3/8` halfway through — after all zones had been marked `updating` but before the new records were inserted. `exit/2` is now guarded on the pid actually being a pid
- A zone being updated while its configuration changes is now actually terminated. The `RPZ_UPD` list holds records freshly parsed from the configuration file, where `status` is `ready`/`notready`/`forceAXFR` and `pid` is `undefined`, so the `X#rpz.status == updating` filter was NEVER true: neither the "Zone was updated. Terminating" log line nor the `exit/2` ever fired, and the still-running updater went on to overwrite the `forceAXFR` status with its own stale snapshot. Each entry is now resolved back to its pre-reload counterpart, where a running updater's pid is recorded
- `my_process_is_alive(undefined)` now reports `false`. Both call sites read "alive" as "not claimable", so reporting `true` made any zone left in `status=updating` with no owning process permanently un-updatable while in that state — and the reload path produces exactly that combination
### Build / packaging
- EUnit tests are no longer compiled into release beams. `eunit.hrl` DEFINES `TEST` itself, so an unguarded `-include_lib` of it makes any `-ifdef(TEST)` in the same module always true: the `-ifdef(TEST)` block in `ioc2rpz_db` was never actually excluded. All six modules now guard the include, and the test blocks in `ioc2rpz`, `ioc2rpz_fun`, `ioc2rpz_sup` and `ioc2rpz_rest` are inside `-ifdef(TEST)`. Release beams export no `_test`/`_test_` functions; several of those tests create and delete `cfg_table` and `rpz_ixfr_table`, which the running server owns
- The application version is derived from `?ioc2rpz_ver` in `include/ioc2rpz.hrl`, the same source the relx release version uses. `ioc2rpz.app.src` carried a hand-maintained `{vsn,"1.4.0.1"}` that had drifted several builds behind, so the application and release versions disagreed
- CI: a GitHub Actions workflow now runs compile, eunit, xref and dialyzer on push and pull request (OTP 24 and 27), builds the release and the Docker image, and asserts that no test code leaks into release beams. `.github/` previously held only `FUNDING.yml`
- `{dialyzer,{plt_extra_apps,...}}` added so calls into cowboy's transitive dependencies (ranch, cowlib) are analysed instead of reported as unknown functions
- Docker: `rebar.lock` is copied alongside `rebar.config`, so cowlib and ranch resolve to the locked, hash-verified versions instead of whatever satisfied the constraints at build time; base image pinned to `erlang:27-alpine`; deprecated `MAINTAINER` replaced with OCI `LABEL`s; the image runs as a non-root `ioc2rpz` user; a `HEALTHCHECK` queries the DNS listener over TCP on loopback; `EXPOSE` now includes 443 (`?PortDoH`); `php`, `lftp` and `ripgrep` are no longer installed (no code path uses them)
- [CB] Docker: `ENV IO2Cookie=ioc2rpz` was removed. It baked a well-known Erlang distribution cookie into every image, and anyone able to reach the distribution port of such a node can evaluate arbitrary code in the VM. The entrypoint now generates a random cookie when `IO2Cookie` is not supplied. Pass your own (`-e IO2Cookie=...`) if several nodes must talk to each other
## 2026-08-25 v1.4.0.4
- DNS rate limits are now configurable per server and per RPZ zone, as an optional trailing `{rate_limit,[...]}` element of the `{srv,{...}}` and `{rpz,{...}}` tuples. Supported options: `window` (in SECONDS), `max_requests` (granular `{IP, QName, QType}` bucket) and, at the server level only, `max_unknown_requests` (aggregate per-IP bucket — a request counted there did not resolve to a zone, so there is no zone configuration to read it from)
- Every level and every individual option is optional. Each option resolves on its own with the precedence **RPZ zone → server → macro default**, so a zone can override just `max_requests` and still inherit `window` from the server, and a server can configure nothing at all. Configurations with no `rate_limit` anywhere keep the exact previous behaviour — the `?RATE_LIMIT_WINDOW` / `?MAX_REQUESTS_PER_WINDOW` / `?MAX_UNKNOWN_REQUESTS_PER_WINDOW` macros are now the last fallback rather than the only setting
- Invalid values and unrecognised options are logged and ignored (the option is inherited from the next level down instead) rather than rejecting the configuration, consistent with the other optional settings. A maximum of `0` is accepted and refuses every request in that bucket; `window` must be greater than 0
- The optional `{rate_limit,...}` and `TrackSources` trailing elements may be given together in either order, or either one on its own — neither has to be spelled out to configure the other. `{srv,{...}}` now accepts 4 to 6 elements and `{rpz,{...}}` 15 to 17; all previously valid forms parse unchanged
- Changing a limit takes effect on a configuration reload and does not force a zone transfer
- [CB] Rate-limit entries changed shape from `{Key, WindowStart, Count}` to `{Key, WindowStart, Count, WindowMs}`. Because windows are now configurable per zone, the periodic cleanup expires each entry against its OWN window instead of a single global cutoff — a key with a longer window is no longer swept early, which would have reset its counter and let its limit be exceeded
- `ioc2rpz_fun:check_rate_limit/1` was replaced by `check_rate_limit/3` (key, maximum, window); the threshold is no longer derived from the key shape inside the counter but resolved by the new `ioc2rpz:rl_limits/2` from the zone, the server and the macro defaults
## 2026-08-24 v1.4.0.3
- DNS rate limiting is now race-free. Every request is served by its own process (UDP packets are spawned, TCP/TLS connections have a worker each), and the counter used a read-modify-write (`ets:lookup` + `ets:insert`) sequence, so concurrent requests for the same key read the same value and overwrote each other's increment. Under a flood — precisely when the limiter matters — the effective limit was overshot several-fold (measured: 16 requests admitted against a limit of 6, with the stored count still at 6). Counting is now a single atomic `ets:update_counter/4`, and the window rollover a compare-and-swap, so no increment can be lost
- [CB] Rate-limit entries changed shape from `{Key, {WindowStart, Count}}` to `{Key, WindowStart, Count}` (required to count atomically). The window start is now fixed for the duration of a window instead of being pushed forward by every admitted request: the limit is `?MAX_REQUESTS_PER_WINDOW` per `?RATE_LIMIT_WINDOW` as documented. Previously the window only reset after a full idle gap, so a client polling just under the window interval accumulated hits across many minutes and was eventually refused despite staying well below the configured rate
- [CB] Sources listed in the management ACL (`#srv.acl`) are exempt from DNS rate limiting. Management stations and monitoring systems poll far more often than a secondary does, and refusing them removed exactly the visibility needed while a server was under pressure. The ACL is only consulted after the counter has already tripped, so ordinary traffic pays nothing for the check. Exemptions are recorded in the debug log; CEF 429 is now emitted only for requests that are actually refused
- [CB] Query and zone names are matched case-insensitively (RFC 4343). The zone lookup compared the wire-format query name byte for byte, so a query for `EXAMPLE.RPZ` missed a zone configured as `example.rpz`: it was answered NOTAUTH and counted in the aggregate per-IP rate-limit bucket instead of the zone's own bucket. Query names are now lower-cased before the zone lookup, the rate-limit key, and the management-command/sample-zone dispatch, and zone names from the configuration file are canonicalised to lower case. Responses still echo the question section verbatim in the case the client sent, and logs still show the requested name as received. A zone written in upper/mixed case in the configuration changes its cache key and is rebuilt once on upgrade
- The `rate_limits` ETS table is created by `ioc2rpz_db:init_db/3` alongside the other named tables, with the database supervisor as its heir, instead of being created by (and dying with) `ioc2rpz_sup:init/1`. Creation is idempotent, so an inherited table no longer aborts startup with `badarg`, and a missing table no longer crashes the request being served or the cleanup timer
## 2026-08-20 v1.4.0.2
- [CB] RPZ zone transfer logs now report the transfer size: CEF 201 (transfer success), 131 (remote closed connection) and 130 (transfer error) carry a new trailing `out=<bytes>` field — the standard CEF field for bytes sent to the destination. It is the total on-wire size of every DNS response packet pushed to the client for that AXFR/IXFR, including the 2-byte TCP length prefixes, so abusive clients can be spotted by volume transferred and not only by request count. On a failed transfer `out` reports what was successfully sent before the failure. SIEM parsers pinned to the old field list should be updated
## 2026-07-07 v1.4.0.1
- [CB] Shell source hardening now closes command-chaining bypasses: `shell:` command validation treats `;`, `&` (covers `&&` and background `&`), and newline/carriage-return as command separators in addition to `|`, so EVERY chained command's executable is validated (a destructive command after a separator is caught as a segment leader instead of slipping through mid-string). Unquoted input redirection (`<`) is now rejected alongside output redirection (`>`). Separators inside single/double quotes remain literal (legitimate quoted URLs/awk/sed expressions are unaffected)
- EDNS0 (RFC 6891) UDP responses: `send_dns_udp` now honours the requestor's advertised UDP payload size from the request OPT record when deciding UDP truncation, instead of always capping at 512 bytes. The advertised size is clamped to [512, 4096]; requests without an OPT record keep the classic 512-byte behaviour. Oversized responses are still truncated with the TC bit set so the client retries over TCP (the server does not yet echo an OPT record in responses)
- Configuration file safety: the world-writable check now also warns when the directory containing the configuration file is world-writable WITHOUT the sticky bit — such a directory lets any local user rename/replace the config regardless of the file's own permissions. A world-writable directory with the sticky bit set (e.g. /tmp) is not flagged. Advisory, non-fatal
## 2026-07-04 v1.4.0.0
- IOC source attribution (false-positive source tracing): the `/api/v1/ioc/:ioc` lookup can now report which source(s) inside a feed contributed an indicator, via a new additive `sources` field on each feed object. The field is additive and backward compatible — all existing fields (`feed`, `wildcard`, `type`, `rpz_serial`, `ioc_expiration`) are unchanged, so old clients that ignore it are unaffected
- New optional per-feed `track_sources` setting (`auto | true | false`) as the trailing 16th element of the `{rpz,{...}}` tuple, and a new optional server-level global default (`off | auto | on`, default `off`) as the trailing 5th element of the `{srv,{...}}` tuple. Effective state resolves by precedence: per-feed value → server global default → built-in `off`. The 15-field `rpz` and 4-field `srv` tuples remain valid and leave tracking unset/off
- Off by default: upgrading the binary changes nothing (no tracking, no zone rebuilds, unchanged API) until a value is set. Feeds are GUI-managed; the GUI/community site will write the optional config values and surface `sources` in a later update. Enabling tracking for a cached feed triggers a one-time AXFR rebuild to populate source masks
- `auto` tracks only multi-source feeds; single-source feeds are never masked and return their one source name directly. Tracking disabled or attribution unknown returns `sources: null` (JSON) / `(disabled)`/`(unavailable)` (TXT)
- Toggling `track_sources` (per feed or via the server global default) now forces a one-time AXFR rebuild of the affected feeds on config reload so their per-source masks are (re)derived. Previously enabling tracking left masks at 0 until an unrelated AXFR ran, so multi-source feeds reported `sources: (unavailable)` after a reload despite tracking being on
- Attribution is available only for cached feeds. Masks are authoritative after a full AXFR; on IXFR, masks for newly-added indicators are set, but a mask change for an already-present indicator may only reconcile on the next AXFR. Reordering or editing a zone's source list forces a one-time AXFR rebuild (source-signature check)
- Feeds with more than 63 sources use a wider binary bitmap mask (default) so attribution stays correct
- Configuration reload now applies changes to a source's extraction `regex`, `ioc_type`, and `max_ioc` even when its AXFR/IXFR URLs are unchanged. Previously the reload diff only compared the URLs, so editing just the regex (or ioc_type/max_ioc) of an existing source was silently ignored: the source kept its old settings in cfg_table and its hot cache was not invalidated. Such sources are now correctly marked as updated, re-inserted, purged from the hot cache, and their RPZ zones re-transferred
- Logging is now crash-safe: a format string / argument-count mismatch in a log call no longer takes down the calling process. Previously a bad `msg_CEF`/`logMessage` call raised `badarg` from `io:fwrite`, which for a DNS/AXFR worker crashed the gen_server and aborted the transfer (with a SUPERVISOR/CRASH REPORT). `logMessage/3` and `logMessageCEF/3` now route through a guarded writer that catches formatting errors and emits a fallback line (class, reason, format, args) so the defect is still visible without disrupting DNS service or zone transfers
## 2026-06-26 v1.3.0.8
- [CB] CEF event codes for REST API events were renumbered out of the RPZ-transfer range to remove duplicate/shadowed IDs: REST Basic auth failed 130→140, REST auth failed 131→141, REST MGMT denied 135→145, MGMT request failed 136→146, unsupported request 137→147, zone not found 138→148. Codes 130/131 are now exclusively RPZ transfer events. Update any SIEM correlation rules that referenced the old REST codes
- RPZ statistics (`/api/v1/stats/rpz`) now include a `status` field per zone (`ready`, `updating`, `forceAXFR`, `notready`) so consumers can tell current stats from those reflecting the last completed update
- RPZ indicator/rule counts, serials, and update timestamps are now preserved across a configuration reload (including non-cached/online zones) instead of relying on the cache roundtrip and resetting to zero while a zone is re-validated
- Fixed get_cipher_suites/1: recognized single TLS version atoms (`'tlsv1.2'`, `'tlsv1.3'`, `'tlsv1.1'`, `'dtlsv1.2'`) are now handled correctly (previously only `'tlsv1.2-1.3'` worked and the other documented values crashed), and an unknown/misconfigured version now logs a warning and falls back to TLS 1.2 instead of raising a function_clause error at listener startup
## 2026-06-23 v1.3.0.7
- [CB] Shell source hardening: `shell:` commands are now validated before execution. Each pipeline segment's executable must be an absolute path or an allowlisted text utility (sort, uniq, grep, sed, awk, gawk, etc.); destructive commands and shells (rm, bash, sh, dd, chmod, ...) are blocked, and command substitution (`$(...)`, backticks) and output redirection (`>`, `>>`) are rejected. Rejected commands are not run and are logged via CEF 151 (executed commands via CEF 150)
- [CB] File source path traversal: `file:` source paths containing `..` parent-directory segments are now rejected
- [CB] HTTPS source downloads now verify the remote server's TLS certificate against the system CA store, including hostname verification. Sources served with an invalid, expired, self-signed, or hostname-mismatched certificate will fail to download. For self-signed endpoints use http:// or a shell: source with `curl --insecure`
- DoH POST bodies are now capped at 4096 bytes; oversized requests receive HTTP 413 Payload Too Large instead of being read unbounded into memory
- Configuration file safety: ioc2rpz now logs a warning on startup, reload, and for included files if the configuration file is world-writable (advisory, non-fatal; fix with `chmod o-w`)
## 2026-06-22 v1.3.0.6
- Fixed a regression from v1.3.0.5: the TCP/TLS accept workers were set to `permanent`, which emitted a child_terminated SUPERVISOR REPORT and triggered a supervisor restart on every completed connection — these workers are one-shot (one connection then `{stop, normal}`) and already self-replace via start_socket/1 on accept, so `permanent` spammed the logs and slowly grew the accept-worker pool. Workers are now `transient`: a normal exit is silent and not restarted (pool is maintained by start_socket), while a genuine abnormal crash is still restarted. Top-level child supervisors remain `permanent`.
## 2026-06-22 v1.3.0.5
- Listener pool resilience: top-level UDP child supervisor and the TCP/TLS accept workers are now `permanent` (previously `transient`/`temporary`), and the TCP/TLS worker pools use intensity `{1000, 60}` so bursts of accept/handshake failures no longer deplete the pool or crash the supervisor
- Accept calls now use a 30s timeout (gen_tcp:accept/2, ssl:transport_accept/2); on timeout the worker re-enters the accept loop instead of blocking indefinitely
- TLS listen socket now sets reuseaddr, send_timeout (5s), and send_timeout_close; TCP listen socket now sets reuseaddr and no longer sets {active, once} on the listen socket (set on accepted sockets only)
- Fixed peername badmatch crashes: handle_info for TCP/TLS now handles ssl:peername/inet:peername errors gracefully (logs "peer disconnected", closes socket, stops cleanly) instead of crashing the worker when a peer disconnects in the race window
- send_dns_tls/3 now checks ssl:send/2 and ssl:setopts/2 return values (returns {error, Reason} on failure) like its TCP twin; send_dns_udp/5 now checks and logs gen_udp:send/4 failures
- DoH POST with an empty body now returns HTTP 400 Bad Request instead of an unbound-variable error/500
- Config reload validator (validateCFGRPZ/3) now names the specific missing source/whitelist and affected RPZ zone when an RPZ references a removed source
## 2026-06-22 v1.3.0.4
- Intelligent (hybrid) DNS rate limiting: provisioned zones + supported QTYPEs (SOA/AXFR/IXFR) and recognized management requests are tracked per {IP, QName, QType}; everything else (unknown zone, unsupported qtype, unrecognized name) is aggregated per {IP} to prevent query-name-variation bypass. Adds separate threshold MAX_UNKNOWN_REQUESTS_PER_WINDOW
- Hot cache packet entries are now periodically purged (ioc2rpz_db:cleanup_hotcache/0) to prevent unbounded rpz_hotcache_table growth
- UDP responses over 512 bytes now set the TC (truncation) bit and are truncated per RFC 1035 4.2.1, prompting clients to retry over TCP
- Fixed zone-update race condition: an atomic compare-and-swap (claim_zone_for_update/1) prevents duplicate concurrent updates of the same zone across all spawn paths
- REST API JSON responses now escape user-controlled values (ioc2rpz_fun:json_escape/1), preventing JSON injection/breakage from special characters in source/RPZ names
- Sample zone (sample-zone.ioc2rpz) now answers SOA queries (previously NOTAUTH); fixed the SOA record (zone field set, expire timer 259200)
- TLS certificate reload: a configuration reload (ioc2rpz-reload-cfg) detects changed certificate files and restarts the DoT/REST TLS listeners, applying renewed certificates without a full restart
- README: rewrote "Building from Source" with prerequisites, a minimal config example, and a development-shell section

## 2026-06-21 v1.3.0.3
- Guard zone build against a removed source (badmatch fix)
- Rate-limit table cleanup to prevent memory leak
- Constant-time comparison for REST key and TSIG MAC

## 2026-06-21 v1.3.0.2
- Fix AXFR cache wipe causing badmatch crash on zone transfer (same-serial race condition)
- Harden read_db_pkt to return [] on empty match instead of crashing
- TLS connection handling fix
- Cowboy version bump
- Comprehensive EDoc comments added to all source modules
- Created docs/architecture.md, docs/protocols.md, docs/configuration.md, docs/deployment.md
- Overhauled README.md with full project documentation
- Sample IOC and config files added

## 2025-01-22 v1.3.0.1
- FIX BUG #52 (https://github.com/Homas/ioc2rpz/issues/52)

## 2025-01-10 v1.3.0.0
- Query rate limiting

## 2021-07-31 v1.1.3.0
- new REST API calls
- source types
- keep source in the hot cache

## 2020-12-31 v1.1.2.3
- Fixed issue #35

## 2020-09-07 v1.1.2.2
- Configuration hot cache time per source

## 2020-07-08 v1.1.2.1
- [CB] By default only TLSv1.2 and TLSv1.3 are supported. If you need to downgrade to TLSv1.1 (not recommended) or support only TLSv1.3 update TLSVersion parameter in include/ioc2rpz.hrl

## 2020-02-20 v1.1.1.4
- [CB] A default timeout (SourcePullTimeout - 5 minutes) was added to limit time for fetching feeds/sources via http/https/ftp.

## 2019-12-11 v1.1.1.3
- [CB] IoC lookup REST API call. The submitted indicator converted to lowcase before the lookups.

## 2019-12-11 v1.1.1.2
- [CB] IoC lookup REST API call output was modified

## 2019-12-11 v1.1.1.1
- [CB] Regex expressions were updated to match any type of newline string chars "{newline, any}"

## 2019-12-10 v1.1.1.0
- IoC lookup REST API call

## 2019-12-05 v1.1.0.2
- Bug #20. Whitelists didn't work.

## 2019-11-25 v1.1.0.1
- Bug with updating zones (broken packets after AXFR and wildcard rule after IXFR). It is recommended to upgrade to the newest release.

## 2019-09-28 v1.1.0.0
- DoH (DNS over HTTPs) for SOA requests.

## 2019-09-20 v1.0.0.4
- [CB] Filtering out indicators with illegal chars (ioc2rpz:clean_labels). Performance should be validated.

## 2019-09-20 v1.0.0.3
- Bug. Incremental update. Indicators w/o expiration date were not added to a zone.

## 2019-09-15 v1.0.0.3
- Fixed bug #17 "Full zone update brakes a zone and next incremental updates do not add new indicators".

## 2019-07-21 v1.0.0.2
-IPv4/IPv6 networks detection in IOCs for mixed zones
-IPv6 localhost network detection in IOCs

## 2019-07-21 v1.0.0.1
- RPZ statistics collected: # rules and # indicators

## 2019-07-21 v1.0.0.0
- Released v1.0.0.0

## 2019-07-20 v0.9.5.0
- Bug fixes related to IXFR zone update and transfer
- [CB] Source IXFR update "from" time will be keept the same until we get "non zero" update.
- Retry for unavailable sources (see ioc2rpz.hrl)
- IXFR table management optimization

## 2019-06-13 v0.9.4.0
- Fixed bugs:
	- #10 "redirect_domain add zone name"
	- live zones, wrong records count in the hot cache
- Rule generation code was optimized
- Configuration can be split by multiple files using "include"
- Key groups for RPZs and SRV

## 2019-03-11 v0.9.3.1
- REST API
- added rebar3 to manage dependencies

## 2019-03-01 v0.9.2.1
- bug with configuration reload

## 2019-02-24 v0.9.2.0
- DoT (DNS over TLS) support for zone transfer, SOA and management requests (DNS Notify is not supported).

## 2019-02-22 v0.9.1.1
- UDP service moved under supervisor

## 2019-02-15 v0.9.1
- [CB] Connection and key validation log messages were formated in CEF
- Request to reload TSIG keys list only.

## 2018-09-22
- IPv6 support
- Configuration file name and IPs are moved to an app config file

## 2018-08-16
- concurent zone creation in a cache

## 2018-08-14
- [CB] tcp_send errors handeling
- Query class and type in text in the logs

## 2018-07-25
#- [CB] Individual indicators are converted into low case instead of converting a full source file. It was updated because of possible issues with REGEX.
#It is require more memory. If memory is limited uncomment marked lines in ioc2rpz_conn.erl.

## 2018-04-11
Added a reference to ioc2rpz.gui
No code change

## 2018-01-07 v0.9.0-2018010701
Initial release
