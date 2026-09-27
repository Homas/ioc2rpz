## Bugs
- [ ] **`ioc2rpz:send_dns/3` return value is not uniform across transports.** Latent, not currently reachable — record it before the guards that hide it are relaxed.

  **What it returns.** Three different contracts depending on `#proto.proto`:

  | transport | success | failure |
  |---|---|---|
  | `tcp` (plain and TLS/DoT) | `ok` | `{error, Reason}` |
  | `udp` | `ok` | `{error, Reason}` |
  | `doh` | **`{ok, Pkt}`** | — never fails |

  The `doh` clause (`src/ioc2rpz.erl`, `send_dns(_Socket,Pkt,[Proto,_Args]) when Proto#proto.proto == doh`) does not send anything: it hands the packet back so `parse_dns_request/3` can return it to `ioc2rpz_doh:parse_dns/3`, which matches `{ok, Data}` and lets Cowboy serialise the HTTP response. For a single-message response that is the correct design, not a bug.

  **Why it is a problem.** Every caller that inspects the result compares against the bare atom `ok`, so `{ok, Pkt}` reads as "not a success":
  - `send_packets/20`, `PSize > ?DNSPktMax` clause: `if (SendStatus == ok) -> <next packet>; true -> <failure path> end`. `{ok,Pkt}` takes the failure path at every packet boundary, so the transfer would abort at the first 16 KB with `{error,{ok,Pkt}}` and (before v1.4.0.5) would have purged the zone's cache on the way out.
  - `send_cached_zone/8`: `case send_dns(...) of ok -> <recurse>; {error,Reason} -> ... end` — `{ok,Pkt}` matches neither clause, so a `case_clause` crash, even for a **single-packet** cached zone.
  - `process_dns_request/4`, the `send_zone/4` result: `case ... of ok -> CEF 201; {error,closed} -> CEF 131; {error,Reason} -> CEF 131 end` — same `case_clause`.
  - `add_sent_bytes/1` is not called on the `doh` path, so `out=` would always be reported as 0.

  **Why it is not reachable today.** Every zone-transfer dispatch clause in `process_dns_request/4` is gated on `Proto#proto.proto == tcp`: the RPZ clause (`QType == ?T_SOA orelse (((?T_AXFR andalso NSCOUNT == 0) orelse (?T_IXFR andalso NSCOUNT == 1)) andalso Proto#proto.proto == tcp)`) and the sample-zone clause (`MGMTIP andalso Proto#proto.proto == tcp andalso (?T_AXFR orelse ?T_IXFR)`). A DoH request can therefore only reach `send_SOA/10`, `send_txt_response/4`, `send_status/3` and `send_REQST/7` — all single-packet, all returning `send_dns/3`'s value straight to the DoH handler, which is exactly what `{ok,Pkt}` is for. AXFR/IXFR over DoH is silently answered as if the query type were not supported for that transport.

  **Fix direction — pick one:**
  1. *Make the intent explicit.* Answer NOTIMP (or REFUSED) for `?T_AXFR`/`?T_IXFR` when `Proto#proto.proto == doh`, in its own clause before the TCP-gated ones, and state in the docs that zone transfers are TCP/DoT only. RFC 8484 is one DNS message per HTTP request, so a multi-packet AXFR cannot be expressed anyway. Cheapest and it removes the trap.
  2. *Make the contract uniform.* Have the `doh` clause return `ok` and accumulate the packet (process dictionary, like the `?SentBytesKey` counter, or an explicit accumulator), with the DoH handler reading the accumulated response at the end. Only meaningful for a single-message response, so it does not actually enable AXFR over DoH — it just removes the special-case return value. Also call `add_sent_bytes/1` there so `out=` is populated.

  Either way add a regression test: DoH GET **and** POST of `AXFR`/`IXFR` for a cached multi-packet zone must produce a defined DNS response, not a `case_clause` crash and not an aborted transfer, and must leave `rpz_axfr_table` for that zone untouched.

- [ ] **Restore `ioc_md5` on startup and across config reloads.** Every restart, and every config reload, makes the next full update of EVERY cached zone rebuild it with a new serial, even when not a single indicator changed.

  **What happens.** `ioc2rpz:send_zone_live/9` decides whether a full update produced anything new by comparing the MD5 of the zone's indicator set (`IOC3`, masks excluded) with `Zone#rpz.ioc_md5`: `{cache, MD5} -> {updateSOA, ...}` keeps the serial and logs `Zone ... is the same`. That comparison only works if `ioc_md5` holds the previous build's hash, and two paths drop it:
  - *Startup.* `save_axfr_zone_info/2` (`src/ioc2rpz_db.erl`) persists `ioc_md5` in the `{axfr_rpz_cfg, Zone}` row, and `load_zone_info/1` → `load_axfr_zone_info/1` (`src/ioc2rpz_sup.erl`) returns it. But the `rpz` clause of `read_config3/8` matches it as `_Ioc_md5` (`src/ioc2rpz_sup.erl:517-518`) and the `#rpz{}` it builds (`:543`, and the 15-field clause that follows) never sets `ioc_md5`, so it is `undefined`.
  - *Reload.* `merge_rpz_stats/2` (`src/ioc2rpz_sup.erl:893`) carries `ioc_count`, `rule_count`, `serial`, `serial_ixfr`, `update_time`, `ixfr_update_time` and `ixfr_nz_update_time` from the pre-reload record onto the freshly parsed one, but not `ioc_md5`. A GUI "publish" triggers a reload, so this path runs whenever the configuration is pushed.

  **Impact.** For each cached zone, the first full update after a restart or reload rebuilds the whole AXFR packet cache, stamps a new serial, sends NOTIFY, and makes every secondary pull a full transfer of an identical zone. With large feeds (oisd, blocklistproject), that is a lot of pointless CPU and bandwidth at exactly the moment the node is busiest. It also hides feed staleness. Observed Sep 23-27, 2026: `adultfree`, `blocklist-malicious`, `blox-malicious` and `local` all showed serial `1790198400` (the first 21:20 run after the v1.4.0.6 deploy). All nine `blocklistproject_*` downloads returned the same size and MD5 for days, and Infoblox `combined.csv` had not changed since Aug 13. Earlier deploys, a few days apart, kept resetting those serials, so the feeds never looked old until the node ran 4 days without a restart.

  **Fix.**
  1. Bind the loaded hash in both `load_zone_info` match clauses of `read_config3/8` and set `ioc_md5` in the `#rpz{}` constructors. Only for `Status0 == ready`: the `notready` clause and the `_` fallback must leave it `undefined`, because a zone with no usable cache has to be rebuilt.
  2. Add `ioc_md5 = Old#rpz.ioc_md5` to `merge_rpz_stats/2`.
  3. **Clear it wherever a rebuild is forced.** The MD5 covers only the indicator set, so a restored hash would turn a forced AXFR into `updateSOA` whenever the indicators happen to be the same. The zone would then never be rebuilt with its new settings. Set `ioc_md5 = undefined` together with `status = forceAXFR` for:
     - the `RPZ_UPD` entries in `read_config3([],reload,...)` (action, wildcards, sources, whitelist or keys changed; the `ets:update_element(... #rpz{status=forceAXFR})` line)
     - `maybe_force_source_axfr/4` (the source list or order changed; masks must be re-derived)
     - `track_state_changed/4` (tracking toggled; masks are excluded from the MD5 by design)
     - the `ioc2rpz-update` / forced-update management commands, if any of them go through the full-update path with a snapshot that still carries the hash

     Alternatively, make `send_zone_live/9` skip the `updateSOA` short-circuit when `Zone#rpz.status == forceAXFR`. That would be one change, and it would also cover any forced path missed above.
  4. Confirm `ioc_md5` survives `saveZones()` → restart for zones whose last update was `updateSOA` (that branch already writes it via `update_zone_rec/2`).

  **Tests.** Save a zone with a known `ioc_md5`, run `read_config3/8` startup and check that the record carries it. Also check that `merge_rpz_stats/2` copies it. Then check that a reload which changes `action` for a zone with an unchanged indicator set really rebuilds it (a new serial, NOT `updateSOA`).

  **Changes behaviour** (mark `[CB]` in the ChangeLog). Serials only move when indicators change, so after upgrading, a restart no longer makes every feed look freshly updated. Any serial-age "stale feed" alert on the community site will then show real upstream staleness. Before or together with this change, switch that alert to the last-check time (`update_time`) or to the source MD5.

- [ ] **Shutdown does not shut anything down.** `ioc2rpz_sup:stop_ioc2rpz_sup/0` (`src/ioc2rpz_sup.erl:52-57`) only logs and calls `ioc2rpz_db:saveZones()`; the `gen_server:stop(?MODULE)` line is commented out. Both callers report success while the node keeps serving: the DNS `ioc2rpz-terminate` command (`src/ioc2rpz.erl:627-628`) answers "ioc2rpz is terminating." and the REST `/api/v1/mgmt/terminate` endpoint (`src/ioc2rpz_rest.erl:274`) does the same. Note `gen_server:stop/1` would have been the wrong API anyway — `ioc2rpz_sup` is a supervisor. The fix is to save the DB, then stop the application (`application:stop(ioc2rpz)`) or the node (`init:stop/0`), and only answer once shutdown is actually under way. `ioc2rpz_proc_sup:stop_ioc2rpz_proc_sup/0` (`:35-38`) has the same wrong-API problem and has no callers at all — remove it or implement it properly. *(review item 8)*
- [ ] Zone update can be triggered twice (Serial is the same) which leads to duplicate packets in the rpz_axfr_table. A workaround was implemented in read_db_pkt function to ensure that only one set of packets is passed but it impacts performance. 
- [ ] Check zone refresh time when the SOA record is requested (different vs axfr)
- [ ] Take a look on the bugs mentioned in REST section
- [ ] If a TSIG is not auth. different responses on SOA and AXFR
- [ ] If a source was removed, RPZ will fail - add validation/clean up

## Core / DNS
- [ ] If connection was closed by remote server - log that instead of "success"
- [x] DNS requests rate limiting
- [ ] Ratelimit params to config
- [ ] Sample zone is broken
- [ ] If IXFR source not set or equal AXFR - get removed records for IXFR
- [ ] Force RPZ, Source refresh
- [ ] RPZ from RPZs
- [ ] simple permissions model
- [ ] REST API rate limiting
- [ ] HotCache optimization if refresh time less than hotcache storage time
- [ ] Zone update - flush hot cache
- [x] Enforce domain validation. Discard indicators with wrong chars
  - [ ] (ioc2rpz:clean_labels). Performance should be validated.
- [ ] , A and AAAA requests. Optional A/AAAA support is added to be able to access the server via unique hostnames. In that case ioc2rpz behaves as an authoritative server
- [ ] RPZ storage type: ets, mnesia
- [ ] Mnesia for storage (and auto creation)
https://github.com/ChicagoBoss/ChicagoBoss/wiki/Automatic-schema-initialization-for-mnesia
- [ ] Redo AXFR logs
- [ ] Access to the hotcache and the cfg_table via FUNs
- [ ] (1) Terminate updating zones during config reload
- [ ] (1) Clean up the code & add comments
- [ ] Logs level startup config
- [ ] Check delete in ioc2rpz: rpz_hotcache_table/pkthotcache

- [ ] Distributed configuration
- [ ] Wait while a remote server confirms receiving a notification
- [ ] (2) EDNS0 Support: DNS Cookie, edns-tcp-keepalive, NSID
- [ ] (3) Memory optimization for huge zones (erl -pa ebin +MEas bf ?????)
- [ ] DoD https://tools.ietf.org/html/draft-ietf-dprive-dnsodtls-06

- [ ] EUnit Tests for main funs.
- [ ] Handle RPZ update if one of a sources is not availble or a recent update returned significatnly low number of indicators

## Sources
- [ ] Simultanious source downloads
- [ ] Add source PostreSQL, MySQL via "shell:"
- [ ] RPZ action per source
- [ ] (2) Source based on files check by mod.date and size -> read by chunks

- [ ] ioc type in config
- [ ] max file size
- [ ] RPZ action
- [ ] NS type
- [ ] lowcase optimization option
  - [ ] (1) IOC to lowercase - check memory usage impact (in ioc2rpz_conn)
- [ ] spawn processes
- [ ] Hot cache optimization depending on RPZ refresh time and source usage in multiple feeds
- [ ] Cache optimization for huge zones
- [ ] Statistics table


## RPZ
- [ ] warm cache in mnesia
- [ ] Monitor significant drop in # of IoCs and if detected - postpone an update to 1 - 3 IXF cycles or specified time
- [ ] RPZ by source intersection
- [ ] Max # of IOCs
- [ ] Catalog zones
- [ ] Statistics per zone (# records, last update, # AXFR, # IXFR, last axfr update time, avg axfr update time, last ixfr update time, avg ixfr update time)
- [ ] RPZ behavior: ignore unreachable sources, use old data for unreachable sources, do not update the zone
- [ ] Additional local records: ptr, srv, mx etc
- [ ] RPZ transfer rate limiting

- [ ] (2) FDateTime,ToDateTime,FDateTimeZ,ToDateTimeZ + support them for AXFR  
[:FDateTime:] = "2017-10-13 13:13:13", [:FDateTimeZ:] = "2017-10-13T13:13:13Z"  
[:ToDateTime:] = "2017-10-13 13:13:13", [:ToDateTimeZ:] = "2017-10-13T13:13:13Z"

## Servers
- [ ] Enforcement max # of IOCs
- [ ] Secondary DNS via MNESIA and distributed

## REST
- [ ] MGMT via REST API
  - [ ] Statistics per source, RPZ, performance
  - [ ] Bug RPZ stats after reload config
- [ ] Bug in cowboy. Can not send 501 in "catch all"

## Configuration

## Management
- [ ] DNS health check requests
- [/] Disable MGMT via DNS (update ioc2rpz.gui first) - default behaviour

## Build / packaging
- [ ] **Run the container as non-root properly.** v1.4.0.5 added `USER ioc2rpz`, and v1.4.0.7 reverted it (the `USER` line is commented out in the `Dockerfile`, the user is still created). Re-enable it only together with ALL of the following. Otherwise it breaks REST/DoT again, silently.

  **What broke (Sep 2026, io2core-de1).** The container ran as `uid=100(ioc2rpz) gid=101(ioc2rpz)`. The mounted `/opt/ioc2rpz/ssl/ioc2rpz.net.cert` and `.key` were `root:root` with mode `0400`. Erlang `ssl` reads certfile/keyfile per connection, not in `ssl:listen/2` or at ranch listener start. So the REST (8443) and DoT (853) listeners started normally, and every handshake then failed before the ServerHello. The server closed TCP without sending an alert (`curl: (35) ... SSL_ERROR_SYSCALL`). `ranch:handshake/2` drops the error without logging it, and ioc2rpz logged nothing. Plain DNS on 53 was unaffected, so the container passed its HEALTHCHECK.

  **To do:**
  1. *Pin the UID/GID* in the `Dockerfile`: `addgroup -S -g <GID> ioc2rpz && adduser -S -u <UID> -G ioc2rpz ...`. Today `adduser -S` takes the next free system ID of the base image (100/101 on `erlang:27-alpine` now), so it can change with any base-image update, and a host-side `chown` would silently stop matching. Pick an ID unlikely to exist on hosts (e.g. 10053) so it does not collide with a real host account that would then be able to read the key. Consider build args (`ARG IOC2RPZ_UID`) so operators can match an existing host group.
  2. *Check TLS files at startup.* Before starting the REST, DoH and DoT listeners (`ioc2rpz_proc_sup:cowboy_tls_childspecs/4`, `tls_listen_opts/0`), `file:read_file/1` the certfile, keyfile and (if set) cacertfile. If one is unreadable, log an error naming the file, the reason (`eacces`, `enoent`, ...) and the running UID/GID, and emit a CEF event. Better still, parse the files (`public_key:pem_decode/1`, non-empty), so a readable but corrupt or empty file is caught too. Then either do not start that listener (consistent with the existing "no certificate configured" path) or start it and keep logging. Decide which, and document it.
  3. *Also check the other volumes at startup.* The config file (`cfg`) must be readable, and `db/` must be writable: `saveZones()` writes there, and a failure would only surface at the next restart as lost zone state. Log clearly if either check fails.
  4. *Surface handshake failures.* Something like a ranch/cowboy `stream_handlers` hook or an `ssl` `log_level` setting, so a listener that resets every connection leaves a trace, rate-limited.
  5. *Document* in README (Docker section) and ChangeLog (`[CB]`): the UID/GID; how to prepare host files (`chown <UID>:<GID>` or `chgrp <GID>` + `chmod 0440` on the key; `db/` writable); that renewal/copy scripts (certbot deploy hooks and the like) must re-apply ownership, because they recreate files as `root:root`; and that `--user 0` is the escape hatch.
  6. *CI:* extend the Docker job in `.github/workflows/ci.yml` to start the image with a root-owned 0400 key mounted and assert that the startup check reports it. Then start it with a correctly owned key and assert that `curl -k https://127.0.0.1:8443/` completes a handshake.

- [ ] **Dead code reported by xref (`locals_not_used`).** These local functions are unreachable in a release build. Some were only ever reached from EUnit tests, which no longer compile into release beams, so they are now genuinely dead: `ioc2rpz:bin_to_hexstr/1`, `ioc2rpz:hexstr_to_bin/1,2`, `ioc2rpz:domstr_to_bin/1`, `ioc2rpz:gen_txt_rec/1`, `ioc2rpz:remove_WL/2`, `ioc2rpz_db:get_allzones_info/1`, `ioc2rpz_fun:z_split/2,3`, `ioc2rpz_rest:rest_terminate/2`, `ioc2rpz_sup:update_all_zones_inc/1`. Delete them (or wire up the ones that were meant to be used — `update_all_zones_inc/1` looks like an intended feature), then re-enable `locals_not_used` in `{xref_checks,...}` so CI catches the next one. See `rebar3 xref --extra_checks="[locals_not_used]"`.
- [ ] **`{dev_mode, true}` in the relx section of `rebar.config` (`:15`).** A plain `rebar3 release` therefore produces a symlink farm pointing back into `_build/default/lib`, which is not relocatable — the release directory cannot be copied or archived and used elsewhere. The Dockerfile happens to hide this by overriding it (`rebar3 release -d false`), so the broken default is easy to miss. Flip the default to `false` and move `dev_mode` into a `dev` profile for anyone who wants the fast local iteration loop. *(review item 15)*
- [x] ~~`sys.config.src` `${IPv4}`/`${IPv6}`/`${CONF}` substituted at BUILD time, so runtime `ENV` has no effect~~ — **investigated, not an issue.** relx ships `sys.config.src` into the release (`releases/<vsn>/sys.config.src`, with no generated `sys.config` beside it) and the extended start script templates it at BOOT. Runtime `ENV` does take effect for all of `IPv4`/`IPv6`/`CONF`/`DB`/`CD`/`NODE_NAME`, and there is no inconsistency between them. Verified against a built release and a running container (the log line `Env ip4: ... conf: ... db: ... cwd: ...` reflects the container's environment). Recorded here so it is not re-raised. *(review item 19)*

## Unsorted
- [ ] Switch from IXFR cache to Sources cache. IXFR cache allows you to support less zone updates but IOCs can be stored multiple times. Sources cache will contain duplicate IOCs from the same source but RPZs will be updated more frequently (looks like it is not bad).
  - [ ] (3) Share IOC between the feeds in IXFR table (do not forget about different whitelists)

## Other/optimization TODO
- [ ] (1) Do not cache expired IOCs if ExpDateTime<Serial_IXFR / update ExpDateTime if exists
- [ ] (1) Check zones IXFR update from multiple sources




## Source failure handling & graceful degradation (was dot-reliability-fix task 24)

> Moved out of the `dot-reliability-fix` spec because the original "task 24" was
> framed as a one-line bug fix (`{ok,<<>>}` → `{error,{http_status,Code}}`) but the
> real problem is a design/behavior decision that needs an explicit degradation
> policy. Captured here so it isn't lost. **Not scheduled.**

### Background — what the code does today

- `ioc2rpz_conn:get_ioc/2` already **retries** failed downloads: `?Src_Retry` = 3
  attempts, `?Src_Retry_TimeOut` = 3s apart, `?SourcePullTimeout` = 5 min per attempt.
  So a source returning nothing has already been retried, not dropped on first hiccup.
- **Retry asymmetry:** retries only cover the `{error, Reason}` branch (connection
  refused / timeout / remote close). A *valid* non-200 HTTP response (403/500/503 page)
  hits the `{ok,{{_,Code,_},...}}` clause, which returns `{ok,<<>>}` immediately with
  **no retry**. TCP-level failure → 3 tries; HTTP 503 → 1 try. Inconsistent.
- Non-200 currently returns `{ok,<<>>}`, which `get_ioc/3` treats as a *successful*
  download of zero indicators (logs "got 0 indicators").
- In `ioc2rpz:mrpz_from_ioc/4`, when an **expired** cached source fails to refetch:
  ```
  ets:delete(rpz_hotcache_table,{SRC,UType}),   % old data deleted
  IOC1 = get_ioc(...),                          % [] on failure
  ets:insert(rpz_hotcache_table, {{SRC,UType}, CTime, term_to_binary([])})  % caches []
  ```
  i.e. it does **not** keep stale data forever — it drops the source's contribution and
  caches the empty result.

### The real issues

1. **Stale-forever vs. drop is a genuine tradeoff, not a bug.**
   - Retain old data forever → a permanently dead feed keeps injecting stale/wrong
     indicators into the RPZ indefinitely.
   - Drop on failure (current) → coverage silently disappears on a transient outage that
     outlasted the 3 retries.
   Neither is universally correct; the operator needs to choose per source.

2. **No last-known-good store.** The hot cache is the *only* store of parsed per-source
   IOCs. Once an entry expires and the refetch fails, it's deleted and unrecoverable
   until the feed returns. There is no persistent last-good copy to fall back on.

3. **No source attribution in the merged RPZ.** Sources are flattened with `IOC1 ++ IOC`
   then deduped/whitelisted. The served zone is a union with no record of which indicator
   came from which source, so graceful per-source degradation ("keep A's last-good set,
   drop B's") can't be reconstructed from the zone once built. Per-source data only exists
   while each `{SRC,UType}` cache entry is alive.

### Proposed direction (decide policy before implementing)

- [ ] Distinguish failure types in `get_ioc/2`: non-200 → `{error,{http_status,Code}}`;
      log distinct reasons for `{failed_connect,_}`, `socket_closed_remotely`, `timeout`.
- [ ] Fix the retry asymmetry: retry non-200 statuses (at least 5xx) like connection errors.
- [ ] Have `get_ioc/3` treat a failed download as failure (not empty success) — return a
      sentinel distinct from a genuinely empty feed so the zone-update logic can decide.
- [ ] On failure of an **expired** source, stop the `delete + cache []` behavior. Instead,
      apply a per-source degradation policy:
        - `ignore_unreachable` — keep serving last-good cached data past TTL,
        - up to a configurable **grace / max-staleness window**,
        - with a **hard max-staleness bound** so a permanently dead feed eventually ages
          out (addresses issue 1) instead of lingering forever,
        - or `drop` (current behavior) as an explicit opt-in.
      (See existing TODO: "RPZ behavior: ignore unreachable sources, use old data for
      unreachable sources, do not update the zone".)
- [ ] Add a persistent last-known-good copy per source so data survives cache expiry +
      fetch failure (issue 2).
- [ ] Surface per-source health: last successful fetch time, last error, stale flag —
      via logs and REST stats — so a degraded source is visible instead of silently 0.
- [ ] Tie in with existing TODO items: "Monitor significant drop in # of IoCs and ...
      postpone an update", and "Handle RPZ update if one of a sources is not available or
      a recent update returned significantly low number of indicators".

### Original spec subtasks (for reference)

- 24.1 `get_ioc/2` non-200 → `{error,{http_status,Code}}` instead of `{ok,<<>>}`.
- 24.2 `get_ioc/2` error clause: distinguish `{failed_connect,_}` / `socket_closed_remotely`
       / `timeout` / other in the log.
- 24.3 `get_ioc/3` handle `{error,{http_status,Code}}`: log status, return `[]`.
- 24.4 Docs (`deployment.md` Common Log Messages): non-200 now returns the typed error;
       add specific connection-error entries.
- 24.5 Testing: source returning 403/500 → typed error in logs (not "success" 0 bytes);
       source whose server closes the connection → "connection closed by remote server".
