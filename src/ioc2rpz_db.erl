%Copyright 2017-2021 Vadim Pavlov ioc2rpz[at]gmail[.]com
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

%% @doc IOC2RPZ DB Cache
%%
%% This module manages all database operations for the ioc2rpz DNS RPZ server,
%% providing an abstraction layer over ETS (and optionally Mnesia) storage.
%%
%% The module manages the following ETS tables:
%% <ul>
%%   <li>`rpz_axfr_table' - Stores cached AXFR zone transfer packets and per-zone
%%       AXFR configuration metadata. Uses `ordered_set' type with keys of the form
%%       `{rpz, Zone, Serial, PktN, ParentPID}' for packets and
%%       `{axfr_rpz_cfg, Zone}' for zone config.</li>
%%   <li>`rpz_ixfr_table' - Stores individual IOC (Indicator of Compromise) records
%%       for incremental zone transfers and per-zone IXFR configuration metadata.
%%       Uses `duplicate_bag' type with keys of the form
%%       `{ioc, Zone, IOC, IoCType}' for indicators and
%%       `{ixfr_rpz_cfg, Zone}' for zone config.</li>
%%   <li>`cfg_table' - Stores server configuration: RPZ zones, TSIG keys, sources,
%%       whitelists, and server settings. Uses `ordered_set' type.</li>
%%   <li>`rpz_hotcache_table' - Stores recently accessed zone packets for fast
%%       retrieval, keyed by `{pkthotcache, Zone, PktN}'. Uses `ordered_set' type.</li>
%%   <li>`stat_table' - Stores server statistics counters. Uses `ordered_set' type.</li>
%% </ul>
%%
%% All tables are created as `public' named tables with `read_concurrency' and
%% `write_concurrency' enabled, and use the `{heir, PID, []}' option to survive
%% owner process crashes by transferring ownership to the database supervisor.
%% @end
-module(ioc2rpz_db).
-include_lib("ioc2rpz.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([init_db/3,db_table_info/2,read_db_pkt/1,write_db_pkt/2,delete_db_pkt/1,delete_old_db_pkt/1,read_db_record/3,write_db_record/3,delete_old_db_record/1,saveZones/0,loadZones/0,loadZones/1,
        get_zone_info/2,clean_DB/1,save_zone_info/1,get_allzones_info/2, lookup_db_record/2,cleanup_hotcache/0,source_signature/1]).


%% @doc Initializes the database storage backend.
%%
%% For ETS: attempts to load persisted AXFR/IXFR tables from `DBDir'. If loading
%% fails, creates new `rpz_axfr_table' and `rpz_ixfr_table' tables; otherwise
%% transfers ownership of the restored tables to `PID'. Always creates fresh
%% `cfg_table', `rpz_hotcache_table', and `stat_table' tables.
%%
%% For Mnesia: creates or connects to a Mnesia schema on the local node, then
%% creates the ETS-only tables (`cfg_table', `rpz_hotcache_table', `stat_table').
%%
%% @param Storage Database backend atom (`ets' or `mnesia')
%% @param DBDir Directory path for persisted ETS table files
%% @param PID Heir process PID that receives table ownership on owner crash
%% @returns `{ok, []}'
%% @end
init_db(ets,DBDir,PID) ->
  [{STA,_},{STI,_}]=loadZones(DBDir),
  if STA /= ok ->
    ets:new(rpz_axfr_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]); %because labels are shortened
    true -> ets:give_away(rpz_axfr_table, PID, [])
  end,
  if STI /= ok ->
    ets:new(rpz_ixfr_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, duplicate_bag, public, named_table]); %set
    true -> ets:give_away(rpz_ixfr_table, PID, [])
  end,
  ets:new(cfg_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]),
  ets:new(rpz_hotcache_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]), %because labels are shortened
  ets:new(stat_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]),
  init_rate_limit_table(PID),
  {ok,[]};

init_db(mnesia,_DBDir,PID) ->
%init schema
%create tables
  case mnesia:create_schema([node()]) of % local node only. TODO Update to multinode
    ok -> %Create new DB
      mnesia:start(),
      mnesia:create_table(rpz_axfr_table, [{type, set}]),
%    ets:new(rpz_axfr_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]); %because labels are shortened
%    ets:new(rpz_ixfr_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, duplicate_bag, public, named_table]); %set
      ok;
    _Else -> %DB was already created, starting mnesia
      mnesia:start()
  end,
  ets:new(cfg_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]),
  ets:new(rpz_hotcache_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]),
  ets:new(stat_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]),
  init_rate_limit_table(PID),
  {ok,[]}.


%% @doc Creates the rate-limit table (`?RATE_LIMIT_TABLE') with the database
%% supervisor as its heir, so the table survives the death of the process that
%% created it instead of taking every in-flight DNS request down with it.
%%
%% Creation is idempotent: if the table is still alive (inherited by the heir
%% after an owner crash, or created by a test) it is kept as is, because
%% `ets:new/2' on an existing named table raises `badarg' and would abort
%% startup. `set' is required — {@link ioc2rpz_fun:check_rate_limit/2} counts
%% with `ets:update_counter/4'.
%%
%% @param PID Heir process PID that receives table ownership on owner crash
%% @returns `ok'
%% @end
-spec init_rate_limit_table(pid()) -> ok.
init_rate_limit_table(PID) ->
  case ets:info(?RATE_LIMIT_TABLE, name) of
    undefined ->
      ets:new(?RATE_LIMIT_TABLE, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, set, public, named_table]),
      ok;
    _ ->
      ok %already exists (inherited by the heir) - keep the counters
  end.


%% @doc Returns information about a database table.
%%
%% Delegates to `ets:info/2' or `mnesia:table_info/2' depending on the
%% configured `?DBStorage' backend.
%%
%% @param Table The table name atom (e.g., `rpz_axfr_table', `cfg_table')
%% @param Param The info parameter to query (e.g., `size', `memory')
%% @returns The requested table information value
%% @end
db_table_info(Table,Param) ->
  db_table_info(?DBStorage,Table,Param).
db_table_info(ets,Table,Param) ->
  ets:info(Table,Param);
db_table_info(mnesia,Table,Param) ->
  mnesia:table_info(Table,Param).

%% @doc Reads cached AXFR zone transfer packets for a given zone.
%%
%% Retrieves all packets from `rpz_axfr_table' matching the zone's binary name
%% and current serial number. Packets are stored as compressed binaries via
%% `term_to_binary/2' and are decompressed on read with `binary_to_term/1'.
%%
%% As a safeguard against a race condition where multiple processes may write
%% packets for the same zone concurrently, only packets written by the same
%% parent process (the first PID found in the result set) are returned.
%%
%% @param Zone An `#rpz{}' record with at least `zone' and `serial' fields set
%% @returns A list of `{PktN, ANCOUNT, NSCOUNT, ARCOUNT, Records}' tuples,
%%          or `ok' for mnesia backend (not yet implemented)
%% @end
read_db_pkt(Zone) -> %axfr
  read_db_pkt(?DBStorage,Zone).
read_db_pkt(ets,Zone) ->
%  Pkt = ets:match(rpz_axfr_table,{{rpz,Zone#rpz.zone,Zone#rpz.serial,'_','_'},'$2'}),
%  [binary_to_term(X) || [X] <- Pkt];

% 2025-01-11 There is a bug that multiple processes can save the zone at the same time. The following validation is done only as a saveguard. It may be removed when the bug is fixed
% An empty result must not crash the caller: it can happen if cfg_table advertises
% a serial for which no packets are cached (e.g. a partially completed/cleaned up
% update). Return [] in that case so send_zone can fall back gracefully.
  Pkt = ets:match(rpz_axfr_table,{{rpz,Zone#rpz.zone,Zone#rpz.serial,'_','$1'},'$2'}),
  case Pkt of
    [] -> [];
    [[PID, _] | _] ->
      [binary_to_term(X) || [PPID, X] <- Pkt, PPID == PID]
  end;

read_db_pkt(mnesia,_Zone) ->
  ok.

%% @doc Removes expired packet entries from the `rpz_hotcache_table'.
%%
%% Deletes cached zone-transfer packet entries (keyed `{pkthotcache, Zone, PktN}')
%% whose stored timestamp is older than `?HotCacheTime' seconds. Without this
%% periodic sweep these packet entries are only checked for staleness on read and
%% otherwise accumulate indefinitely, growing `rpz_hotcache_table' unbounded.
%%
%% Source IOC hot-cache entries (keyed `{SourceName, axfr|ixfr}') are deliberately
%% NOT touched here — they honour each source's own `hotcache_time' and are
%% refreshed by `ioc2rpz_sup:load_hotsources/1'.
%%
%% Intended to be called via `timer:apply_interval/4' from the supervisor.
%% @returns `ok'.
-spec cleanup_hotcache() -> ok.
cleanup_hotcache() ->
  Cutoff = ioc2rpz_fun:curr_serial() - ?HotCacheTime,
  %% Delete packet hot-cache entries {{pkthotcache,_,_}, Timestamp, _} where Timestamp < Cutoff
  Deleted = ets:select_delete(rpz_hotcache_table,
    [{{{pkthotcache,'_','_'}, '$1', '_'}, [{'<', '$1', Cutoff}], [true]}]),
  ?logDebugMSG("Hot cache cleanup removed ~p expired packet entries~n", [Deleted]),
  ok.

%% @doc Writes a single AXFR zone transfer packet to the cache.
%%
%% Inserts a packet into `rpz_axfr_table' with a composite key containing the
%% zone binary name, serial number, packet sequence number, and the calling
%% process's parent PID (for concurrent-write safeguarding). The packet tuple
%% is compressed using `term_to_binary/2' with the `?Compression' level.
%%
%% @param Zone An `#rpz{}' record with `zone' and `serial' fields set
%% @param Pkt A tuple `{PktN, ANCOUNT, NSCOUNT, ARCOUNT, Records}'
%% @returns `true' on successful ETS insert, or `ok' for mnesia (not implemented)
%% @end
write_db_pkt(Zone, Pkt) ->
  write_db_pkt(?DBStorage, Zone, Pkt).
write_db_pkt(ets, Zone, {PktN,_ANCOUNT,_NSCOUNT,_ARCOUNT,_Records} = Pkt) ->
  ets:insert(rpz_axfr_table, {{rpz,Zone#rpz.zone,Zone#rpz.serial,PktN,erlang:process_info(self(), parent)}, term_to_binary(Pkt,[{compressed,?Compression}])});
write_db_pkt(mnesia, _Zone, _Pkt) ->
  ok.

%% @doc Deletes cached AXFR zone transfer packets for a given zone.
%%
%% When `Zone#rpz.serial' is 42 (magic value for full cleanup), deletes ALL
%% packets and the AXFR zone config entry for the zone from `rpz_axfr_table'.
%% Otherwise, selectively deletes packets whose sequence number is less than
%% or equal to the zone's current serial (i.e., stale packets from old serials).
%%
%% @param Zone An `#rpz{}' record with `zone' and `serial' fields set.
%%        Use `serial=42' to remove all data for the zone.
%% @returns `true' on successful ETS delete, or `ok' for mnesia (not implemented)
%% @end
delete_db_pkt(Zone) -> %axfr
  delete_db_pkt(?DBStorage,Zone).

delete_db_pkt(ets,Zone) when Zone#rpz.serial == 42 ->
  %?logDebugMSG("Removing AXFR zone ~p ~n",[Zone#rpz.zone_str]),
  ets:match_delete(rpz_axfr_table,{{rpz,Zone#rpz.zone,'_','_','_'},'_'}),
  ets:match_delete(rpz_axfr_table,{{axfr_rpz_cfg,Zone#rpz.zone},'_','_','_','_','_','_','_','_'});

delete_db_pkt(ets,Zone) ->
  %axfr_rpz_cfg
  %?logDebugMSG("Removing AXFR zone ~p serial ~p ~n",[Zone#rpz.zone_str, Zone#rpz.serial]),
  ets:select_delete(rpz_axfr_table,[{{{rpz,Zone#rpz.zone,Zone#rpz.serial,'$1','_'},'_'},[{'=<','$1',Zone#rpz.serial}],[true]}]);

delete_db_pkt(mnesia,_Zone) ->
  ok.

%% @doc Deletes only the stale cached AXFR packets for a zone.
%%
%% Removes every cached packet for the zone whose serial is strictly older than
%% `Zone#rpz.serial' (the just-written generation). Unlike {@link delete_db_pkt/1},
%% this never removes packets for the current serial, so it is safe to call right
%% after writing a new generation even when the previous serial collides with the
%% new one (serials have 60-second resolution, see {@link ioc2rpz_fun:curr_serial_60/0}).
%%
%% This avoids a race where a cleanup deletes the packets that were just cached,
%% leaving `cfg_table' advertising a ready zone with an empty packet cache and
%% crashing AXFR/IXFR transfers in {@link read_db_pkt/1}.
%%
%% @param Zone An `#rpz{}' record whose `serial' is the new (current) generation.
%% @returns The number of deleted objects, or `ok' for mnesia (not implemented).
%% @end
delete_old_db_pkt(Zone) -> %axfr
  delete_old_db_pkt(?DBStorage,Zone).

delete_old_db_pkt(ets,Zone) ->
  ets:select_delete(rpz_axfr_table,[{{{rpz,Zone#rpz.zone,'$1','_','_'},'_'},[{'<','$1',Zone#rpz.serial}],[true]}]);

delete_old_db_pkt(mnesia,_Zone) ->
  ok.

%% @doc Reads IOC (Indicator of Compromise) records from the IXFR cache.
%%
%% Selects records from `rpz_ixfr_table' based on the zone, a reference serial
%% number, and a filter type:
%% <ul>
%%   <li>`all' - All records added or expired between `Serial' and the zone's
%%       current serial</li>
%%   <li>`updated' - Records that were updated within the serial range</li>
%%   <li>`new' - Records added after `Serial' that have not yet expired</li>
%%   <li>`expired' - Records that expired between `Serial' and the current serial</li>
%%   <li>`active' - All currently active (non-expired) records since `serial_ixfr'</li>
%% </ul>
%%
%% @param Zone An `#rpz{}' record with `zone', `serial', and `serial_ixfr' fields
%% @param Serial The reference serial number for the IXFR delta query
%% @param Type One of `all', `updated', `new', `expired', or `active'
%% @returns A list of matching `{{ioc, Zone, IOC, IoCType}, AddSerial, ExpSerial}'
%%          tuples, or `ok' for mnesia (not implemented)
%% @end
read_db_record(Zone,Serial,Type) -> %ixfr
  read_db_record(?DBStorage,Zone,Serial,Type).
read_db_record(ets,Zone,Serial,all) ->
  %% Task 15.1 / R6 upgrade tolerance: ALSO match legacy pre-upgrade rows whose
  %% value has only 2 elements {AddSerial,ExpSerial} (no 4th mask element). Such
  %% rows are read as mask 0 (unknown) — the mask is not projected here ('$$'
  %% yields [IOC,AddSerial,ExpSerial,IoCType]) — and stay readable until the
  %% source-signature-forced AXFR (task 3) rewrites the zone in the new
  %% 3-value-element shape. A stored object matches exactly one value arity, so
  %% unioning the new (mask) and legacy clauses never duplicates a row. This is
  %% only needed for the first post-upgrade load of a ?SaveETS-persisted table.
  ets:select(rpz_ixfr_table,[
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3','_'},[{'>','$3',Serial},{'=<','$3',Zone#rpz.serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3','_'},[{'>','$2',Serial},{'=<','$2',Zone#rpz.serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'>','$3',Serial},{'=<','$3',Zone#rpz.serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'>','$2',Serial},{'=<','$2',Zone#rpz.serial}],['$$']}
  ]);

read_db_record(ets,Zone,Serial,updated) ->
%  io:fwrite(group_leader(),"Read updated records. Zone ~p Serial ~p ~n",[Zone,Serial]),
  %% Task 15.1 / R6 upgrade tolerance: trailing legacy 2-value-element clauses
  %% keep pre-upgrade rows readable (mask 0) until the forced AXFR rewrites them.
  ets:select(rpz_ixfr_table,[
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3','_'},[{'=<','$3',Serial},{'>=','$3',Zone#rpz.serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3','_'},[{'=<','$2',Serial},{'>','$2',Zone#rpz.serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'=<','$3',Serial},{'>=','$3',Zone#rpz.serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'=<','$2',Serial},{'>','$2',Zone#rpz.serial}],['$$']}
  ]);


read_db_record(ets,Zone,Serial,new) ->
%  io:fwrite(group_leader(),"Read expired records. Zone ~p Serial ~p ~n",[Zone,Serial]),
  %% Task 15.1 / R6 upgrade tolerance: trailing legacy 2-value-element clauses
  %% keep pre-upgrade rows readable (mask 0) until the forced AXFR rewrites them.
  ets:select(rpz_ixfr_table,[
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3','_'},[{'>','$2',Serial},{'>','$3',Zone#rpz.serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3','_'},[{'==','$3',0},{'>','$2',Serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'>','$2',Serial},{'>','$3',Zone#rpz.serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'==','$3',0},{'>','$2',Serial}],['$$']}
  ]);

read_db_record(ets,Zone,Serial,expired) ->
%  io:fwrite(group_leader(),"Read expired records. Zone ~p Serial ~p ~n",[Zone,Serial]),
  %% Task 15.1 / R6 upgrade tolerance: trailing legacy 2-value-element clause
  %% keeps pre-upgrade rows readable (mask 0) until the forced AXFR rewrites them.
  ets:select(rpz_ixfr_table,[
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3','_'},[{'=<','$2',Serial},{'>=','$3',Serial},{'=<','$3',Zone#rpz.serial}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'=<','$2',Serial},{'>=','$3',Serial},{'=<','$3',Zone#rpz.serial}],['$$']}
  ]);


read_db_record(ets,Zone,_Serial,active) -> %All not expired
  %% Task 15.1 / R6 upgrade tolerance: trailing legacy 2-value-element clauses
  %% keep pre-upgrade rows readable (mask 0) until the forced AXFR rewrites them.
  ets:select(rpz_ixfr_table,[
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3','_'},[{'>','$3',Zone#rpz.serial},{'>=','$2',Zone#rpz.serial_ixfr}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3','_'},[{'==','$3',0},{'>=','$2',Zone#rpz.serial_ixfr}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'>','$3',Zone#rpz.serial},{'>=','$2',Zone#rpz.serial_ixfr}],['$$']},
    {{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'==','$3',0},{'>=','$2',Zone#rpz.serial_ixfr}],['$$']}
  ]);

read_db_record(mnesia,_Zone,_Serial,all) -> ok;
read_db_record(mnesia,_Zone,_Serial,updated) -> ok;
read_db_record(mnesia,_Zone,_Serial,new) -> ok;
read_db_record(mnesia,_Zone,_Serial,expired) -> ok;
read_db_record(mnesia,_Zone,_Serial,active) -> ok.


%% @doc Writes IOC records to the IXFR cache for a zone.
%%
%% Only writes records if the zone has caching enabled (`Zone#rpz.cache == <<"true">>').
%%
%% For AXFR updates: inserts all non-expired IOCs directly into `rpz_ixfr_table'
%% with the zone's current serial as the add-serial.
%%
%% For IXFR updates: computes the delta between the new IOC list and existing
%% records in the table using `ordsets:subtract/2', then calls `update_db_record/9'
%% for each new or changed indicator to handle insert/update/expiry logic.
%%
%% @param Zone An `#rpz{}' record with caching enabled
%% @param IOCs A list of `{IOC, IOCExp, IoCType}' tuples
%% @param XFR Either `axfr' or `ixfr' indicating the update type
%% @returns `{ok, Count}' where Count is the number of new/changed records
%% @end
write_db_record(Zone,IOC,XFR) when Zone#rpz.cache == <<"true">> -> %, Zone#rpz.ixfr_update_time/=0 -> %TODO check why was checked here?
  write_db_record(?DBStorage,Zone,IOC,XFR);
write_db_record(_Zone,_IOC,_XFR) ->
  {ok,0}.

write_db_record(ets,Zone,IOCs,axfr) ->
  CTime=erlang:system_time(seconds),

  %clean up after closing the issue 17
  %% Store the per-zone source mask as the 4th value element (design §3.2/§6.1):
  %%   {{ioc,Zone,IOC,IoCType}, Serial, IOCExp, Mask}
  %% Defensive: accept both 4-tuples {IOC,Exp,Type,Mask} and legacy 3-tuples
  %% {IOC,Exp,Type} (⇒ Mask=0) so residual 3-tuple callers stay safe during staging.
  NRbefore=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3','$4'},[],['true']}]), % to debug issue 17
  [ets:insert(rpz_ixfr_table, {{ioc,Zone#rpz.zone,IOC,IoCType},Zone#rpz.serial,IOCExp,Mask}) || {IOC,IOCExp,IoCType,Mask} <- [normalize_ioc_mask(IOCEntry) || IOCEntry <- IOCs], (IOCExp > CTime) or (IOCExp == 0)],
  NRafter=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3','$4'},[],['true']}]), % to debug issue 17
   ?logDebugMSG("AXFR update ets. Zone ~p. Before ~p After ~p Indicators ~p~n",[Zone#rpz.zone_str, NRbefore, NRafter,length(IOCs)]), % to debug issue 17
	{ok,0}; %length(IOCs)

write_db_record(mnesia,_Zone,{_IOC,_IOCExp,_IoCType},axfr) ->
	{ok,0};

write_db_record(ets,Zone,IOCs,ixfr) when IOCs /= [] ->
  CTime=erlang:system_time(seconds),
	?logDebugMSG("Fetching zone ~p from ets~n",[Zone#rpz.zone_str]),
	%% IXFR diff projection (design §6.2, R5/R6). The stored value now carries a
	%% 4th element (Mask, '$5') since task 7 (AXFR mask storage). Match that extra
	%% element with a wildcard so IOCDB is not silently empty; keep the projection
	%% at the 3-tuple {IOC,ExpSerial,IoCType} so the mask is EXCLUDED from the diff
	%% and never fabricates spurious serial deltas.
	IOCDB=ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3','$4','$5'},[],[{{'$1','$4','$2'}}]}]),
	?logDebugMSG("Finding new or updated records~n",[]),
	%% Project the incoming 4-tuples {IOC,Exp,Type,Mask} to 3-tuples {IOC,Exp,Type}
	%% for the diff so serial/diff logic is identical to the pre-attribution path
	%% (masks never perturb the delta). normalize_ioc_mask/1 tolerates residual
	%% 3-tuple callers during staging.
	IOCs3=[{I,E,T} || {I,E,T,_M} <- [normalize_ioc_mask(IOCEntry) || IOCEntry <- IOCs]],
	IOCNEW=ordsets:subtract(ordsets:from_list(IOCs3),ordsets:from_list(IOCDB)),

%	?logDebugMSG("Update ets. New ~p, DB ~p, Delta ~p~n IOCs ~p~n IOCDB ~p~n IOCNEW ~p~n",[ordsets:size(IOCs),ordsets:size(IOCDB),ordsets:size(IOCNEW),IOCs,IOCDB,IOCNEW]),
	?logDebugMSG("Update ets. New ~p, DB ~p, Delta ~p~n",[length(IOCs),length(IOCDB),ordsets:size(IOCNEW)]),
	%% Recover each new indicator's Mask (task 8.2, R5/R6). The diff key is the
	%% 3-tuple {IOC,ExpSerial,IoCType} (mask excluded so serials don't churn), so
	%% build a {IOC,Exp,Type} => Mask lookup from the incoming 4-tuples and pass
	%% the mask into update_db_record so new/updated IXFR rows persist it. Duplicate
	%% keys with differing masks are OR-combined (merge_dedup merges upstream, so
	%% this is belt-and-suspenders). Absent ⇒ 0 (untracked).
	IOCMaskMap=lists:foldl(fun({I,E,T,M},Acc) -> maps:update_with({I,E,T}, fun(Old) -> Old bor M end, M, Acc) end, #{}, [normalize_ioc_mask(IOCEntry) || IOCEntry <- IOCs]),
  [update_db_record(?DBStorage,Zone#rpz.zone,Zone#rpz.serial,IOC,IOCExp,IoCType,maps:get({IOC,IOCExp,IoCType},IOCMaskMap,0),ets:lookup(rpz_ixfr_table, {ioc,Zone#rpz.zone,IOC,IoCType}),CTime) || {IOC,IOCExp,IoCType} <- IOCNEW],
	{ok,ordsets:size(IOCNEW)};

write_db_record(ets,Zone,IOCs,ixfr) when IOCs == [] ->
	?logDebugMSG("Zone ~p incremental request returned no new indicators~n",[Zone#rpz.zone_str]),
	{ok,0};

write_db_record(mnesia,_Zone,_IOCs,ixfr) ->
	{ok,0};

write_db_record(_DBStorage,_Zone,_IOCs,_XFR) ->
	{ok,0}. %non cached zones

%% @private
%% @doc Normalizes an in-flight indicator tuple to the 4-tuple
%% `{IOC, IOCExp, IoCType, Mask}' form. Accepts the new 4-tuple as-is and
%% tolerates legacy 3-tuples `{IOC, IOCExp, IoCType}' by defaulting `Mask' to 0
%% (untracked). See design §3.3 / §6.1.
normalize_ioc_mask({IOC,IOCExp,IoCType,Mask}) -> {IOC,IOCExp,IoCType,Mask};
normalize_ioc_mask({IOC,IOCExp,IoCType}) -> {IOC,IOCExp,IoCType,0}.

%% update_db_record/9 (task 8.2, R5/R6): the `Mask' argument (7th position, after
%% IoCType) carries the incoming per-source bitmask for the new/updated indicator.
%% The stored `rpz_ixfr_table' object is now the 4-element value
%% {{ioc,Zone,IOC,IoCType}, Serial, IOCExp, Mask} (task 7), so the existing-row
%% lookups match 4 value elements and inserts/deletes carry the mask. Note
%% ets:delete_object requires an EXACT object match, hence the stored OMask is
%% captured and reused in the delete pattern.
update_db_record(ets, _Zone, _Serial, _IOC, IOCExp, _IoCType, _Mask, [], CTime) when IOCExp > 0,IOCExp =< CTime ->
	%?logDebugMSG("Bypassing ~p ~p ~p ~p ~p ~n",[Serial, IOC, IOCExp, false, CTime]),
	ok; % do not add new but expired indicators

update_db_record(ets, Zone, _Serial, IOC, IOCExp, IoCType, Mask, [{{ioc,_,_,_},OSerial,ExpTime,OMask}], CTime) when ExpTime < IOCExp, IOCExp >= CTime ->
	ets:delete_object(rpz_ixfr_table,{{ioc,Zone,IOC,IoCType},OSerial,ExpTime,OMask}),ets:insert_new(rpz_ixfr_table, {{ioc,Zone,IOC,IoCType},OSerial,IOCExp,Mask bor OMask});

update_db_record(ets, Zone, Serial, IOC, IOCExp, IoCType, Mask, [{{ioc,_,_,_},_OSerial,ExpTime,_OMask}], CTime) when IOCExp > 0, IOCExp > CTime, ExpTime == 0 ->
	ets:select_delete(rpz_ixfr_table,[{{{ioc,Zone,IOC,IoCType},'_','_','_'},[],[true]}]),ets:insert_new(rpz_ixfr_table, {{ioc,Zone,IOC,IoCType},Serial,IOCExp,Mask});

update_db_record(ets, Zone, Serial, IOC, IOCExp, IoCType, Mask, [], CTime) when IOCExp > CTime ; IOCExp == 0 ->
	%?logDebugMSG("Update ~p ~p ~p ~p ~p ~n",[Serial, IOC, IOCExp, false, CTime]),
	ets:insert_new(rpz_ixfr_table, {{ioc,Zone,IOC,IoCType},Serial,IOCExp,Mask}); %insert for duplicate_bag

update_db_record(ets, Zone, Serial, IOC, IOCExp, IoCType, _Mask, Update, CTime) -> %ok; %not new but IOCExp =< CTime, e.g. IOCExp=0 and we cached an indicator with a real expiration time (ExpTime)
	?logDebugMSG("Not expected update ~p ~p ~p ~p ~p ~p ~p ~n",[Zone, Serial, IOC, IOCExp, IoCType, Update, CTime]);

update_db_record(mnesia, _Zone, _Serial, _IOC, _IOCExp, _IoCType, _Mask, _Update, _CTime) -> ok.

%%%
%%% Lookup if an indicator is in the DB.
%%% Recurs - validate hosts/fqdns if they are blocked by a wildcard rule or a subnet.
%%%
%% @doc Looks up whether an IOC exists in the IXFR database.
%%
%% When `Recurs' is `false', performs a direct lookup for the exact IOC string
%% in `rpz_ixfr_table'.
%%
%% When `Recurs' is `true', first checks if the IOC is an IP address (IPv4 or
%% IPv6). If so, does a direct lookup. If it's a domain name, recursively checks
%% each parent domain label (from TLD upward) to detect wildcard blocking rules
%% (e.g., if `evil.example.com' is queried, also checks `example.com' and `com').
%%
%% @param IOC The indicator binary string (domain name or IP address)
%% @param Recurs `true' to check parent domains for wildcard matches, `false'
%%        for exact match only
%% @returns `{ok, [{IOC, Matches}]}' where Matches is a list of
%%          `{Zone, AddSerial, ExpSerial}' tuples for each matching zone
%% @end
lookup_db_record(IOC, Recurs) ->
	lookup_db_record(?DBStorage, IOC, Recurs).

lookup_db_record(ets, IOC, false) ->
	%% Task 15.1 / R6 upgrade tolerance: the second (legacy) match-spec clause
	%% matches pre-upgrade rows that have only 2 value elements
	%% {AddSerial,ExpSerial} and projects a mask of 0 (unknown), so a legacy row
	%% is still returned (as {Zone,AddSerial,ExpSerial,0}) instead of being
	%% invisible until the source-signature-forced AXFR (task 3) rewrites the
	%% zone. A stored object matches exactly one value arity, so the two clauses
	%% never both fire for the same row.
	Rows = ets:select(rpz_ixfr_table,[
		{{{ioc,'$0',IOC, '_'},'$2','$3','$4'},[],[{{'$0','$2','$3','$4'}}]},
		{{{ioc,'$0',IOC, '_'},'$2','$3'},[],[{{'$0','$2','$3',0}}]}]),
	{ok,[{IOC,merge_zone_masks(Rows)}]};

lookup_db_record(mnesia, IOC, false) ->
	{ok,[{IOC,[]}]};

lookup_db_record(ets, IOC, true) ->
% check IP or domain
			%ioc2rpz_fun:logMessage("Checking IOC ~s ~n",[IOC]),
			{ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}(\\/[0-9]{1,3})?)$|(:)"),
      case re:run(IOC,MP,[global,notempty,{capture,[1],binary}]) of
        %% Task 15.1 / R6: legacy 2-value-element clause projects mask 0 (see the
        %% `false` clause above) so pre-upgrade IP rows stay readable.
        {match,_} -> {ok,[{IOC,merge_zone_masks(ets:select(rpz_ixfr_table,[{{{ioc,'$0',IOC, '_'},'$2','$3','$4'},[],[{{'$0','$2','$3','$4'}}]},{{{ioc,'$0',IOC, '_'},'$2','$3'},[],[{{'$0','$2','$3',0}}]}]))}]};
        _ ->  lookup_db_record(ets,IOC,<<"">>,ioc2rpz_fun:rsplit_tail(IOC, <<".">>),[])
      end;

lookup_db_record(mnesia, IOC, true) ->
	{ok,[{IOC,[]}]}.

lookup_db_record(ets,IOC, _FQDN, [], Result) ->
  %ioc2rpz_fun:logMessage("Result: ~p\n\n",[{ok,Result}]),
	FResult = [{IOC2,ARR} || {IOC2,ARR} <-Result, ((IOC == IOC2) or (ARR /= []))],
	{ok,FResult};

lookup_db_record(ets,IOC, FQDN, [Label|REST], Result) ->
	NFQDN = if FQDN == <<"">> -> Label; true ->  <<Label/binary,".",FQDN/binary>> end,
  %ioc2rpz_fun:logMessage("Checking ~p ~n",[NFQDN]),
	%% Task 15.1 / R6: legacy 2-value-element clause projects mask 0 (see the
	%% `false` clause) so pre-upgrade parent-label rows stay readable.
	lookup_db_record(ets, IOC, NFQDN, REST, Result ++ [{NFQDN,merge_zone_masks(ets:select(rpz_ixfr_table,[{{{ioc,'$0',NFQDN,'_'},'$2','$3','$4'},[],[{{'$0','$2','$3','$4'}}]},{{{ioc,'$0',NFQDN,'_'},'$2','$3'},[],[{{'$0','$2','$3',0}}]}]))}]).

%% @doc Groups raw `{Zone, AddSerial, ExpSerial, Mask}' rows (as returned by a
%% `duplicate_bag' select) by `{Zone, AddSerial, ExpSerial}' and OR-combines the
%% source `Mask' across rows that share that key. Produces one
%% `{Zone, AddSerial, ExpSerial, Mask}' tuple per distinct
%% `{Zone, AddSerial, ExpSerial}', so the returned mask reflects all
%% currently-contributing sources for a zone (design §6.4). Genuinely-distinct
%% serial rows stay separate, preserving the current API cardinality. The result
%% is sorted for a stable ordering.
%% @end
merge_zone_masks(Rows) ->
	Merged = lists:foldl(
		fun({Zone,AddSerial,ExpSerial,Mask}, Acc) ->
			Key = {Zone,AddSerial,ExpSerial},
			maps:update_with(Key, fun(M) -> M bor Mask end, Mask, Acc)
		end, #{}, Rows),
	lists:sort([{Zone,AddSerial,ExpSerial,Mask}
		|| {{Zone,AddSerial,ExpSerial},Mask} <- maps:to_list(Merged)]).


%% @doc Deletes old IOC records from the IXFR cache for a given zone.
%%
%% When `Zone#rpz.serial' is 42 (magic value for full cleanup), deletes ALL
%% IOC records and the IXFR zone config entry for the zone from `rpz_ixfr_table'.
%% Otherwise, selectively deletes IOC records whose add-serial is older than
%% the zone's current serial.
%%
%% @param Zone An `#rpz{}' record with `zone' and `serial' fields set.
%%        Use `serial=42' to remove all data for the zone.
%% @returns `true' or `ok'
%% @end
delete_old_db_record(Zone) ->
  delete_old_db_record(?DBStorage,Zone).


delete_old_db_record(ets, Zone) when Zone#rpz.serial == 42 ->
  %?logDebugMSG("Removing IXFR zone ~p ~n",[Zone#rpz.zone_str]),
  %% IOC objects now carry a 4-element key {ioc,Zone,IOC,IoCType} plus 3 value
  %% elements {AddSerial,ExpSerial,Mask}. (task 9.2, design §6.5, R1)
  ets:match_delete(rpz_ixfr_table,{{ioc,Zone#rpz.zone,'_','_'},'_','_','_'}),
  %% ixfr_rpz_cfg rows: match_delete needs an exact arity, so issue one delete
  %% per known shape. Current rows carry 6 value elements (task 3.1 added the
  %% source signature); legacy pre-upgrade rows carry 5 value elements.
  ets:match_delete(rpz_ixfr_table,{{ixfr_rpz_cfg,Zone#rpz.zone},'_','_','_','_','_','_'}),
  ets:match_delete(rpz_ixfr_table,{{ixfr_rpz_cfg,Zone#rpz.zone},'_','_','_','_','_'});

delete_old_db_record(ets, Zone) ->
  NRbefore=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3','_'},[],['true']}]),
  ets:select_delete(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'_','_'},'$1','_','_'},[{'<','$1',Zone#rpz.serial}],[true]}]),
  NRafter=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3','_'},[],['true']}]),
  if NRbefore /= NRafter -> ?logDebugMSG("Delete old records from zone ~p.  before ~p after ~p ~n",[Zone#rpz.zone_str, NRbefore, NRafter]); true -> ok end;
delete_old_db_record(mnesia, _Zone) ->
ok.

%% @doc Removes all cached AXFR and IXFR data for the given RPZ zones.
%%
%% Iterates over all zones stored in the AXFR and IXFR tables, and for each
%% zone whose binary name appears in the provided `RPZ' list, deletes both
%% the AXFR packets (via `delete_db_pkt/1') and IXFR records (via
%% `delete_old_db_record/1') using the magic serial value 42 for full cleanup.
%%
%% @param RPZ A list of `#rpz{}' records whose cached data should be purged
%% @returns A list of cleanup results (one per zone cleaned)
%% @end
clean_DB(RPZ) ->
  AXFR=get_allzones_info(ets,axfr),
  RPZn = [X#rpz.zone || X <- RPZ ],
  [{?logDebugMSG("Zone ~p removing from AXFR cache ~n",[Y]), delete_db_pkt(#rpz{zone=X,zone_str=Y,serial=42}),delete_old_db_record(#rpz{zone=X,zone_str=Y,serial=42})} || [X,Y|_] <- AXFR, lists:member(X, RPZn) ],
  IXFR=get_allzones_info(ets,ixfr),
  [{?logDebugMSG("Zone ~p removing from IXFR cache ~n",[Y]), delete_db_pkt(#rpz{zone=X,zone_str=Y,serial=42}),delete_old_db_record(#rpz{zone=X,zone_str=Y,serial=42})} || [X,Y|_] <- IXFR, lists:member(X, RPZn) ]. 

%% @doc Retrieves stored zone configuration metadata from the cache.
%%
%% For `axfr': reads the `{axfr_rpz_cfg, Zone}' entry from `rpz_axfr_table',
%% returning fields: zone_str, serial, soa_timers, cache, wildcards, sources,
%% ioc_md5, update_time, ioc_count, rule_count.
%%
%% For `ixfr': reads the `{ixfr_rpz_cfg, Zone}' entry from `rpz_ixfr_table',
%% returning fields: zone_str, serial, serial_ixfr, ixfr_update_time,
%% ixfr_nz_update_time.
%%
%% @param Zone An `#rpz{}' record with the `zone' field set
%% @param DB Either `axfr' or `ixfr'
%% @returns A list of matched field values, or `ok' for mnesia
%% @end
get_zone_info(Zone,DB) ->
  get_zone_info(?DBStorage,Zone,DB).

get_zone_info(ets,Zone,axfr) ->
  ets:match(rpz_axfr_table,{{axfr_rpz_cfg,Zone#rpz.zone},'$0','$1','$2','$3','$4','$5','$6','$7','$8','$9'});
get_zone_info(ets,Zone,ixfr) ->
  %% Prefer the new 6-field row (trailing source-list signature, '$5'); fall
  %% back to the legacy 5-field row for pre-upgrade cached zones so loading
  %% still works. (Full migration tolerance is task 15.)
  case ets:match(rpz_ixfr_table,{{ixfr_rpz_cfg,Zone#rpz.zone},'$0','$1','$2','$3','$4','$5'}) of
    [] -> ets:match(rpz_ixfr_table,{{ixfr_rpz_cfg,Zone#rpz.zone},'$0','$1','$2','$3','$4'});
    New -> New
  end;
get_zone_info(mnesia,_Zone,axfr) ->
  ok;
get_zone_info(mnesia,_Zone,ixfr) ->
  ok.

%% @doc Retrieves configuration metadata for all zones in the cache.
%%
%% Similar to `get_zone_info/2' but matches all zone entries (wildcard on zone
%% name). Returns a list of lists, one per zone, containing the zone binary
%% name followed by the same metadata fields as `get_zone_info/2'.
%%
%% @param DB Either `axfr' or `ixfr'
%% @returns A list of zone metadata lists, or `ok' for mnesia
%% @end
get_allzones_info(DB) ->
  get_allzones_info(?DBStorage,DB).

get_allzones_info(ets,axfr) ->
  ets:match(rpz_axfr_table,{{axfr_rpz_cfg,'$0'},'$1','$2','$3','$4','$5','$6','$7','$8','$9','$10'});
get_allzones_info(ets,ixfr) ->
  %% New 6-field rows (with trailing source signature '$6') and legacy 5-field
  %% rows. A row matches exactly one pattern by its arity, so concatenating the
  %% two matches covers both without duplicates.
  ets:match(rpz_ixfr_table,{{ixfr_rpz_cfg,'$0'},'$1','$2','$3','$4','$5','$6'}) ++
  ets:match(rpz_ixfr_table,{{ixfr_rpz_cfg,'$0'},'$1','$2','$3','$4','$5'});
get_allzones_info(mnesia,axfr) ->
  ok;
get_allzones_info(mnesia,ixfr) ->
  ok.



%% @doc Persists all cached zone data to disk files.
%%
%% Only operates when `?SaveETS' is `true' and `?DBStorage' is `ets'.
%% First saves zone configuration metadata for all cached RPZ zones (via
%% `save_zone_info/1'), then writes the full `rpz_axfr_table' and
%% `rpz_ixfr_table' to files in the configured `db_dir' directory using
%% `ets:tab2file/3' with MD5 checksums and object counts for integrity
%% verification on reload.
%% @end
saveZones() when ?SaveETS == true, ?DBStorage == ets ->
  [ save_zone_info(X) || [X] <- ets:match(cfg_table,{[rpz,'_'],'_','$4'}),  X#rpz.cache == <<"true">>],
  [[DBDir]] = ets:match(cfg_table,{db_dir,'$1'}),
  tab2file(?DBStorage,rpz_axfr_table,DBDir++"/ioc2rpz_axfr_table.db"),
  tab2file(?DBStorage,rpz_ixfr_table,DBDir++"/ioc2rpz_ixfr_table.db");

saveZones() -> ok.

%% @doc Loads persisted zone data from disk files.
%%
%% Only operates when `?SaveETS' is `true' and `?DBStorage' is `ets'.
%% Reads the `db_dir' from `cfg_table' and delegates to `loadZones/1'.
%% @end
loadZones() when ?SaveETS == true, ?DBStorage == ets ->
  [[DBDir]] = ets:match(cfg_table,{db_dir,'$1'}),
  loadZones(DBDir);

loadZones() -> ok.

%% @doc Loads persisted AXFR and IXFR ETS tables from the given directory.
%%
%% Attempts to restore `rpz_axfr_table' and `rpz_ixfr_table' from
%% `ioc2rpz_axfr_table.db' and `ioc2rpz_ixfr_table.db' files respectively,
%% using `ets:file2tab/2' with verification enabled (MD5 checksum and object
%% count validation).
%%
%% @param DBDir The directory path containing the persisted table files
%% @returns A list of two `{Status, Table}' tuples, one for AXFR and one for IXFR,
%%          where Status is `ok' on success or an error tuple on failure
%% @end
loadZones(DBDir)  ->
  STA=file2tab(?DBStorage,DBDir++"/ioc2rpz_axfr_table.db"),
  STI=file2tab(?DBStorage,DBDir++"/ioc2rpz_ixfr_table.db"),
  [STA,STI].

%% @doc Saves zone configuration metadata to both AXFR and IXFR cache tables.
%%
%% Inserts an `{axfr_rpz_cfg, Zone}' entry into `rpz_axfr_table' containing
%% zone_str, serial, soa_timers, cache, wildcards, sources, ioc_md5,
%% update_time, ioc_count, and rule_count fields.
%%
%% Also inserts an `{ixfr_rpz_cfg, Zone}' entry into `rpz_ixfr_table'
%% containing zone_str, serial, serial_ixfr, ixfr_update_time, and
%% ixfr_nz_update_time fields.
%%
%% This metadata is used by `loadZones/1' to restore zone state after a
%% server restart, and by `get_zone_info/2' to retrieve zone configuration
%% without accessing `cfg_table'.
%%
%% @param Zone An `#rpz{}' record with all relevant fields populated
%% @returns `true' (from `ets:insert/2'), or `ok' for mnesia
%% @end
save_zone_info(Zone) ->
  save_axfr_zone_info(Zone),
  save_ixfr_zone_info(Zone).

save_axfr_zone_info(Zone) ->
  save_axfr_zone_info(?DBStorage,Zone).
save_axfr_zone_info(ets,Zone) ->
  ets:insert(rpz_axfr_table, {{axfr_rpz_cfg,Zone#rpz.zone},Zone#rpz.zone_str,Zone#rpz.serial,Zone#rpz.soa_timers, Zone#rpz.cache, Zone#rpz.wildcards, Zone#rpz.sources, Zone#rpz.ioc_md5, Zone#rpz.update_time, Zone#rpz.ioc_count, Zone#rpz.rule_count});

save_axfr_zone_info(mnesia,_Zone) ->
  ok.

save_ixfr_zone_info(Zone) ->
  save_ixfr_zone_info(?DBStorage,Zone).
save_ixfr_zone_info(ets,Zone) ->
  %% Append the ordered-source-name signature as the trailing element so a
  %% source-list change (added/removed/reordered sources) is detectable across
  %% restarts. The compare/forceAXFR logic reads it in task 3.2.
  ets:insert(rpz_ixfr_table, {{ixfr_rpz_cfg,Zone#rpz.zone},Zone#rpz.zone_str,Zone#rpz.serial,Zone#rpz.serial_ixfr,Zone#rpz.ixfr_update_time,Zone#rpz.ixfr_nz_update_time,source_signature(Zone#rpz.sources)});
save_ixfr_zone_info(mnesia,_Zone) ->
  ok.

%% @doc Computes an order-sensitive signature of a zone's source list.
%%
%% Source masks are per-zone positional (bit i = i-th entry of the source
%% list), so reordering or editing the list invalidates stored masks. This
%% signature is a SHA-256 over the ordered source names; any add, removal, or
%% reorder yields a different value, which the zone-load path uses to force an
%% AXFR rebuild (design.md §7.2).
%%
%% `#rpz.sources' is the ordered list of source names, so the ordered list is
%% hashed directly.
%%
%% @param Sources The zone's ordered `#rpz.sources' list of source names
%% @returns A 32-byte binary hash
%% @end
source_signature(Sources) ->
  crypto:hash(sha256, term_to_binary(Sources)).

tab2file(ets,Tbl_Name,File_Name) ->
  ets:tab2file(Tbl_Name,File_Name,[{extended_info,[object_count,md5sum]},{sync,true}]);
tab2file(_DBStorage,_Tbl_Name,_File_Name) -> ok.

file2tab(ets,File_Name) ->
  ets:file2tab(File_Name,[{verify,true}]);
file2tab(_DBStorage,_File_Name) -> ok.

%%%%
%%%% EUnit tests
%%%%

-ifdef(TEST).

%% Same ordered source list ⇒ identical signature (deterministic).
source_signature_same_list_test() ->
  Sources = [<<"abuse-ch">>, <<"internal-list">>, <<"partner-feed">>],
  ?assertEqual(source_signature(Sources), source_signature(Sources)).

%% Reordering the source list ⇒ different signature (order-sensitive), because
%% masks are positional and reordering invalidates them.
source_signature_reorder_differs_test() ->
  A = [<<"abuse-ch">>, <<"internal-list">>],
  B = [<<"internal-list">>, <<"abuse-ch">>],
  ?assertNotEqual(source_signature(A), source_signature(B)).

%% Adding/removing a source ⇒ different signature.
source_signature_membership_differs_test() ->
  A = [<<"abuse-ch">>, <<"internal-list">>],
  B = [<<"abuse-ch">>, <<"internal-list">>, <<"partner-feed">>],
  ?assertNotEqual(source_signature(A), source_signature(B)).

%% Signature is a 32-byte SHA-256 binary.
source_signature_shape_test() ->
  Sig = source_signature([<<"s0">>, <<"s1">>]),
  ?assert(is_binary(Sig)),
  ?assertEqual(32, byte_size(Sig)).

%% Helper: create a fresh rpz_ixfr_table matching init_db's type
%% (duplicate_bag, public, named_table) for AXFR write tests.
setup_ixfr_table() ->
  catch ets:delete(rpz_ixfr_table),
  ets:new(rpz_ixfr_table, [duplicate_bag, public, named_table]).

teardown_ixfr_table(_) ->
  catch ets:delete(rpz_ixfr_table),
  ok.

%% AXFR write stores the source mask as the 4th value element:
%%   {{ioc,Zone,IOC,Type}, Serial, Exp, Mask}      (design §3.2/§6.1, R1/R5)
write_db_record_axfr_stores_mask_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example", serial = 1572419220},
     IOC = <<"bad.example.com">>,
     Type = fqdn,
     Exp = 0,
     Mask = 5,
     {ok,0} = write_db_record(ets, Zone, [{IOC,Exp,Type,Mask}], axfr),
     Objs = ets:lookup(rpz_ixfr_table, {ioc, Zone#rpz.zone, IOC, Type}),
     ?assertEqual([{{ioc, Zone#rpz.zone, IOC, Type}, Zone#rpz.serial, Exp, Mask}], Objs)
   end}.

%% Defensive path: a legacy 3-tuple {IOC,Exp,Type} is tolerated and stored
%% with Mask = 0.
write_db_record_axfr_legacy_3tuple_mask0_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example", serial = 42},
     IOC = <<"legacy.example.com">>,
     Type = fqdn,
     Exp = 0,
     {ok,0} = write_db_record(ets, Zone, [{IOC,Exp,Type}], axfr),
     Objs = ets:lookup(rpz_ixfr_table, {ioc, Zone#rpz.zone, IOC, Type}),
     ?assertEqual([{{ioc, Zone#rpz.zone, IOC, Type}, Zone#rpz.serial, Exp, 0}], Objs)
   end}.

%% IXFR diff stability (design §6.2, R5/R6): the mask (4th stored value element)
%% MUST NOT perturb the serial/diff logic. Pre-populate the table via the AXFR
%% path (which stores 4-element masked objects), then run the IXFR path with the
%% SAME indicators as 4-tuples — the delta must be 0 (nothing looks "new").
write_db_record_ixfr_same_set_no_delta_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example",
                 cache = <<"true">>, serial = 1572419220},
     IOCs = [{<<"a.example.com">>,0,fqdn,1}, {<<"b.example.com">>,0,fqdn,2}],
     {ok,0} = write_db_record(ets, Zone, IOCs, axfr),
     ?assertEqual({ok,0}, write_db_record(ets, Zone, IOCs, ixfr))
   end}.

%% Mask-only differences MUST NOT create a spurious delta on IXFR: the diff
%% projects to {IOC,Exp,Type}, so re-feeding the same indicators with different
%% mask values still yields 0 new (design §6.2, R5/R6).
write_db_record_ixfr_mask_only_change_no_delta_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example",
                 cache = <<"true">>, serial = 1572419220},
     Stored = [{<<"a.example.com">>,0,fqdn,1}, {<<"b.example.com">>,0,fqdn,2}],
     {ok,0} = write_db_record(ets, Zone, Stored, axfr),
     %% Same {IOC,Exp,Type} pairs, only the masks differ.
     Incoming = [{<<"a.example.com">>,0,fqdn,4}, {<<"b.example.com">>,0,fqdn,8}],
     ?assertEqual({ok,0}, write_db_record(ets, Zone, Incoming, ixfr))
   end}.

%% IXFR insert of a NEW indicator persists the incoming mask as the 4th value
%% element (task 8.2, R5/R6). Start from an empty table so the indicator is
%% brand new; the plain-insert clause of update_db_record/9 must carry the mask.
write_db_record_ixfr_new_indicator_stores_mask_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example",
                 cache = <<"true">>, serial = 1572419220},
     IOC = <<"new.example.com">>,
     Incoming = [{IOC,0,fqdn,6}],
     ?assertEqual({ok,1}, write_db_record(ets, Zone, Incoming, ixfr)),
     Objs = ets:lookup(rpz_ixfr_table, {ioc, Zone#rpz.zone, IOC, fqdn}),
     ?assertEqual([{{ioc, Zone#rpz.zone, IOC, fqdn}, Zone#rpz.serial, 0, 6}], Objs)
   end}.

%% Round-trip (task 8.2, R5/R6): AXFR pre-populates masked rows; an IXFR with the
%% SAME set produces 0 new (no churn), and a follow-up IXFR that adds a new
%% indicator stores that indicator's mask as the 4th value element.
write_db_record_ixfr_roundtrip_adds_new_mask_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example",
                 cache = <<"true">>, serial = 1572419220},
     Base = [{<<"a.example.com">>,0,fqdn,1}, {<<"b.example.com">>,0,fqdn,2}],
     {ok,0} = write_db_record(ets, Zone, Base, axfr),
     %% Same set via IXFR ⇒ no new indicators (no serial churn).
     ?assertEqual({ok,0}, write_db_record(ets, Zone, Base, ixfr)),
     %% Now add a new indicator with its own mask.
     NewIOC = <<"c.example.com">>,
     Added = Base ++ [{NewIOC,0,fqdn,4}],
     ?assertEqual({ok,1}, write_db_record(ets, Zone, Added, ixfr)),
     Objs = ets:lookup(rpz_ixfr_table, {ioc, Zone#rpz.zone, NewIOC, fqdn}),
     ?assertEqual([{{ioc, Zone#rpz.zone, NewIOC, fqdn}, Zone#rpz.serial, 0, 4}], Objs)
   end}.

%% read_db_record/4 match specs must match the new 4-element masked objects
%% ({key, AddSerial, ExpSerial, Mask}) and still return the pre-change 4-element
%% '$$' shape [IOC, AddSerial, ExpSerial, IoCType] — the mask is excluded via a
%% trailing wildcard in the match spec (task 9.1, design §6.3, R1).
read_db_record_active_masked_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Z = <<"rpz.example">>,
     Zone = #rpz{zone = Z, zone_str = "rpz.example", serial = 100, serial_ixfr = 10},
     %% Active (never expires): Exp=0, AddSerial >= serial_ixfr.
     A = {{ioc, Z, <<"a.example.com">>, fqdn}, 20, 0, 5},
     %% Active (expires in the future): Exp > serial, AddSerial >= serial_ixfr.
     B = {{ioc, Z, <<"b.example.com">>, fqdn}, 15, 200, 6},
     %% NOT active (already expired: Exp =< serial).
     C = {{ioc, Z, <<"c.example.com">>, fqdn}, 15, 50, 7},
     ets:insert(rpz_ixfr_table, [A, B, C]),
     Got = lists:sort(read_db_record(ets, Zone, 0, active)),
     Expected = lists:sort([[<<"a.example.com">>, 20, 0, fqdn],
                            [<<"b.example.com">>, 15, 200, fqdn]]),
     ?assertEqual(Expected, Got)
   end}.

%% Second type (expired) also matches the 4-element objects and returns the
%% 4-element [IOC, AddSerial, ExpSerial, IoCType] shape with the mask excluded.
read_db_record_expired_masked_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Z = <<"rpz.example">>,
     Zone = #rpz{zone = Z, zone_str = "rpz.example", serial = 150},
     %% Expired: AddSerial =< Serial, Serial =< ExpSerial =< zone serial.
     E = {{ioc, Z, <<"e.example.com">>, fqdn}, 50, 120, 9},
     %% NOT expired (Exp > zone serial).
     F = {{ioc, Z, <<"f.example.com">>, fqdn}, 50, 200, 3},
     ets:insert(rpz_ixfr_table, [E, F]),
     Got = read_db_record(ets, Zone, 100, expired),
     ?assertEqual([[<<"e.example.com">>, 50, 120, fqdn]], Got)
   end}.

%% delete_old_db_record/1 (non-42) removes only IOC rows whose AddSerial is
%% strictly older than the zone serial, leaving current rows intact. The stored
%% objects are 4-element masked rows {key, AddSerial, ExpSerial, Mask}; the
%% match specs must include the 3rd value wildcard for the mask (task 9.2,
%% design §6.5, R1).
delete_old_db_record_removes_stale_only_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Z = <<"rpz.example">>,
     Zone = #rpz{zone = Z, zone_str = "rpz.example", serial = 100},
     %% Stale: AddSerial < 100.
     S1 = {{ioc, Z, <<"stale1.example.com">>, fqdn}, 50, 0, 1},
     S2 = {{ioc, Z, <<"stale2.example.com">>, fqdn}, 99, 0, 2},
     %% Current: AddSerial >= 100.
     C1 = {{ioc, Z, <<"cur1.example.com">>, fqdn}, 100, 0, 4},
     C2 = {{ioc, Z, <<"cur2.example.com">>, fqdn}, 120, 0, 8},
     ets:insert(rpz_ixfr_table, [S1, S2, C1, C2]),
     delete_old_db_record(ets, Zone),
     ?assertEqual([], ets:lookup(rpz_ixfr_table, {ioc, Z, <<"stale1.example.com">>, fqdn})),
     ?assertEqual([], ets:lookup(rpz_ixfr_table, {ioc, Z, <<"stale2.example.com">>, fqdn})),
     ?assertEqual([C1], ets:lookup(rpz_ixfr_table, {ioc, Z, <<"cur1.example.com">>, fqdn})),
     ?assertEqual([C2], ets:lookup(rpz_ixfr_table, {ioc, Z, <<"cur2.example.com">>, fqdn}))
   end}.

%% delete_old_db_record/1 with serial==42 fully removes ALL IOC rows for the
%% zone (regardless of AddSerial) and the current 6-value-element ixfr_rpz_cfg
%% row (task 9.2, design §6.5, R1).
delete_old_db_record_serial42_full_cleanup_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Z = <<"rpz.example">>,
     Zone = #rpz{zone = Z, zone_str = "rpz.example", serial = 42},
     I1 = {{ioc, Z, <<"a.example.com">>, fqdn}, 50, 0, 1},
     I2 = {{ioc, Z, <<"b.example.com">>, fqdn}, 60, 200, 2},
     %% Current cfg row: 6 value elements (zone_str, serial, serial_ixfr,
     %% ixfr_update_time, ixfr_nz_update_time, SourceSignature).
     Cfg = {{ixfr_rpz_cfg, Z}, "rpz.example", 60, 55, 111, 222, <<"sig">>},
     %% A row for a different zone must survive.
     Other = {{ioc, <<"other.example">>, <<"x.example.com">>, fqdn}, 10, 0, 1},
     ets:insert(rpz_ixfr_table, [I1, I2, Cfg, Other]),
     delete_old_db_record(ets, Zone),
     ?assertEqual([], ets:lookup(rpz_ixfr_table, {ioc, Z, <<"a.example.com">>, fqdn})),
     ?assertEqual([], ets:lookup(rpz_ixfr_table, {ioc, Z, <<"b.example.com">>, fqdn})),
     ?assertEqual([], ets:lookup(rpz_ixfr_table, {ixfr_rpz_cfg, Z})),
     ?assertEqual([Other], ets:lookup(rpz_ixfr_table, {ioc, <<"other.example">>, <<"x.example.com">>, fqdn}))
   end}.

%% serial==42 cleanup also removes a legacy 5-value-element ixfr_rpz_cfg row
%% (pre task 3.1) so upgraded caches are fully purged (task 9.2, R1).
delete_old_db_record_serial42_legacy_cfg_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Z = <<"rpz.example">>,
     Zone = #rpz{zone = Z, zone_str = "rpz.example", serial = 42},
     %% Legacy cfg row: 5 value elements (no source signature).
     LegacyCfg = {{ixfr_rpz_cfg, Z}, "rpz.example", 60, 55, 111, 222},
     ets:insert(rpz_ixfr_table, [LegacyCfg]),
     delete_old_db_record(ets, Zone),
     ?assertEqual([], ets:lookup(rpz_ixfr_table, {ixfr_rpz_cfg, Z}))
   end}.

%% lookup_db_record/2 exact (false) lookup returns per-zone
%% {Zone, AddSerial, ExpSerial, Mask} tuples. Rows sharing the same
%% {Zone, AddSerial, ExpSerial} have their masks OR-ed; each zone yields its own
%% entry (design §6.4, R1). Uses the new 4-element masked objects.
lookup_db_record_false_ors_masks_per_zone_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     IOC = <<"bad.example.com">>,
     Za = <<"rpz.a">>,
     Zb = <<"rpz.b">>,
     %% Zone A: two rows, SAME {Zone,AddSerial,ExpSerial} but different masks
     %% (1 and 4) -> must OR to 5.
     RowA1 = {{ioc, Za, IOC, fqdn}, 100, 0, 1},
     RowA2 = {{ioc, Za, IOC, fqdn}, 100, 0, 4},
     %% Zone B: a single row with its own mask -> separate entry.
     RowB  = {{ioc, Zb, IOC, fqdn}, 200, 0, 2},
     ets:insert(rpz_ixfr_table, [RowA1, RowA2, RowB]),
     {ok, [{IOC, Matches}]} = lookup_db_record(ets, IOC, false),
     ?assertEqual([{Za,100,0,5}, {Zb,200,0,2}], lists:sort(Matches))
   end}.

%% lookup_db_record/2 recursive (true) IP lookup returns per-zone
%% {Zone, AddSerial, ExpSerial, Mask} tuples with masks OR-ed across rows that
%% share the same {Zone, AddSerial, ExpSerial} (task 10.2, design §6.4, R1).
lookup_db_record_true_ip_ors_masks_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     IOC = <<"192.0.2.1">>,
     Z   = <<"rpz.a">>,
     %% Two rows, SAME {Zone,AddSerial,ExpSerial} but different masks
     %% (1 and 4) -> must OR to 5.
     Row1 = {{ioc, Z, IOC, ip}, 100, 0, 1},
     Row2 = {{ioc, Z, IOC, ip}, 100, 0, 4},
     ets:insert(rpz_ixfr_table, [Row1, Row2]),
     {ok, [{IOC, Matches}]} = lookup_db_record(ets, IOC, true),
     ?assertEqual([{Z,100,0,5}], lists:sort(Matches))
   end}.

%% lookup_db_record/2 recursive (true) FQDN lookup accumulates parent-label
%% matches as {Zone, AddSerial, ExpSerial, Mask} 4-tuples with per-zone mask
%% OR-ing (task 10.2, design §6.4, R1). A parent entry (example.com) is matched
%% when querying a child (evil.example.com).
lookup_db_record_true_fqdn_recursive_parent_mask_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Parent = <<"example.com">>,
     Child  = <<"evil.example.com">>,
     Z      = <<"rpz.a">>,
     %% Two parent rows sharing {Zone,AddSerial,ExpSerial}, masks 2 and 8 -> 10.
     Row1 = {{ioc, Z, Parent, fqdn}, 300, 0, 2},
     Row2 = {{ioc, Z, Parent, fqdn}, 300, 0, 8},
     ets:insert(rpz_ixfr_table, [Row1, Row2]),
     {ok, Result} = lookup_db_record(Child, true),
     %% The accumulated entry for the parent label carries the OR-ed mask.
     ?assertEqual([{Z,300,0,10}], proplists:get_value(Parent, Result))
   end}.

%% End-to-end round-trip (task 10.3, design §6.1/§6.4, R1): drive the actual
%% AXFR WRITE path with TWO entries for the SAME {IOC,fqdn} carrying different
%% masks (1 and 4) at the SAME serial/exp. The AXFR write inserts one row per
%% incoming tuple into the duplicate_bag, so the table holds two rows; a `false`
%% (exact) lookup MUST OR the masks and return {Zone,Serial,Exp,5}. This proves
%% the DB layer OR-combines whatever rows are present, independent of pre-merge.
lookup_db_record_axfr_roundtrip_two_rows_or_mask_false_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example", serial = 1572419220},
     IOC = <<"bad.example.com">>,
     Exp = 0,
     %% Two rows, same {IOC,fqdn}, same serial/exp, masks 1 and 4.
     {ok,0} = write_db_record(ets, Zone, [{IOC,Exp,fqdn,1}, {IOC,Exp,fqdn,4}], axfr),
     {ok, [{IOC, Matches}]} = lookup_db_record(ets, IOC, false),
     ?assertEqual([{Zone#rpz.zone, Zone#rpz.serial, Exp, 5}], Matches)
   end}.

%% Same AXFR-write round-trip as above, verified through the recursive (`true`)
%% lookup path for an FQDN: the two rows written for {IOC,fqdn} must OR to 5 in
%% the accumulated per-label result (task 10.3, design §6.4, R1).
lookup_db_record_axfr_roundtrip_two_rows_or_mask_true_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example", serial = 1572419220},
     IOC = <<"bad.example.com">>,
     Exp = 0,
     {ok,0} = write_db_record(ets, Zone, [{IOC,Exp,fqdn,1}, {IOC,Exp,fqdn,4}], axfr),
     {ok, Result} = lookup_db_record(ets, IOC, true),
     ?assertEqual([{Zone#rpz.zone, Zone#rpz.serial, Exp, 5}], proplists:get_value(IOC, Result))
   end}.

%% A single PRE-MERGED row (mask already OR-ed to 5 by the build-time merge)
%% written through the AXFR path must ALSO yield 5 on lookup — proving both the
%% "one row" and "two rows" cases converge on the same result (task 10.3, R1).
lookup_db_record_axfr_roundtrip_premerged_mask_false_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example", serial = 1572419220},
     IOC = <<"bad.example.com">>,
     Exp = 0,
     {ok,0} = write_db_record(ets, Zone, [{IOC,Exp,fqdn,5}], axfr),
     {ok, [{IOC, Matches}]} = lookup_db_record(ets, IOC, false),
     ?assertEqual([{Zone#rpz.zone, Zone#rpz.serial, Exp, 5}], Matches)
   end}.

%% IXFR diff unaffected + lookup still correct (task 10.3, design §6.2/§6.4,
%% R5/R6): after an AXFR populate, an IXFR write with the SAME {IOC,Exp,Type}
%% set (even with different masks) yields {ok,0} (no spurious delta), and a
%% subsequent lookup STILL returns the correct OR-ed mask from the stored rows.
write_db_record_ixfr_same_set_lookup_ors_mask_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example",
                 cache = <<"true">>, serial = 1572419220},
     IOC = <<"bad.example.com">>,
     Exp = 0,
     %% AXFR populate: two rows for the same {IOC,fqdn}, masks 1 and 4 -> 5.
     {ok,0} = write_db_record(ets, Zone, [{IOC,Exp,fqdn,1}, {IOC,Exp,fqdn,4}], axfr),
     %% IXFR with the SAME {IOC,Exp,Type} set (different masks) -> no delta.
     ?assertEqual({ok,0}, write_db_record(ets, Zone, [{IOC,Exp,fqdn,2}, {IOC,Exp,fqdn,8}], ixfr)),
     %% Lookup still returns the OR of the rows actually present (the AXFR rows).
     {ok, [{IOC, Matches}]} = lookup_db_record(ets, IOC, false),
     ?assertEqual([{Zone#rpz.zone, Zone#rpz.serial, Exp, 5}], Matches)
   end}.

%% Untracked / single-source write→lookup (task 10.3, R3): an AXFR write with
%% Mask=0 (tracking disabled or single-source feed) round-trips to a lookup that
%% returns mask 0, so attribution is reported as "unknown" downstream.
lookup_db_record_axfr_roundtrip_untracked_mask0_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Zone = #rpz{zone = <<"rpz.example">>, zone_str = "rpz.example", serial = 1572419220},
     IOC = <<"plain.example.com">>,
     Exp = 0,
     {ok,0} = write_db_record(ets, Zone, [{IOC,Exp,fqdn,0}], axfr),
     {ok, [{IOC, Matches}]} = lookup_db_record(ets, IOC, false),
     ?assertEqual([{Zone#rpz.zone, Zone#rpz.serial, Exp, 0}], Matches)
   end}.

%% Task 15.1 / R6 upgrade tolerance: a legacy PRE-UPGRADE row has only 2 value
%% elements {AddSerial,ExpSerial} (no mask). read_db_record(active) MUST tolerate
%% such a row alongside new 3-value-element rows and still return it in the
%% pre-change '$$' shape [IOC,AddSerial,ExpSerial,IoCType] (mask excluded), rather
%% than crashing or silently dropping it.
read_db_record_active_tolerates_legacy_row_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     Z = <<"rpz.example">>,
     Zone = #rpz{zone = Z, zone_str = "rpz.example", serial = 100, serial_ixfr = 10},
     %% New 3-value-element (masked) active row.
     New = {{ioc, Z, <<"new.example.com">>, fqdn}, 20, 0, 5},
     %% Legacy 2-value-element (pre-upgrade) active row: no 4th mask element.
     Legacy = {{ioc, Z, <<"legacy.example.com">>, fqdn}, 15, 0},
     ets:insert(rpz_ixfr_table, [New, Legacy]),
     Got = lists:sort(read_db_record(ets, Zone, 0, active)),
     Expected = lists:sort([[<<"new.example.com">>, 20, 0, fqdn],
                            [<<"legacy.example.com">>, 15, 0, fqdn]]),
     ?assertEqual(Expected, Got)
   end}.

%% Task 15.1 / R6 upgrade tolerance: an exact (false) lookup MUST tolerate a
%% legacy 2-value-element row and return it with mask 0 (unknown), alongside a
%% new 3-value-element row from another zone. This proves legacy rows stay
%% readable during the first post-upgrade load instead of being invisible until
%% the forced AXFR rewrites them.
lookup_db_record_false_tolerates_legacy_row_mask0_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     IOC = <<"bad.example.com">>,
     Za = <<"rpz.legacy">>,
     Zb = <<"rpz.new">>,
     %% Legacy pre-upgrade row: 2 value elements {AddSerial,ExpSerial}, no mask.
     Legacy = {{ioc, Za, IOC, fqdn}, 100, 0},
     %% New masked row (3 value elements) in a different zone.
     New = {{ioc, Zb, IOC, fqdn}, 200, 0, 2},
     ets:insert(rpz_ixfr_table, [Legacy, New]),
     {ok, [{IOC, Matches}]} = lookup_db_record(ets, IOC, false),
     %% Legacy row surfaces with mask 0; new row keeps its mask.
     ?assertEqual([{Za,100,0,0}, {Zb,200,0,2}], lists:sort(Matches))
   end}.

%% Task 15.1 / R6: the recursive (true) IP lookup path is also legacy-tolerant —
%% a legacy 2-value-element IP row resolves to mask 0 rather than being dropped.
lookup_db_record_true_ip_tolerates_legacy_row_mask0_test_() ->
  {setup, fun setup_ixfr_table/0, fun teardown_ixfr_table/1,
   fun() ->
     IOC = <<"192.0.2.1">>,
     Z   = <<"rpz.legacy">>,
     Legacy = {{ioc, Z, IOC, ip}, 100, 0},
     ets:insert(rpz_ixfr_table, [Legacy]),
     {ok, [{IOC, Matches}]} = lookup_db_record(ets, IOC, true),
     ?assertEqual([{Z,100,0,0}], lists:sort(Matches))
   end}.

-endif.
