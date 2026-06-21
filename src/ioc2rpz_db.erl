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
-export([init_db/3,db_table_info/2,read_db_pkt/1,write_db_pkt/2,delete_db_pkt/1,delete_old_db_pkt/1,read_db_record/3,write_db_record/3,delete_old_db_record/1,saveZones/0,loadZones/0,loadZones/1,
        get_zone_info/2,clean_DB/1,save_zone_info/1,get_allzones_info/2, lookup_db_record/2,cleanup_hotcache/0]).


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
  {ok,[]}.


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
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'>','$3',Serial},{'=<','$3',Zone#rpz.serial}],['$$']},{{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'>','$2',Serial},{'=<','$2',Zone#rpz.serial}],['$$']}]);

read_db_record(ets,Zone,Serial,updated) ->
%  io:fwrite(group_leader(),"Read updated records. Zone ~p Serial ~p ~n",[Zone,Serial]),
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'=<','$3',Serial},{'>=','$3',Zone#rpz.serial}],['$$']},{{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'=<','$2',Serial},{'>','$2',Zone#rpz.serial}],['$$']}]);


read_db_record(ets,Zone,Serial,new) ->
%  io:fwrite(group_leader(),"Read expired records. Zone ~p Serial ~p ~n",[Zone,Serial]),
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'>','$2',Serial},{'>','$3',Zone#rpz.serial}],['$$']},{{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'==','$3',0},{'>','$2',Serial}],['$$']}]);

read_db_record(ets,Zone,Serial,expired) ->
%  io:fwrite(group_leader(),"Read expired records. Zone ~p Serial ~p ~n",[Zone,Serial]),
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'=<','$2',Serial},{'>=','$3',Serial},{'=<','$3',Zone#rpz.serial}],['$$']}]);


read_db_record(ets,Zone,_Serial,active) -> %All not expired
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'>','$3',Zone#rpz.serial},{'>=','$2',Zone#rpz.serial_ixfr}],['$$']},{{{ioc,Zone#rpz.zone,'$1','$4'},'$2','$3'},[{'==','$3',0},{'>=','$2',Zone#rpz.serial_ixfr}],['$$']}]);

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
%% records in the table using `ordsets:subtract/2', then calls `update_db_record/8'
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
  NRbefore=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1'},'$2','$3'},[],['true']}]), % to debug issue 17
  [ets:insert(rpz_ixfr_table, {{ioc,Zone#rpz.zone,IOC,IoCType},Zone#rpz.serial,IOCExp}) || {IOC,IOCExp,IoCType} <- IOCs, (IOCExp > CTime) or (IOCExp == 0)],
  NRafter=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1'},'$2','$3'},[],['true']}]), % to debug issue 17
   ?logDebugMSG("AXFR update ets. Zone ~p. Before ~p After ~p Indicators ~p~n",[Zone#rpz.zone_str, NRbefore, NRafter,length(IOCs)]), % to debug issue 17
	{ok,0}; %length(IOCs)

write_db_record(mnesia,_Zone,{_IOC,_IOCExp,_IoCType},axfr) ->
	{ok,0};

write_db_record(ets,Zone,IOCs,ixfr) when IOCs /= [] ->
  CTime=erlang:system_time(seconds),
	?logDebugMSG("Fetching zone ~p from ets~n",[Zone#rpz.zone_str]),
	IOCDB=ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3','$4'},[],[{{'$1','$4','$2'}}]}]),
	?logDebugMSG("Finding new or updated records~n",[]),
	IOCNEW=ordsets:subtract(ordsets:from_list(IOCs),ordsets:from_list(IOCDB)),

%	?logDebugMSG("Update ets. New ~p, DB ~p, Delta ~p~n IOCs ~p~n IOCDB ~p~n IOCNEW ~p~n",[ordsets:size(IOCs),ordsets:size(IOCDB),ordsets:size(IOCNEW),IOCs,IOCDB,IOCNEW]),
	?logDebugMSG("Update ets. New ~p, DB ~p, Delta ~p~n",[length(IOCs),length(IOCDB),ordsets:size(IOCNEW)]),
  [update_db_record(?DBStorage,Zone#rpz.zone,Zone#rpz.serial,IOC,IOCExp,IoCType,ets:lookup(rpz_ixfr_table, {ioc,Zone#rpz.zone,IOC,IoCType}),CTime) || {IOC,IOCExp,IoCType} <- IOCNEW],
	{ok,ordsets:size(IOCNEW)};

write_db_record(ets,Zone,IOCs,ixfr) when IOCs == [] ->
	?logDebugMSG("Zone ~p incremental request returned no new indicators~n",[Zone#rpz.zone_str]),
	{ok,0};

write_db_record(mnesia,_Zone,_IOCs,ixfr) ->
	{ok,0};

write_db_record(_DBStorage,_Zone,_IOCs,_XFR) ->
	{ok,0}. %non cached zones

update_db_record(ets, _Zone, _Serial, _IOC, IOCExp, _IoCType, [], CTime) when IOCExp > 0,IOCExp =< CTime ->
	%?logDebugMSG("Bypassing ~p ~p ~p ~p ~p ~n",[Serial, IOC, IOCExp, false, CTime]),
	ok; % do not add new but expired indicators

update_db_record(ets, Zone, _Serial, IOC, IOCExp, IoCType, [{{ioc,_,_,_},OSerial,ExpTime}], CTime) when ExpTime < IOCExp, IOCExp >= CTime ->
	ets:delete_object(rpz_ixfr_table,{{ioc,Zone,IOC,IoCType},OSerial,ExpTime}),ets:insert_new(rpz_ixfr_table, {{ioc,Zone,IOC,IoCType},OSerial,IOCExp});

update_db_record(ets, Zone, Serial, IOC, IOCExp, IoCType, [{{ioc,_,_,_},_OSerial,ExpTime}], CTime) when IOCExp > 0, IOCExp > CTime, ExpTime == 0 ->
	ets:select_delete(rpz_ixfr_table,[{{{ioc,Zone,IOC,IoCType},'_','_'},[],[true]}]),ets:insert_new(rpz_ixfr_table, {{ioc,Zone,IOC,IoCType},Serial,IOCExp});

update_db_record(ets, Zone, Serial, IOC, IOCExp, IoCType, [], CTime) when IOCExp > CTime ; IOCExp == 0 ->
	%?logDebugMSG("Update ~p ~p ~p ~p ~p ~n",[Serial, IOC, IOCExp, false, CTime]),
	ets:insert_new(rpz_ixfr_table, {{ioc,Zone,IOC,IoCType},Serial,IOCExp}); %insert for duplicate_bag

update_db_record(ets, Zone, Serial, IOC, IOCExp, IoCType, Update, CTime) -> %ok; %not new but IOCExp =< CTime, e.g. IOCExp=0 and we cached an indicator with a real expiration time (ExpTime)
	?logDebugMSG("Not expected update ~p ~p ~p ~p ~p ~p ~p ~n",[Zone, Serial, IOC, IOCExp, IoCType, Update, CTime]);

update_db_record(mnesia, _Zone, _Serial, _IOC, _IOCExp, _IoCType, _Update, _CTime) -> ok.

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
	{ok,[{IOC,ets:select(rpz_ixfr_table,[{{{ioc,'$0',IOC, '_'},'$2','$3'},[],[{{'$0','$2','$3'}}]}])}]};

lookup_db_record(mnesia, IOC, false) ->
	{ok,[{IOC,[]}]};

lookup_db_record(ets, IOC, true) ->
% check IP or domain
			%ioc2rpz_fun:logMessage("Checking IOC ~s ~n",[IOC]),
			{ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}(\\/[0-9]{1,3})?)$|(:)"),
      case re:run(IOC,MP,[global,notempty,{capture,[1],binary}]) of
        {match,_} -> {ok,[{IOC,ets:select(rpz_ixfr_table,[{{{ioc,'$0',IOC, '_'},'$2','$3'},[],[{{'$0','$2','$3'}}]}])}]};
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
	lookup_db_record(ets, IOC, NFQDN, REST, Result ++ [{NFQDN,ets:select(rpz_ixfr_table,[{{{ioc,'$0',NFQDN,'_'},'$2','$3'},[],[{{'$0','$2','$3'}}]}])}]).


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
  ets:match_delete(rpz_ixfr_table,{{ioc,Zone#rpz.zone,'_'},'_','_'}),
  ets:match_delete(rpz_ixfr_table,{{ixfr_rpz_cfg,Zone#rpz.zone},'_','_','_','_'});

delete_old_db_record(ets, Zone) ->
  NRbefore=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3'},[],['true']}]),
  ets:select_delete(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'_','_'},'$1','_'},[{'<','$1',Zone#rpz.serial}],[true]}]),
  NRafter=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3'},[],['true']}]),
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
  ets:match(rpz_ixfr_table,{{ixfr_rpz_cfg,Zone#rpz.zone},'$0','$1','$2','$3','$4'});
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
  ets:insert(rpz_ixfr_table, {{ixfr_rpz_cfg,Zone#rpz.zone},Zone#rpz.zone_str,Zone#rpz.serial,Zone#rpz.serial_ixfr,Zone#rpz.ixfr_update_time,Zone#rpz.ixfr_nz_update_time});
save_ixfr_zone_info(mnesia,_Zone) ->
  ok.

tab2file(ets,Tbl_Name,File_Name) ->
  ets:tab2file(Tbl_Name,File_Name,[{extended_info,[object_count,md5sum]},{sync,true}]);
tab2file(_DBStorage,_Tbl_Name,_File_Name) -> ok.

file2tab(ets,File_Name) ->
  ets:file2tab(File_Name,[{verify,true}]);
file2tab(_DBStorage,_File_Name) -> ok.
