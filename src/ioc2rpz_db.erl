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

%IOC2RPZ DB Cache

-module(ioc2rpz_db).
-include_lib("ioc2rpz.hrl").
-export([init_db/3,db_table_info/2,read_db_pkt/1,write_db_pkt/2,delete_db_pkt/1,read_db_record/3,write_db_record/3,delete_old_db_record/1,saveZones/0,loadZones/0,loadZones/1,
        get_zone_info/2,clean_DB/1,save_zone_info/1,get_allzones_info/2, lookup_db_record/2]).


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


db_table_info(Table,Param) ->
  db_table_info(?DBStorage,Table,Param).
db_table_info(ets,Table,Param) ->
  ets:info(Table,Param);
db_table_info(mnesia,Table,Param) ->
  mnesia:table_info(Table,Param).

read_db_pkt(Zone) -> %axfr
  read_db_pkt(?DBStorage,Zone).
read_db_pkt(ets,Zone) ->
  Pkt = ets:match(rpz_axfr_table,{{rpz,Zone#rpz.zone,Zone#rpz.serial,'_','_'},'$2'}),
  [binary_to_term(X) || [X] <- Pkt];
read_db_pkt(mnesia,_Zone) ->
  ok.

write_db_pkt(Zone, Pkt) ->
  write_db_pkt(?DBStorage, Zone, Pkt).
write_db_pkt(ets, Zone, {PktN,_ANCOUNT,_NSCOUNT,_ARCOUNT,_Records} = Pkt) ->
  ets:insert(rpz_axfr_table, {{rpz,Zone#rpz.zone,Zone#rpz.serial,PktN,self()}, term_to_binary(Pkt,[{compressed,?Compression}])});
write_db_pkt(mnesia, _Zone, _Pkt) ->
  ok.

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

clean_DB(RPZ) ->
  AXFR=get_allzones_info(ets,axfr),
  RPZn = [X#rpz.zone || X <- RPZ ],
  [{?logDebugMSG("Zone ~p removing from AXFR cache ~n",[Y]), delete_db_pkt(#rpz{zone=X,zone_str=Y,serial=42}),delete_old_db_record(#rpz{zone=X,zone_str=Y,serial=42})} || [X,Y|_] <- AXFR, lists:member(X, RPZn) ], %looks like was a bug -->>>> not lists:member
  IXFR=get_allzones_info(ets,ixfr),
  [{?logDebugMSG("Zone ~p removing from IXFR cache ~n",[Y]), delete_db_pkt(#rpz{zone=X,zone_str=Y,serial=42}),delete_old_db_record(#rpz{zone=X,zone_str=Y,serial=42})} || [X,Y|_] <- IXFR, lists:member(X, RPZn) ]. %looks like was a bug -->>>> not lists:member

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



saveZones() when ?SaveETS == true, ?DBStorage == ets ->
  [ save_zone_info(X) || [X] <- ets:match(cfg_table,{[rpz,'_'],'_','$4'}),  X#rpz.cache == <<"true">>],
  [[DBDir]] = ets:match(cfg_table,{db_dir,'$1'}),
  tab2file(?DBStorage,rpz_axfr_table,DBDir++"/ioc2rpz_axfr_table.db"),
  tab2file(?DBStorage,rpz_ixfr_table,DBDir++"/ioc2rpz_ixfr_table.db");

saveZones() -> ok.

loadZones() when ?SaveETS == true, ?DBStorage == ets ->
  [[DBDir]] = ets:match(cfg_table,{db_dir,'$1'}),
  loadZones(DBDir);

loadZones() -> ok.

loadZones(DBDir)  ->
  STA=file2tab(?DBStorage,DBDir++"/ioc2rpz_axfr_table.db"),
  STI=file2tab(?DBStorage,DBDir++"/ioc2rpz_ixfr_table.db"),
  [STA,STI].

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
