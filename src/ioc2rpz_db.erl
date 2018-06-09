%Copyright 2017-2018 Vadim Pavlov ioc2rpz[at]gmail[.]com
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
        get_zone_info/2,clean_DB/1,save_zone_info/1]).


init_db(ets,DBDir,PID) ->
  [{STA,_},{STI,_}]=loadZones(DBDir),
  if STA /= ok ->
    ets:new(rpz_axfr_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]); %because labels are shortened
    true -> ets:give_away(rpz_axfr_table, PID, [])
  end,
  if STI /= ok ->
    ets:new(rpz_ixfr_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, set, public, named_table]);
    true -> ets:give_away(rpz_ixfr_table, PID, [])
  end,
  ets:new(cfg_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]),
  ets:new(rpz_hotcache_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]), %because labels are shortened
  {ok,[]};

init_db(mnesia,DBDir,PID) ->
%TODO
%init schema
%create tables
  ets:new(cfg_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]),
  ets:new(rpz_hotcache_table, [{heir,PID,[]}, {read_concurrency, true}, {write_concurrency, true}, ordered_set, public, named_table]),
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
  Pkt = ets:match(rpz_axfr_table,{{rpz,Zone#rpz.zone,Zone#rpz.serial,'_'},'$2'}),
  [binary_to_term(X) || [X] <- Pkt];
read_db_pkt(mnesia,Zone) ->
  ok.

write_db_pkt(Zone, Pkt) ->
  write_db_pkt(?DBStorage, Zone, Pkt).
write_db_pkt(ets, Zone, {PktN,_ANCOUNT,_NSCOUNT,_ARCOUNT,_Records} = Pkt) ->
  ets:insert(rpz_axfr_table, {{rpz,Zone#rpz.zone,Zone#rpz.serial,PktN}, term_to_binary(Pkt,[{compressed,?Compression}])});
write_db_pkt(mnesia, Zone, Pkt) ->
  ok.

delete_db_pkt(Zone) -> %axfr
  delete_db_pkt(?DBStorage,Zone).

delete_db_pkt(ets,Zone) when Zone#rpz.serial == 42 ->
  ioc2rpz_fun:logMessage("Removing AXFR zone ~p ~n",[Zone#rpz.zone_str]),
  ets:match_delete(rpz_axfr_table,{{rpz,Zone#rpz.zone,'_','_'},'_'}),
  ets:match_delete(rpz_axfr_table,{{axfr_rpz_cfg,Zone#rpz.zone},'_','_','_','_','_','_','_','_'});

delete_db_pkt(ets,Zone) ->
  %axfr_rpz_cfg
  ets:select_delete(rpz_axfr_table,[{{{rpz,Zone#rpz.zone,Zone#rpz.serial,'$1'},'_'},[{'=<','$1',Zone#rpz.serial}],[true]}]);

delete_db_pkt(mnesia,Zone) ->
  ok.

read_db_record(Zone,Serial,Type) -> %ixfr
  read_db_record(?DBStorage,Zone,Serial,Type).
read_db_record(ets,Zone,Serial,all) ->
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[{'>','$3',Serial},{'=<','$3',Zone#rpz.serial}],['$$']},{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[{'>','$2',Serial},{'=<','$2',Zone#rpz.serial}],['$$']}]);

read_db_record(ets,Zone,Serial,updated) ->
%  io:fwrite(group_leader(),"Read updated records. Zone ~p Serial ~p ~n",[Zone,Serial]),
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[{'=<','$3',Serial},{'>=','$3',Zone#rpz.serial}],['$$']},{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[{'=<','$2',Serial},{'>','$2',Zone#rpz.serial}],['$$']}]);


read_db_record(ets,Zone,Serial,new) ->
%  io:fwrite(group_leader(),"Read expired records. Zone ~p Serial ~p ~n",[Zone,Serial]),
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[{'>','$2',Serial},{'>','$3',Zone#rpz.serial}],['$$']},{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[{'==','$3',0},{'>','$2',Serial}],['$$']}]);

read_db_record(ets,Zone,Serial,expired) ->
%  io:fwrite(group_leader(),"Read expired records. Zone ~p Serial ~p ~n",[Zone,Serial]),
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[{'=<','$2',Serial},{'>=','$3',Serial},{'=<','$3',Zone#rpz.serial}],['$$']}]);


read_db_record(ets,Zone,_Serial,active) -> %All not expired
  ets:select(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[{'>','$3',Zone#rpz.serial},{'>=','$2',Zone#rpz.serial_ixfr}],['$$']},{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[{'==','$3',0},{'>=','$2',Zone#rpz.serial_ixfr}],['$$']}]);

read_db_record(mnesia,Zone,Serial,all) -> ok;
read_db_record(mnesia,Zone,Serial,updated) -> ok;
read_db_record(mnesia,Zone,Serial,new) -> ok;
read_db_record(mnesia,Zone,Serial,expired) -> ok;
read_db_record(mnesia,Zone,Serial,active) -> ok.


write_db_record(Zone,IOC,XFR) when Zone#rpz.cache == <<"true">> -> %, Zone#rpz.ixfr_update_time/=0 -> %TODO check why was checked here?
  write_db_record(?DBStorage,Zone,IOC,XFR);
write_db_record(Zone,IOC,XFR) ->
  ok.

write_db_record(ets,Zone,IOCs,axfr) ->
  %TODO интеллектуальное обновление записи:
  %запись уже заэкспайрилась - обновить serial и expiration time (потому что могли уже кому-то передать удаление записи) возможно в ключ нужно будет добавить версионность
  %запись ежё действует - обновить expiration time
  %добавить Serial в ключ?  AXFR время обновления - полностью удаляет IXFR таблицу?
  %Типа AXFR - полная ресинхронизация, которая делается редко, IXFR делается часто
  [ets:insert_new(rpz_ixfr_table, {{ioc,Zone#rpz.zone,IOC,Zone#rpz.serial},IOCExp}) || {IOC,IOCExp} <- IOCs];

write_db_record(mnesia,Zone,{IOC,IOCExp},axfr) -> ok;

write_db_record(?DBStorage,Zone,IOCs,ixfr) ->
  CTime=erlang:system_time(seconds),
  [update_db_record(?DBStorage,Zone#rpz.zone,Zone#rpz.serial,IOC,IOCExp,CTime) || {IOC,IOCExp} <- IOCs];

write_db_record(_DBStorage,_Zone,_IOCs,_XFR) -> ok. %non cached zones

update_db_record(ets, Zone, Serial, IOC, IOCExp, CTime) ->
  case ets:match(rpz_ixfr_table, {{ioc,Zone,IOC,'$1'},'$2'}) of
   [[OSerial,ExpTime]] when ExpTime == IOCExp -> ok; %Do not update the record
   [[OSerial,ExpTime]] when ExpTime < IOCExp, IOCExp >= CTime  -> ets:update_element(rpz_ixfr_table,{ioc,Zone,IOC,OSerial},{2,IOCExp}); %update expiration
   [[OSerial,ExpTime]] -> ets:delete(rpz_ixfr_table,{ioc,Zone,IOC,OSerial}),ets:insert_new(rpz_ixfr_table, {{ioc,Zone,IOC,Serial},IOCExp}); %update serial and expiration
    _Else -> ets:insert_new(rpz_ixfr_table, {{ioc,Zone,IOC,Serial},IOCExp})
  end,
  ok;
update_db_record(mnesia, Zone, Serial, IOC, IOCExp, CTime) -> ok.

delete_old_db_record(Zone) ->
  delete_old_db_record(?DBStorage,Zone).


delete_old_db_record(ets, Zone) when Zone#rpz.serial == 42 ->
  ioc2rpz_fun:logMessage("Removing IXFR zone ~p ~n",[Zone#rpz.zone_str]),
  ets:match_delete(rpz_ixfr_table,{{ioc,Zone#rpz.zone,'_','_'},'_'}),
  ets:match_delete(rpz_ixfr_table,{{ixfr_rpz_cfg,Zone#rpz.zone},'_','_','_','_'});

delete_old_db_record(ets, Zone) ->
  NRbefore=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[],['true']}]),
  ets:select_delete(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'_','$1'},'_'},[{'<','$1',Zone#rpz.serial}],[true]}]),
  NRafter=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[],['true']}]),
  if NRbefore /= NRafter -> ioc2rpz_fun:logMessage("Delete old records from zone ~p.  before ~p after ~p ~n",[Zone#rpz.zone_str, NRbefore, NRafter]); true -> ok end;
delete_old_db_record(mnesia, Zone) ->
ok.

clean_DB(RPZ) ->
  AXFR=get_allzones_info(ets,axfr),
  RPZn = [X#rpz.zone || X <- RPZ ],
  [{ioc2rpz_fun:logMessage("Zone ~p removing from cache ~n",[Y]), delete_db_pkt(#rpz{zone=X,zone_str=Y,serial=42}),delete_old_db_record(#rpz{zone=X,zone_str=Y,serial=42})} || [X,Y|_] <- AXFR, not lists:member(X, RPZn) ],
  IXFR=get_allzones_info(ets,ixfr),
  [{ioc2rpz_fun:logMessage("Zone ~p removing from cache ~n",[Y]), delete_db_pkt(#rpz{zone=X,zone_str=Y,serial=42}),delete_old_db_record(#rpz{zone=X,zone_str=Y,serial=42})} || [X,Y|_] <- IXFR, not lists:member(X, RPZn) ].

get_zone_info(Zone,DB) ->
  get_zone_info(?DBStorage,Zone,DB).

get_zone_info(ets,Zone,axfr) ->
  ets:match(rpz_axfr_table,{{axfr_rpz_cfg,Zone#rpz.zone},'$0','$1','$2','$3','$4','$5','$6','$7'});
get_zone_info(ets,Zone,ixfr) ->
  ets:match(rpz_ixfr_table,{{ixfr_rpz_cfg,Zone#rpz.zone},'$0','$1','$2','$3'});
get_zone_info(mnesia,_Zone,axfr) ->
  ok;
get_zone_info(mnesia,_Zone,ixfr) ->
  ok.

get_allzones_info(DB) ->
  get_allzones_info(?DBStorage,DB).

get_allzones_info(ets,axfr) ->
  ets:match(rpz_axfr_table,{{axfr_rpz_cfg,'$0'},'$1','$2','$3','$4','$5','$6','$7','$8'});
get_allzones_info(ets,ixfr) ->
  ets:match(rpz_ixfr_table,{{ixfr_rpz_cfg,'$0'},'$1','$2','$3','$4'});
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
  ets:insert(rpz_axfr_table, {{axfr_rpz_cfg,Zone#rpz.zone},Zone#rpz.zone_str,Zone#rpz.serial,Zone#rpz.soa_timers, Zone#rpz.cache, Zone#rpz.wildcards, Zone#rpz.sources, Zone#rpz.ioc_md5, Zone#rpz.update_time});
save_axfr_zone_info(mnesia,Zone) ->
  ok.

save_ixfr_zone_info(Zone) ->
  save_ixfr_zone_info(?DBStorage,Zone).
save_ixfr_zone_info(ets,Zone) ->
  ets:insert(rpz_ixfr_table, {{ixfr_rpz_cfg,Zone#rpz.zone},Zone#rpz.zone_str,Zone#rpz.serial,Zone#rpz.serial_ixfr,Zone#rpz.ixfr_update_time});
save_ixfr_zone_info(mnesia,Zone) ->
  ok.

tab2file(ets,Tbl_Name,File_Name) ->
  ets:tab2file(Tbl_Name,File_Name,[{extended_info,[object_count,md5sum]},{sync,true}]);
tab2file(_DBStorage,_Tbl_Name,_File_Name) -> ok.

file2tab(ets,File_Name) ->
  ets:file2tab(File_Name,[{verify,true}]);
file2tab(_DBStorage,_File_Name) -> ok.
