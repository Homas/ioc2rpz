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

%IOC2RPZ Supervisor
-module(ioc2rpz_sup).
-behaviour(supervisor).
-include_lib("kernel/include/file.hrl").
-include_lib("ioc2rpz.hrl").
-export([start_ioc2rpz_sup/1,stop_ioc2rpz_sup/0, start_socket/0,reload_config/0,update_all_zones/1,update_zone_full/1,
        update_zone_inc/1,reload_config3/0]).
-export([init/1]).

%-compile([export_all]).

start_ioc2rpz_sup([IP,IPv6,Filename,DBDir]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [IP,IPv6,Filename,DBDir]).

stop_ioc2rpz_sup() ->
  ioc2rpz_fun:logMessage("ioc2rpz recieved stop message ~n", []),
  ioc2rpz_fun:logMessage("saving DB ~n", []),
  ioc2rpz_db:saveZones(),
  ioc2rpz_fun:logMessage("ioc2rpz is terminating ~n", []),
  gen_server:stop(?MODULE).

init([IPStr,IPStr6, Filename, DBDir]) ->
  Pid=self(),
  ioc2rpz_fun:logMessage("ioc2rpz version: ~p, IP: ~s ~s, PID: ~p, Config: ~p ~n", [?ioc2rpz_ver,IPStr,IPStr6,Pid,Filename]),

  {ok, PidDB} = ioc2rpz_db_sup:start_db(),
  {ok, _} = ioc2rpz_db:init_db(?DBStorage,DBDir,PidDB),

  ets:insert_new(cfg_table, {cfg_file,Filename}), ets:insert_new(cfg_table, {db_dir,DBDir}),
  {ok,RPZ,_,_} = read_config3(Filename),
  %os:set_signal(sighup,handle),
  %os:set_signal(sigterm,handle),
  ioc2rpz_db:clean_DB(RPZ),
  inets:start(), ssl:start(),
%  spawn(ioc2rpz_sup,update_all_zones,[false]), %load zones to cache
%  spawn_opt(ioc2rpz_sup,update_all_zones,[false],[link,{fullsweep_after,0}]), %load zones to cache
  update_all_zones(false),
  timer:apply_interval(?ZoneRefTime,ioc2rpz_sup,update_all_zones,[false]),

  {ok, TCPSocket} = open_sockets(IPStr) ,
  {ok, TCPSocket6} = open_sockets6(IPStr6) ,

  spawn_opt(fun empty_listeners/0,[link,{fullsweep_after,0}]),
  ioc2rpz_fun:logMessage("ioc2rpz started ~n", []),

  {ok, {{simple_one_for_one, 60, 3600}, [{ioc2rpz, {ioc2rpz, start_ioc2rpz, [TCPSocket, [Pid]]}, temporary, 1000, worker, [ioc2rpz]} %simple_one_for_one
                                 %,{ioc2rpz_udp, {ioc2rpz_udp, start_ioc2rpz_udp, [IP, []]}, temporary, 1000, worker, [ioc2rpz_udp]}
                                 ]}}.



open_sockets(IPStr) when IPStr /= "", IPStr /= [] ->
  {ok,IP}=inet:parse_address(IPStr),
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{ip, IP},{active,once}, binary]),
  ioc2rpz_udp:start_ioc2rpz_udp(IPStr, [inet]), % TODO - put it in supervisor
  {ok, TCPSocket};

open_sockets(IPStr) ->
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{active,once}, binary]),
  ioc2rpz_udp:start_ioc2rpz_udp(IPStr, [inet]), % TODO - put it in supervisor
  {ok, TCPSocket}.
  
open_sockets6(IPStr) when IPStr /= "", IPStr /= [] ->
  {ok,IP}=inet:parse_address(IPStr),
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{ip, IP},{active,once}, binary, inet6]),
  ioc2rpz_udp:start_ioc2rpz_udp(IPStr, [inet6]), % TODO - put it in supervisor
  {ok, TCPSocket};

open_sockets6(IPStr) ->
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{active,once}, binary, inet6]),
  ioc2rpz_udp:start_ioc2rpz_udp(IPStr, [inet6]), % TODO - put it in supervisor
  {ok, TCPSocket}.
  

start_socket() ->
  supervisor:start_child(?MODULE, []).

empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,5)],
  ok.


%TODO task to clean hotcache ?HotCacheTime
%timer:apply_after(10000,io,format,["Hello timer 10 sec\n"]).

reload_config()->
  [[Filename]] = ets:match(cfg_table,{cfg_file,'$1'}),
  [[DBDir]] = ets:match(cfg_table,{db_dir,'$1'}),
  ioc2rpz_fun:logMessage("ioc2rpz reloading configuration from ~p ~n", [Filename]),
  [ ioc2rpz_db:save_zone_info(X) || [X] <- ets:match(cfg_table,{[rpz,'_'],'_','$4'}),  X#rpz.cache == <<"true">>],
  ets:delete_all_objects(cfg_table),
  ets:delete_all_objects(rpz_hotcache_table),
  ets:delete_all_objects(stat_table),
  ets:insert_new(cfg_table, {cfg_file,Filename}), ets:insert_new(cfg_table, {db_dir,DBDir}),
  {ok,RPZ,_,_} = read_config2(Filename),
  ioc2rpz_db:clean_DB(RPZ),
  update_all_zones(false),
  ok.

reload_config3()->
  [[Filename]] = ets:match(cfg_table,{cfg_file,'$1'}),
  ioc2rpz_fun:logMessage("ioc2rpz reloading configuration from ~p ~n", [Filename]),
  read_config3(Filename,reload),
  ok.

    
read_config2(Filename)  ->
  {ok,CFG} = file:consult(Filename),
  read_config2(CFG,[],[],[]).

read_config2([{srv,{Serv,Email,MKeys,ACL}}|REST],RPZs,Keys,_) ->
  {ok,ServB}=ioc2rpz:domstr_to_bin(list_to_binary(Serv),0),
  {ok,EmailB}=ioc2rpz:domstr_to_bin(list_to_binary(Email),0),
  MKeysX=[ioc2rpz:domstr_to_bin(list_to_binary(X),0)|| X <- MKeys], MKeysB=[X || {_,X} <- MKeysX],
  ets:insert_new(cfg_table, {srv,ServB,EmailB,MKeysB,ACL}),
  read_config2(REST,RPZs,Keys,#srv{server=ServB,email=EmailB,mkeys=MKeysB,acl=ACL});

read_config2([{key,{KName,Alg,Key}}|REST],RPZs,Keys,Srv) ->
  [KNameB] = ioc2rpz_fun:strs_to_binary([KName]),
  {ok,KeyDNSF}=ioc2rpz:domstr_to_bin(KNameB,0),
  KeyB=base64:decode(Key),
  ets:insert_new(cfg_table, {[key,KeyDNSF],KNameB,Alg,KeyB}),
  read_config2(REST,RPZs,[#key{name=KNameB,alg=Alg,key=KeyB}|Keys],Srv);

read_config2([{source,{Name,AXFR,IXFR,REGEX}}|REST],RPZs,Keys,Srv) ->
  ets:insert_new(cfg_table, {[source,Name],#source{name=Name,axfr_url=AXFR,ixfr_url=parse_ixfr_url(AXFR,IXFR),regex=REGEX}}), %TODO no IXFR for files
  read_config2(REST,RPZs,Keys,Srv);

read_config2([{whitelist,{Name,AXFR,REGEX}}|REST],RPZs,Keys,Srv) ->
  ets:insert_new(cfg_table, {[source,Name],#source{name=Name,axfr_url=AXFR,regex=REGEX}}),
  read_config2(REST,RPZs,Keys,Srv);

read_config2([{rpz,{Zone, Refresh, Retry, Expiration, Neg_ttl, Cache, Wildcards, Action, AKeys, IOCType, AXFR_Time, IXFR_Time, Sources, NotifyList, Whitelist}}|REST],RPZs,Keys,Srv) ->
  {ok,ZoneB} = ioc2rpz:domstr_to_bin(list_to_binary(Zone),0),
  AKeysX=[ioc2rpz:domstr_to_bin(list_to_binary(X),0)|| X <- AKeys], AKeysB=[X || {_,X} <- AKeysX],
  SOATimers = <<Refresh:32,Retry:32,Expiration:32,Neg_ttl:32>>,
  case {Cache,load_zone_info(#rpz{zone=ZoneB,axfr_time=AXFR_Time, zone_str=Zone,ixfr_time=AXFR_Time, cache=Cache})} of
    {"true",[ready = Status,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time, ready,_Serial,Serial_IXFR,IXFR_Update_time]} -> ok;
    {"true",[ready= Status,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time, notready| _ ]} -> IXFR_Update_time=0, Serial_IXFR=0;
    {"true",[notready = Status|_]} -> Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0;
    _ -> Status = notready, Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0
  end,
  ZAction = case Action of
   Action when Action=="nodata";Action=="passthru";Action=="drop";Action=="tcp-only";Action=="nxdomain";Action=="blockns" -> list_to_binary(Action);
   [{LAction,LData}] when LAction=="redirect_domain" -> {list_to_binary(LAction),list_to_binary(LData)};
   [{LAction,LData}] when LAction=="redirect_ip" -> {list_to_binary(LAction),ioc2rpz_fun:ip_to_bin(LData)};
   _ -> ioc2rpz_fun:read_local_actions(Action)
  end,
  RPZ = #rpz{zone=ZoneB, zone_str=Zone, soa_timers=SOATimers, cache=list_to_binary(Cache), wildcards=list_to_binary(Wildcards), action=ZAction, akeys=AKeysB, ioc_type=list_to_binary(IOCType), axfr_time=AXFR_Time, ixfr_time=IXFR_Time, sources=Sources, notifylist=NotifyList, whitelist=Whitelist, serial=Serial, status=Status, update_time=Update_time, ixfr_update_time=IXFR_Update_time, serial_ixfr=Serial_IXFR},
  ets:insert_new(cfg_table, {[rpz,ZoneB],ZoneB,RPZ}),
  read_config2(REST,[RPZ|RPZs],Keys,Srv);

read_config2([],RPZs,Keys,Srv)  ->
  {ok,RPZs,Keys,Srv}.


read_config3(Filename)  ->
  {ok,CFG} = file:consult(Filename),
  read_config3(CFG,startup,[],[],[],[],[]).

read_config3(Filename,reload)  ->
  {ok,CFG} = file:consult(Filename),
  read_config3(CFG,reload,[],[],[],[],[]).

read_config3([{srv,{Serv,Email,MKeys,ACL}}|REST],RType,_,Keys,WhiteLists,Sources,RPZ) ->
  {ok,ServB}=ioc2rpz:domstr_to_bin(list_to_binary(Serv),0),
  {ok,EmailB}=ioc2rpz:domstr_to_bin(list_to_binary(Email),0),
  MKeysX=[ioc2rpz:domstr_to_bin(list_to_binary(X),0)|| X <- MKeys], MKeysB=[X || {_,X} <- MKeysX],
  read_config3(REST,RType,#srv{server=ServB,email=EmailB,mkeys=MKeysB,acl=ACL},Keys,WhiteLists,Sources,RPZ);

read_config3([{key,{KName,Alg,Key}}|REST],RType,Srv,Keys,WhiteLists,Sources,RPZ) ->
  [KNameB] = ioc2rpz_fun:strs_to_binary([KName]),
  {ok,KeyDNSF}=ioc2rpz:domstr_to_bin(KNameB,0),
  KeyB=base64:decode(Key),
  read_config3(REST,RType,Srv,[#key{name=KNameB,alg=Alg,key=KeyB,name_bin=KeyDNSF}|Keys],WhiteLists,Sources,RPZ);

read_config3([{whitelist,{Name,AXFR,REGEX}}|REST],RType,Srv,Keys,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,[#source{name=Name,axfr_url=AXFR,regex=REGEX}|WhiteLists],Sources,RPZ);

read_config3([{source,{Name,AXFR,IXFR,REGEX}}|REST],RType,Srv,Keys,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,WhiteLists,[#source{name=Name,axfr_url=AXFR,ixfr_url=parse_ixfr_url(AXFR,IXFR),regex=REGEX}|Sources],RPZ);

read_config3([{rpz,{Zone, Refresh, Retry, Expiration, Neg_ttl, Cache, Wildcards, Action, AKeys, IOCType, AXFR_Time, IXFR_Time, Sources, NotifyList, Whitelist}}|REST],RType,Srv,Keys,WhiteLists,SourcesC,RPZ) ->
  {ok,ZoneB} = ioc2rpz:domstr_to_bin(list_to_binary(Zone),0),
  AKeysX=[ioc2rpz:domstr_to_bin(list_to_binary(X),0)|| X <- AKeys], AKeysB=[X || {_,X} <- AKeysX],
  SOATimers = <<Refresh:32,Retry:32,Expiration:32,Neg_ttl:32>>,
  case {Cache,load_zone_info(#rpz{zone=ZoneB,axfr_time=AXFR_Time, zone_str=Zone,ixfr_time=AXFR_Time, cache=Cache})} of
    {"true",[ready = Status,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time, ready,_Serial,Serial_IXFR,IXFR_Update_time]} -> ok;
    {"true",[ready= Status,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time, notready| _ ]} -> IXFR_Update_time=0, Serial_IXFR=0;
    {"true",[notready = Status|_]} -> Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0;
    _ -> Status = notready, Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0
  end,
  ZAction = case Action of
   Action when Action=="nodata";Action=="passthru";Action=="drop";Action=="tcp-only";Action=="nxdomain";Action=="blockns" -> list_to_binary(Action);
   [{LAction,LData}] when LAction=="redirect_domain" -> {list_to_binary(LAction),list_to_binary(LData)};
   [{LAction,LData}] when LAction=="redirect_ip" -> {list_to_binary(LAction),ioc2rpz_fun:ip_to_bin(LData)};
   _ -> ioc2rpz_fun:read_local_actions(Action)
  end,
  read_config3(REST,RType,Srv,Keys,WhiteLists,SourcesC,[#rpz{zone=ZoneB, zone_str=Zone, soa_timers=SOATimers, cache=list_to_binary(Cache), wildcards=list_to_binary(Wildcards), action=ZAction, akeys=AKeysB, ioc_type=list_to_binary(IOCType), axfr_time=AXFR_Time, ixfr_time=IXFR_Time, sources=Sources, notifylist=NotifyList, whitelist=Whitelist, serial=Serial, status=Status, update_time=Update_time, ixfr_update_time=IXFR_Update_time, serial_ixfr=Serial_IXFR}|RPZ]);

read_config3([],startup,Srv,Keys,WhiteLists,Sources,RPZ)  ->
  [ ets:insert_new(cfg_table, {[key,X#key.name_bin],X#key.name,X#key.alg,X#key.key}) || X <- [ validateCFGKeys(Y) || Y <- Keys ] ],
  SrvV = validateCFGSrv(Srv), ets:insert_new(cfg_table, {srv,SrvV#srv.server,SrvV#srv.email,SrvV#srv.mkeys,SrvV#srv.acl}),
  WhiteLists_V=[ X || X <- [ validateCFGWL(Y) || Y <- WhiteLists ] ],
  [ ets:insert_new(cfg_table, {[source,X#source.name],X}) || X <- WhiteLists_V ],
  Sources_V=[ X || X <- [ validateCFGSrc(Y) || Y <- Sources ] ],
  [ ets:insert_new(cfg_table, {[source,X#source.name],X}) || X <- Sources_V ],
  [ ets:insert_new(cfg_table, {[rpz,X#rpz.zone],X#rpz.zone,X}) || X <- [ validateCFGRPZ(Y,Sources_V,WhiteLists_V) || Y <- RPZ ] ],
  {ok,RPZ,Keys,Srv};

read_config3([],reload,Srv,Keys,WhiteLists,Sources,RPZ)  ->
  RPZ_C=[ X || [X] <- ets:match(cfg_table, {[rpz,'_'],'_','$3'})],
  [ ets:update_element(cfg_table, [rpz,X#rpz.zone], [{3, X#rpz{serial_new=-1, status=updating, update_time=-1}}]) || X <- RPZ_C ],

  Keys_C=ets:match(cfg_table, {[key,'$1'],'$2','$3','$4'}),
  Keys_V=[ validateCFGKeys(Y) || Y <- Keys ],
  [ ets:delete(cfg_table, [key,X]) || [X,Y,_,_] <- Keys_C, not lists:member(Y, [ Z#key.name || Z <- Keys_V ]) ],
  [ ets:insert(cfg_table, {[key,X#key.name_bin],X#key.name,X#key.alg,X#key.key}) || X <- Keys_V ],
  SrvV = validateCFGSrv(Srv), ets:insert(cfg_table, {srv,SrvV#srv.server,SrvV#srv.email,SrvV#srv.mkeys,SrvV#srv.acl}),
  SW=ets:match(cfg_table, {[source,'_'],'$2'}),
  WhiteLists_C=[X||[X] <- SW,X#source.ixfr_url == undefined ],
  Sources_C=[X||[X] <- SW,X#source.ixfr_url /= undefined ],
  WhiteLists_V=[ validateCFGWL(Y) || Y <- WhiteLists ],
  Sources_V=[ validateCFGSrc(Y) || Y <- Sources ],
  WhiteLists_D = [ X || X <- WhiteLists_C, not lists:member(X#source.name, [ Z#source.name || Z <- WhiteLists_V ]) ],
  Sources_D = [ X || X <- Sources_C, not lists:member(X#source.name, [ Z#source.name || Z <- Sources_V ]) ],

  WhiteLists_N = [ X || X <- WhiteLists_V, not lists:member(X#source.name, [ Z#source.name || Z <- WhiteLists_C ]) ],
  Sources_N = [ X || X <- Sources_V, not lists:member(X#source.name, [ Z#source.name || Z <- Sources_C ]) ],

  WhiteLists_UPD = [ X || X <- WhiteLists_V, X /= lists:keyfind(X#source.name,2,WhiteLists_C),lists:member(X#source.name, [ Z#source.name || Z <- WhiteLists_C ]) ],
  Sources_UPD = [ X || X <- Sources_V, X /= lists:keyfind(X#source.name,2,Sources_C),lists:member(X#source.name, [ Z#source.name || Z <- Sources_C ]) ],

  [ ets:insert(cfg_table, {[source,X#source.name],X}) || X <- WhiteLists_V ++ Sources_V ],
  [ ets:delete(cfg_table, [source,X#source.name]) || X <- WhiteLists_D ++ Sources_D ],
  [ ets:delete(rpz_hotcache_table, {X#source.name,Y}) || X <- WhiteLists_UPD ++ Sources_UPD ++ WhiteLists_D ++ Sources_D, Y <- [axfr,ixfr] ],

  RPZ_C_UPD=[ X || X <- RPZ_C, X#rpz.status == updating], %TODO stop update processes
  
  RPZ_V= [ Z || Z <- [ validateCFGRPZ(Y,Sources_V,WhiteLists_V) || Y <- RPZ ], Z /= [] ],
  RPZ_D = [ X || X <- RPZ_C, not lists:member(X#rpz.zone, [ Z#rpz.zone || Z <- RPZ_V ]) ],
  RPZ_N = [ X || X <- RPZ_V, not lists:member(X#rpz.zone, [ Z#rpz.zone || Z <- RPZ_C ]) ],

  RPZ_UPD = [ X || X <- RPZ_V, not checkRPZEq(X,lists:keyfind(X#rpz.zone,3,RPZ_C)),lists:member(X#rpz.zone, [ Z#rpz.zone || Z <- RPZ_C ]) ] ++ 
            [ X || X <- RPZ_V, ioc2rpz_fun:intersection(X#rpz.whitelist,[Z#source.name || Z <- WhiteLists_UPD]) /= [] ] ++
            [ X || X <- RPZ_V, ioc2rpz_fun:intersection(X#rpz.sources,[Z#source.name || Z <- Sources_UPD]) /= [] ],
  [ ets:delete(cfg_table, [rpz,X#rpz.zone]) || X <- RPZ_D ],
  [ ets:insert(cfg_table, {[rpz,X#rpz.zone],X#rpz.zone,X}) || X <- RPZ_V ],
  [ ets:match_delete(rpz_hotcache_table,{{pkthotcache,X#rpz.zone,'_'},'_','_'}) || X <- RPZ_D ++ RPZ_UPD ],
  ioc2rpz_db:clean_DB(RPZ_D ++ RPZ_UPD), %TODO check
  [ ets:update_element(cfg_table, [rpz,X#rpz.zone], [{3, X#rpz{status=notready}}]) || X <- RPZ_UPD ],

  [ ioc2rpz_fun:logMessage("Whitelist ~p was added.~n",[X#source.name]) || X <- WhiteLists_N ],
  [ ioc2rpz_fun:logMessage("Whitelist ~p was updated.~n",[X#source.name]) || X <- WhiteLists_UPD ],
  [ ioc2rpz_fun:logMessage("Whitelist ~p was removed.~n",[X#source.name]) || X <- WhiteLists_D ],

  [ ioc2rpz_fun:logMessage("Source ~p was added.~n",[X#source.name]) || X <- Sources_N ],
  [ ioc2rpz_fun:logMessage("Source ~p was updated.~n",[X#source.name]) || X <- Sources_UPD ],
  [ ioc2rpz_fun:logMessage("Source ~p was removed.~n",[X#source.name]) || X <- Sources_D ],

  [ ioc2rpz_fun:logMessage("Zone ~p was added.~n",[X#rpz.zone_str]) || X <- RPZ_N ],
  [ ioc2rpz_fun:logMessage("Zone ~p was updated.~n",[X#rpz.zone_str]) || X <- RPZ_UPD ],
  [ ioc2rpz_fun:logMessage("Zone ~p was removed.~n",[X#rpz.zone_str]) || X <- RPZ_D ],

  update_all_zones(false),
  ok.

checkRPZEq(R1,R2) when R1#rpz.zone == R2#rpz.zone,R1#rpz.soa_timers == R2#rpz.soa_timers,R1#rpz.cache == R2#rpz.cache,R1#rpz.wildcards == R2#rpz.wildcards,R1#rpz.action == R2#rpz.action,R1#rpz.ioc_type == R2#rpz.ioc_type,R1#rpz.sources == R2#rpz.sources,R1#rpz.whitelist == R2#rpz.whitelist ->
  true;

checkRPZEq(R1,R2) ->
  false.

validateCFGKeys(Keys) -> %Check if key is good
  Keys.

validateCFGSrv(Srv) -> %Check: MGMT Keys, ACL and email
  Srv.

validateCFGWL(WL) -> %Check: RegEx and AXFR URL availability. If URL is not available - log it and accept
  WL.

validateCFGSrc(Src) -> %Check: RegEx and URLs availability. If URL is not available - log it and accept
  Src.

validateCFGRPZ(RPZ,S,W) -> %Check: Sources, Whitelists
  SV = not lists:member(false, [ lists:member(X, [ Z#source.name || Z <- S ])  || X <- RPZ#rpz.sources  ]),
  WV = not lists:member(false, [ lists:member(X, [ Z#source.name || Z <- W ])  || X <- RPZ#rpz.whitelist ]),
  if not SV -> % or not WV 
    ioc2rpz_fun:logMessage("RPZ ~p was not loaded. No sources. Sources ~p Whitelists ~p.~n",[RPZ#rpz.zone_str,SV,WV]),
    [];
    true -> RPZ
  end.


parse_ixfr_url(AXFR,IXFR) ->
  [ if X == "[:AXFR:]" -> AXFR; true -> X end || X <- re:split(IXFR,"(\\[:[^:]+:\\])",[{return,list},trim]), X /=[]].

load_zone_info(Zone) ->
  load_axfr_zone_info(Zone) ++ load_ixfr_zone_info(Zone).

load_axfr_zone_info(Zone) ->
  load_axfr_zone_info(?DBStorage,Zone).

load_axfr_zone_info(ets,Zone) ->
  CTime=ioc2rpz_fun:curr_serial(),%erlang:system_time(seconds),
  case ioc2rpz_db:get_zone_info(Zone,axfr) of %ets:match(rpz_axfr_table,{{axfr_rpz_cfg,Zone#rpz.zone},'$1','$2','$3','$4','$5','$6','$7'})
    [[_,Serial,Soa_timers,Cache,Wildcards,Sources,Ioc_md5,Update_time]] when (Update_time+Zone#rpz.axfr_time)>CTime ->
      ioc2rpz_fun:logMessage("Get AXFR zone ~p serial ~p status ready. Last update ~p ~n",[Zone#rpz.zone_str,Serial,Update_time]),
      [ready,Serial,Soa_timers,Cache,Wildcards,Sources,Ioc_md5,Update_time];
    [[_,Serial,Soa_timers,Cache,Wildcards,Sources,Ioc_md5,Update_time]] when Zone#rpz.cache == "true" ->
      ioc2rpz_fun:logMessage("Get AXFR zone ~p serial ~p status notready ~n",[Zone#rpz.zone_str,Serial]),
      [notready,Serial,Soa_timers,Cache,Wildcards,Sources,Ioc_md5,Update_time];
    _NonCache when Zone#rpz.cache == "false" ->
      ioc2rpz_fun:logMessage("Zone ~p is non cacheable ~n",[Zone#rpz.zone_str]),
      [];
    _Else ->
      ioc2rpz_fun:logMessage("Get AXFR zone ~p serial 0 status notready cache ~p ~n",[Zone#rpz.zone_str, Zone#rpz.cache]),
      []
  end;
load_axfr_zone_info(mnesia,_Zone) ->
  ok.

load_ixfr_zone_info(Zone) ->
  load_ixfr_zone_info(?DBStorage,Zone).

load_ixfr_zone_info(ets,Zone) ->
  CTime=ioc2rpz_fun:curr_serial(), %erlang:system_time(seconds),
  case ioc2rpz_db:get_zone_info(Zone,ixfr) of %ets:match(rpz_ixfr_table,{{ixfr_rpz_cfg,Zone#rpz.zone},'$1','$2','$3'})
    [[_,Serial,Serial_IXFR,IXFR_Update_time]] when (IXFR_Update_time+Zone#rpz.ixfr_time)>CTime ->
      ioc2rpz_fun:logMessage("Get IXFR zone ~p serial ~p status ready ~n",[Zone#rpz.zone_str,Serial_IXFR]),
      [ready,Serial,Serial_IXFR,IXFR_Update_time];
    [[_,Serial,Serial_IXFR,IXFR_Update_time]] when Zone#rpz.cache == "true"  ->
      ioc2rpz_fun:logMessage("Get IXFR zone ~p serial ~p status notready ~n",[Zone#rpz.zone_str,Serial_IXFR]),
      [notready,Serial,Serial_IXFR,IXFR_Update_time];
    _NonCache when Zone#rpz.cache == "false" ->
      [];
    _Else ->
      ioc2rpz_fun:logMessage("Get IXFR zone ~p serial 0 status notready ~n",[Zone#rpz.zone_str]),
      []
  end;
load_ixfr_zone_info(mnesia,_Zone) ->
  ok.


update_all_zones(true) -> %force update all zones
  AllRPZ = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
  [ spawn_opt(ioc2rpz_sup,update_zone_full,[X],[{fullsweep_after,0}]) || [X] <- AllRPZ,  X#rpz.cache == <<"true">>];
update_all_zones(false) -> %update expired zones
  CTime=ioc2rpz_fun:curr_serial(),%erlang:system_time(seconds),
  AllRPZ = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
%  [io:fwrite(group_leader(),"Zone ~p serial ~p full refresh time ~p cache ~p status ~p ~n",[X#rpz.zone_str,X#rpz.update_time, X#rpz.axfr_time, X#rpz.cache, X#rpz.status]) || [X] <- AllRPZ, X#rpz.cache == <<"true">>],
  [ spawn_opt(ioc2rpz_sup,update_zone_full,[X],[{fullsweep_after,0}]) || [X] <- AllRPZ,(X#rpz.update_time + X#rpz.axfr_time) < CTime,  X#rpz.cache == <<"true">>, X#rpz.status /= updating ],
  [ioc2rpz_fun:logMessage("Start from full update Zone ~p serial ~p full refresh time ~p, Ctime ~p cache ~p status ~p ~n",[X#rpz.zone_str,X#rpz.ixfr_update_time, X#rpz.ixfr_time,CTime, X#rpz.cache, X#rpz.status]) || [X] <- AllRPZ, (X#rpz.update_time + X#rpz.axfr_time) > CTime, (X#rpz.ixfr_update_time + X#rpz.ixfr_time) < CTime,  X#rpz.cache == <<"true">>, X#rpz.status /= updating, X#rpz.ixfr_time /= 0],
  [ spawn_opt(ioc2rpz_sup,update_zone_inc,[X],[{fullsweep_after,0}]) || [X] <- AllRPZ,(X#rpz.update_time + X#rpz.axfr_time) > CTime, (X#rpz.ixfr_update_time + X#rpz.ixfr_time) < CTime,  X#rpz.cache == <<"true">>, X#rpz.status /= updating, X#rpz.ixfr_time /= 0 ].


update_zone_full(Zone) ->
  Pid=self(),
  CTime=ioc2rpz_fun:curr_serial_60(),%CTime=erlang:system_time(seconds),
  ioc2rpz_fun:logMessage("Zone ~p serial ~p, refresh time ~p current status ~p ~n",[Zone#rpz.zone_str,Zone#rpz.serial, Zone#rpz.axfr_time, Zone#rpz.status]),
  [[NSServ,MailAddr,_,_]] = ets:match(cfg_table,{srv,'$2','$3','$4','$5'}),
  SOA = <<NSServ/binary,MailAddr/binary,(ioc2rpz_fun:curr_serial()):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOA)):16, SOA/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  ioc2rpz_fun:logMessage("Updating zone ~p full ~n",[Zone#rpz.zone_str]),
  ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{serial_new=CTime, status=updating, update_time=CTime, pid=Pid}}]),
  {Status,MD5} = ioc2rpz:send_zone_live(<<>>,cache,Zone#rpz{serial=CTime},<<>>,<<(Zone#rpz.zone)/binary,0:32>>, SOAREC,NSRec,[]),
  if Status == updateSOA ->
      ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=ready, serial_new=0, ioc_md5=MD5, update_time=CTime, ixfr_update_time=CTime, pid=undefined}}]),
      ioc2rpz_fun:logMessage("Zone ~p is the same. Checked in ~p seconds, check timestamp ~p ~n",[Zone#rpz.zone_str, (ioc2rpz_fun:curr_serial()- CTime), CTime]);
    true ->
      %if Zone#rpz.serial_ixfr == 0 -> Serial_IXFR=CTime; true -> Serial_IXFR=Zone#rpz.serial_ixfr end,
      ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{serial=CTime, status=ready, serial_new=0, ioc_md5=MD5, update_time=CTime, ixfr_update_time=CTime, serial_ixfr=CTime, pid=undefined}}]),
      ioc2rpz_db:delete_db_pkt(Zone),
      %erlang:garbage_collect(), %TODO check if need
      ioc2rpz:send_notify(Zone),
      ioc2rpz_fun:logMessage("Zone ~p updated in ~p seconds, new serial ~p ~n",[Zone#rpz.zone_str, (ioc2rpz_fun:curr_serial_60() - CTime), CTime])
  end,
  ioc2rpz_db:saveZones(),
  ok.


update_all_zones_inc(true) -> %force inc update all zones
  AllRPZ = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
  [ spawn(ioc2rpz_sup,update_zone_inc,[X]) || [X] <- AllRPZ,  X#rpz.cache == <<"true">>];
update_all_zones_inc(false) -> %update inc expired zones
  CTime=ioc2rpz_fun:curr_serial(),%erlang:system_time(seconds),
  AllRPZ = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
  [io:fwrite(group_leader(),"Zone ~p serial ~p full refresh time ~p cache ~p status ~p ~n",[X#rpz.zone_str,X#rpz.ixfr_update_time, X#rpz.ixfr_time, X#rpz.cache, X#rpz.status]) || [X] <- AllRPZ, (X#rpz.ixfr_update_time + X#rpz.ixfr_time) < CTime,  X#rpz.cache == <<"true">>, X#rpz.status /= updating, X#rpz.ixfr_time /= 0],
  [ spawn(ioc2rpz_sup,update_zone_inc,[X]) || [X] <- AllRPZ,(X#rpz.ixfr_update_time + X#rpz.ixfr_time) < CTime,  X#rpz.cache == <<"true">>, X#rpz.status /= updating, X#rpz.ixfr_time /= 0 ].

update_zone_inc(Zone) ->
  %io:fwrite(group_leader(),"Zone ~p IOC  ~p ~n",[Zone#rpz.zone_str,IOC]),
  Pid=self(),
  NRbefore=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[],['true']}]),
  CTime=ioc2rpz_fun:curr_serial_60(), %erlang:system_time(seconds),
  ioc2rpz_fun:logMessage("Updating zone ~p inc. Last IXFR update ~p seconds ago ~n",[Zone#rpz.zone_str,(CTime - Zone#rpz.ixfr_update_time)]),
  ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=updating, ixfr_update_time=CTime, pid=Pid}}]),
  case {ioc2rpz:mrpz_from_ioc(Zone#rpz{serial=CTime},ixfr),ioc2rpz_db:read_db_record(Zone,CTime,updated)} of
    {[],[]} -> % No new records, no expired records
      ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=ready, ixfr_update_time=CTime, pid=undefined}}]); %, ixfr_update_time=CTime
    {IOC,_} ->  %TODO double check that we really have an update. It looks like We have full file and TIDE send the same response.
      ioc2rpz_db:write_db_record(Zone#rpz{serial=CTime},IOC,ixfr),
      case ioc2rpz_db:read_db_record(Zone,CTime,updated) of % New IOC were added or expired
        [] -> ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=ready, ixfr_update_time=CTime, pid=undefined}}]); %, ixfr_update_time=CTime
        _NewIOC ->
          rebuild_axfr_zone(Zone#rpz{serial=CTime}),
          NRafter=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','$2'},'$3'},[],['true']}]),
          ioc2rpz_fun:logMessage("Zone ~p records before ~p after ~p. ~n",[Zone#rpz.zone_str, NRbefore, NRafter]),
          ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=ready, serial=CTime, ixfr_update_time=CTime, pid=undefined}}]),
          ioc2rpz_db:delete_db_pkt(Zone),
          ioc2rpz_db:saveZones(),
          ioc2rpz:send_notify(Zone)
      end
  end.

rebuild_axfr_zone(Zone) ->
  IOCs = ioc2rpz_db:read_db_record(Zone,0,active),
  IOC = [{X,Exp} || [X,_,Exp] <- IOCs],
  [[NSServ,MailAddr,_,_]] = ets:match(cfg_table,{srv,'$2','$3','$4','$5'}),
  SOA = <<NSServ/binary,MailAddr/binary,(ioc2rpz_fun:curr_serial()):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOA)):16, SOA/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  {ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})$"),
  Questions = <<(Zone#rpz.zone)/binary,0:32>>,
  PktHLen = 12+byte_size(Questions),
  T_ZIP_L=ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]), % нужны ли {read_concurrency, true}, {write_concurrency, true} ???
  ioc2rpz:send_packets(<<>>,IOC, [], 0, 0, true, <<>>, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,[],0,cache,0,false),
  ets:delete(T_ZIP_L),
  ok.
