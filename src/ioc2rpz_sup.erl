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

%% @doc IOC2RPZ top-level supervisor.
%%
%% Manages the OTP supervision tree for the ioc2rpz DNS RPZ server.
%% Responsible for:
%% <ul>
%%   <li>Initialising the ETS configuration tables and database</li>
%%   <li>Parsing the configuration file ({@link read_config3/1})</li>
%%   <li>Starting child supervisors for TCP, TLS (DoT), UDP, REST, and DoH listeners</li>
%%   <li>Scheduling periodic zone updates and hot-source reloads</li>
%%   <li>Providing configuration reload without full restart ({@link reload_config3/1})</li>
%% </ul>
%% @end
-module(ioc2rpz_sup).
-behaviour(supervisor).
-include_lib("kernel/include/file.hrl").
-include_lib("ioc2rpz.hrl").
-export([start_ioc2rpz_sup/1,stop_ioc2rpz_sup/0,update_all_zones/1,update_zone_full/1,
        update_zone_inc/1,reload_config3/1,read_config3/1,load_hotsources/1]).
-export([init/1]).

%-compile([export_all]).

%% @doc Start the top-level supervisor as a locally registered process.
%%
%% @param IP      IPv4 address string for listeners.
%% @param IPv6    IPv6 address string for listeners.
%% @param Filename  Path to the ioc2rpz configuration file.
%% @param DBDir   Directory used for persistent zone database storage.
%% @returns `{ok, Pid}' on success.
start_ioc2rpz_sup([IP,IPv6,Filename,DBDir]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [IP,IPv6,Filename,DBDir]).

%% @doc Gracefully stop the supervisor.
%%
%% Persists all cached zone data to disk before terminating.
stop_ioc2rpz_sup() ->
  ioc2rpz_fun:logMessage("ioc2rpz recieved stop message ~n", []),
  ioc2rpz_fun:logMessage("saving DB ~n", []),
  ioc2rpz_db:saveZones(),
  ioc2rpz_fun:logMessage("ioc2rpz is terminating ~n", []).
%  gen_server:stop(?MODULE).

%% @doc Supervisor `init/1' callback.
%%
%% Performs full server bootstrap:
%% <ol>
%%   <li>Starts the database supervisor and initialises ETS/mnesia storage</li>
%%   <li>Creates the `cfg_table' and `rate_limits' ETS tables</li>
%%   <li>Parses the configuration file via {@link read_config3/1}</li>
%%   <li>Loads hot-sources and triggers initial zone updates</li>
%%   <li>Sets up periodic timers for zone refresh and hot-source reload</li>
%%   <li>Builds the child supervisor specs for TCP, UDP, TLS (DoT), and REST/DoH</li>
%% </ol>
%%
%% Child supervisor setup:
%% <ul>
%%   <li>`ioc2rpz_tcp_sup_v6' — DNS over TCP (port 53)</li>
%%   <li>`ioc2rpz_udp_sup_v6' — DNS over UDP (port 53)</li>
%%   <li>`ioc2rpz_tls_sup_v6' — DNS over TLS / DoT (port 853), only when a certificate is configured</li>
%%   <li>`ioc2rpz_rest_tls_sup_v6' — REST management API over HTTPS, only when a certificate is configured</li>
%% </ul>
%%
%% All child supervisors use `one_for_one' strategy. Each child delegates to
%% {@link ioc2rpz_proc_sup} which runs a `simple_one_for_one' pool of
%% accept-loop workers.
%%
%% @param IPStr   IPv4 address string.
%% @param IPStr6  IPv6 address string.
%% @param Filename  Configuration file path.
%% @param DBDir   Database directory path.
%% @returns `{ok, {SupFlags, ChildSpecs}}'.
init([IPStr,IPStr6, Filename, DBDir]) ->
  Pid=self(),
  ioc2rpz_fun:logMessage("ioc2rpz version: ~p, IP: ~s ~s, PID: ~p, Config: ~p ~n", [?ioc2rpz_ver,IPStr,IPStr6,Pid,Filename]),

  {ok, PidDB} = ioc2rpz_db_sup:start_db(),
  {ok, _} = ioc2rpz_db:init_db(?DBStorage,DBDir,PidDB),

  ets:insert_new(cfg_table, {cfg_file,Filename}), ets:insert_new(cfg_table, {db_dir,DBDir}),
  ets:new(?RATE_LIMIT_TABLE, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]), %rate limiting table
  {ok,RPZ,_,_} = read_config3(Filename),
  %os:set_signal(sighup,handle),
  %os:set_signal(sigterm,handle),
  ioc2rpz_db:clean_DB(RPZ), %looks like it shouldn't be called here 2025-01-11
  inets:start(), ssl:start(),
%  spawn(ioc2rpz_sup,update_all_zones,[false]), %load zones to cache
%  spawn_opt(ioc2rpz_sup,update_all_zones,[false],[link,{fullsweep_after,0}]), %load zones to cache

  %load hot sources (which should be always in the hot cache)
  spawn_opt(ioc2rpz_sup,load_hotsources,[true],[{fullsweep_after,0}]),

  %load cached RPZ zones
  update_all_zones(false),

  %update sources and zones when expired
  timer:apply_interval(?ZoneRefTime,ioc2rpz_sup,load_hotsources,[false]),
  timer:apply_interval(?ZoneRefTime,ioc2rpz_sup,update_all_zones,[false]),

  ioc2rpz_fun:logMessage("ioc2rpz supervisor started ~n", []),

% Check if a certificate was configured
	[[Cert]] = ets:match(cfg_table,{srv,'_','_','_','_','$6','_'}),
  %ioc2rpz_fun:logMessage("cert '~p' ~n", [Cert]),
  if Cert /= [], Cert /= undefined -> ChildTLS=[
      %%%ioc2rpz TLS supervisors
      %#{id => ioc2rpz_tls_sup_v4,
      %start => {ioc2rpz_proc_sup, start_ioc2rpz_proc_sup, [[tls_sup,IPStr,inet]]},
      %restart => transient,
      %shutdown => 1000,
      %type => supervisor,
      %modules => [ioc2rpz_proc_sup]},

			%DoT
      #{id => ioc2rpz_tls_sup_v6,
      start => {ioc2rpz_proc_sup, start_ioc2rpz_proc_sup, [[tls6_sup,IPStr6,inet6]]},
      restart => permanent,
      shutdown => 1000,
      type => supervisor,
      modules => [ioc2rpz_proc_sup]},

			%DoH
			% DoH is for DNS UDP, We can support onlu small zones or SOA. Disabled for now.
      %#{id => ioc2rpz_doh_sup_v6,
      %start => {ioc2rpz_proc_sup, start_ioc2rpz_proc_sup, [[doh6_sup,IPStr6,inet6]]},
      %restart => transient,
      %shutdown => 1000,
      %type => supervisor,
      %modules => [ioc2rpz_proc_sup]},

      %REST
      #{id => ioc2rpz_rest_tls_sup_v6,
      start => {ioc2rpz_proc_sup, start_ioc2rpz_proc_sup, [[rest_tls6_sup,IPStr6,inet6]]},
      restart => permanent,
      shutdown => 1000,
      type => supervisor,
      modules => [ioc2rpz_proc_sup]}

    ];
    true -> ChildTLS=[]
  end,
  SupFlags = #{strategy => one_for_one, intensity => 60, period => 3600},
  ChildSpecs = [
    %%%ioc2rpz TCP supervisors
    %#{id => ioc2rpz_tcp_sup_v4,
    %start => {ioc2rpz_proc_sup, start_ioc2rpz_proc_sup, [[tcp_sup,IPStr,inet]]},
    %restart => transient,
    %shutdown => 1000,
    %type => supervisor,
    %modules => [ioc2rpz_proc_sup]},

		%DNS TCP
    #{id => ioc2rpz_tcp_sup_v6,
    start => {ioc2rpz_proc_sup, start_ioc2rpz_proc_sup, [[tcp6_sup,IPStr6,inet6]]},
    restart => permanent,
    shutdown => 1000,
    type => supervisor,
    modules => [ioc2rpz_proc_sup]},


    %%%ioc2rpz UDP supervisors
    %#{id => ioc2rpz_udp_sup_v4,
    %start => {ioc2rpz_proc_sup, start_ioc2rpz_proc_sup, [[udp_sup,IPStr,inet]]},
    %restart => transient,
    %shutdown => 1000,
    %type => supervisor,
    %modules => [ioc2rpz_proc_sup]},

		%DNS UDP
    #{id => ioc2rpz_udp_sup_v6,
    start => {ioc2rpz_proc_sup, start_ioc2rpz_proc_sup, [[udp6_sup,IPStr6,inet6]]},
    restart => transient,
    shutdown => 1000,
    type => supervisor,
    modules => [ioc2rpz_proc_sup]}

  ],
  {ok, {SupFlags, ChildSpecs ++ ChildTLS}}.



%TODO task to clean hotcache ?HotCacheTime
%timer:apply_after(10000,io,format,["Hello timer 10 sec\n"]).

%reload_config()->
%  [[Filename]] = ets:match(cfg_table,{cfg_file,'$1'}),
%  [[DBDir]] = ets:match(cfg_table,{db_dir,'$1'}),
%  ioc2rpz_fun:logMessage("ioc2rpz reloading configuration from ~p ~n", [Filename]),
%  [ ioc2rpz_db:save_zone_info(X) || [X] <- ets:match(cfg_table,{[rpz,'_'],'_','$4'}),  X#rpz.cache == <<"true">>],
%  ets:delete_all_objects(cfg_table),
%  ets:delete_all_objects(rpz_hotcache_table),
%  ets:delete_all_objects(stat_table),
%  ets:insert_new(cfg_table, {cfg_file,Filename}), ets:insert_new(cfg_table, {db_dir,DBDir}),
%  {ok,RPZ,_,_} = read_config2(Filename),
%  ioc2rpz_db:clean_DB(RPZ),
%  update_all_zones(false),
%  ok.

%% @doc Load or refresh hot-sources (sources marked `keep_in_cache').
%%
%% When called with `true', loads all hot-sources unconditionally (used at
%% startup). When called with any other value, only reloads sources whose
%% hot-cache has expired based on `hotcache_time'.
%%
%% @param LoadAll  `true' to force-load all hot-sources; any other value
%%                 for conditional refresh.
load_hotsources(true)->
  SW=[X#source.name || [X] <- ets:match(cfg_table, {[source,'_'],'$2'}), X#source.keep_in_cache == true],
  ioc2rpz_fun:logMessage("loading hot sources ~p ~n", [SW]),
  ioc2rpz:mrpz_from_ioc(SW,#rpz{serial=ioc2rpz_fun:curr_serial()},axfr,[]);

load_hotsources(_LoadAllSources)->

  SW3=[X || [X] <- ets:match(cfg_table, {[source,'_'],'$2'}), X#source.keep_in_cache == true],
  SW2=[ lists:flatten(ets:select(rpz_hotcache_table,[{{{X#source.name,axfr},'$2','_'},[{'=<', {'+', X#source.hotcache_time, '$2'},ioc2rpz_fun:curr_serial()}],[[X#source.name]]}])) || X <- SW3 ],
  SW=[X|| X <-SW2, X /= []],

  %spawn_opt(ioc2rpz,mrpz_from_ioc,[SW,#rpz{serial=ioc2rpz_fun:curr_serial()},axfr,[]],[{fullsweep_after,0}]).
  ioc2rpz_fun:logMessage("loading hot sources ~p ~n", [SW]),
  ioc2rpz:mrpz_from_ioc(SW,#rpz{serial=ioc2rpz_fun:curr_serial()},axfr,[]).

%%%
%%% Read configuration file
%%%
%% @doc Trigger a configuration reload from the current config file.
%%
%% Reads the config file path from `cfg_table', saves zone info for cached
%% zones, then delegates to {@link read_config3/2} with the given `Action'.
%%
%% Supported actions:
%% <ul>
%%   <li>`reload' — full reload: updates keys, sources, whitelists, and RPZ
%%       zones; forces AXFR on changed zones</li>
%%   <li>`updTkeys' — lightweight reload: only updates TSIG keys and key
%%       groups without refreshing zones</li>
%% </ul>
%%
%% @param Action  `reload' | `updTkeys'.
%% @returns `ok'.
reload_config3(Action)->
  [[Filename]] = ets:match(cfg_table,{cfg_file,'$1'}),
  ioc2rpz_fun:logMessage("ioc2rpz reloading configuration from ~p action ~p~n", [Filename, Action]),
%%% TODO we have to update get zone info..... to get rid of this.
  [ ioc2rpz_db:save_zone_info(X) || [X] <- ets:match(cfg_table,{[rpz,'_'],'_','$4'}),  X#rpz.cache == <<"true">>],
  read_config3(Filename,Action),
  ok.

%% @doc Parse the configuration file at startup.
%%
%% Opens the file with `file:consult/1' and delegates to the 8-arity
%% accumulator {@link read_config3/8} with `startup' mode. On error,
%% logs the reason and calls `exit(config_error)'.
%%
%% @param Filename  Path to the Erlang-term configuration file.
%% @returns `{ok, RPZ, Keys, Srv}' on success; does not return on error.
read_config3(Filename)  ->
  case file:consult(Filename) of
    {ok,CFG} -> read_config3(CFG,startup,#srv{},[],[],[],[],[]);
    {error, Error} when is_atom(Error) -> ioc2rpz_fun:logMessage("Error ~p opening or reading ~p ~n", [Error, Filename]), exit(config_error);
    {error, Reason} -> ioc2rpz_fun:logMessage("Error in configuration file ~p. ~p ~p ~n", [Filename,Reason, file:format_error(Reason)]), exit(config_error)
  end.

%% @doc Parse the configuration file for a reload or include action.
%%
%% Same as {@link read_config3/1} but accepts an `Action' parameter
%% (`reload', `updTkeys', or `include') to control how parsed terms
%% are applied to the running system.
%%
%% @param Filename  Path to the configuration file.
%% @param Action    `reload' | `updTkeys' | `include'.
read_config3(Filename,Action)  ->
  case file:consult(Filename) of
    {ok,CFG} -> read_config3(CFG,Action,#srv{},[],[],[],[],[]);
    {error, Error} when is_atom(Error) -> ioc2rpz_fun:logMessage("Error ~p opening or reading ~p ~n", [Error, Filename]);
    {error, Reason} -> ioc2rpz_fun:logMessage("Error in configuration file ~p. ~p ~p ~n", [Filename,Reason, file:format_error(Reason)])
  end.


%% @doc Recursive configuration parser (8-arity accumulator).
%%
%% Processes a list of Erlang configuration terms one at a time,
%% accumulating server settings, keys, key groups, whitelists, sources,
%% and RPZ zone definitions. Each clause handles a specific config term:
%%
%% <ul>
%%   <li>`{include, Filename}' — recursively parse another config file and
%%       merge its results</li>
%%   <li>`{srv, {...}}' — server identity: name, email, management keys, ACL</li>
%%   <li>`{cert, {...}}' — TLS certificate, private key, and CA cert paths</li>
%%   <li>`{key, {...}}' — TSIG key definition (name, algorithm, base64 secret,
%%       optional key-group membership)</li>
%%   <li>`{key_group, {...}}' — named group of TSIG keys</li>
%%   <li>`{whitelist, {...}}' — whitelist source (various arities for optional
%%       fields: userid, max_count, cache times, ioc_type, keep_in_cache)</li>
%%   <li>`{source, {...}}' — IOC source with AXFR/IXFR URLs (same optional
%%       field variants as whitelist)</li>
%%   <li>`{rpz, {...}}' — RPZ zone definition with SOA timers, cache settings,
%%       action, keys, sources, notify list, and whitelist references</li>
%% </ul>
%%
%% Terminal clauses (empty list) vary by `RType':
%% <ul>
%%   <li>`startup' — inserts all parsed config into `cfg_table' ETS</li>
%%   <li>`reload' — diffs against existing config, adds/removes/updates
%%       keys, sources, whitelists, and RPZ zones; forces AXFR on changed zones</li>
%%   <li>`updTkeys' — updates only TSIG keys and key groups</li>
%%   <li>`include' — returns accumulated values to the parent parse call</li>
%% </ul>
%%
%% @param CFGTerms    Remaining config terms to process.
%% @param RType       Parse mode atom: `startup' | `reload' | `updTkeys' | `include'.
%% @param Srv         Accumulated `#srv{}' record.
%% @param Keys        Accumulated list of `#key{}' records.
%% @param Key_Groups  Accumulated list of `#key_group{}' records.
%% @param WhiteLists  Accumulated list of whitelist `#source{}' records.
%% @param Sources     Accumulated list of IOC `#source{}' records.
%% @param RPZ         Accumulated list of `#rpz{}' records.
read_config3([{include,Filename}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  ioc2rpz_fun:logMessage("ioc2rpz including configuration from ~p~n", [Filename]),
	case read_config3(Filename,include) of
		{ok,_SrvI,KeysI,Key_GroupsI,WhiteListsI,SourcesI,RPZI} -> ok;
		_ -> KeysI=[],Key_GroupsI=[],WhiteListsI=[],SourcesI=[],RPZI=[]
	end,
  read_config3(REST,RType, Srv, KeysI ++ Keys,Key_GroupsI ++ Key_Groups, WhiteListsI ++ WhiteLists, SourcesI ++ Sources, RPZI ++ RPZ);


read_config3([{srv,{Serv,Email,MKeys,ACL}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  {ok,ServB}=ioc2rpz:domstr_to_bin(list_to_binary(Serv),0),
  {ok,EmailB}=ioc2rpz:domstr_to_bin(list_to_binary(Email),0),
  MKeysX=[ioc2rpz:domstr_to_bin(list_to_binary(X),0)|| X <- MKeys, is_list(X)], MKeysB=[X || {_,X} <- MKeysX], %keys group support
	KeyGroups=lists:append([ Y || {groups, Y} <- [ X || X <- MKeys, is_tuple(X) ], is_list(Y) ]),
  read_config3(REST,RType,Srv#srv{server=ServB,email=EmailB,mkeys=MKeysB,acl=ACL,key_groups=KeyGroups},Keys,Key_Groups,WhiteLists,Sources,RPZ);

read_config3([{cert,{Certfile,Keyfile,CAcertfile}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
%%% TODO validate the certificate
  read_config3(REST,RType,Srv#srv{cert=#cert{certfile=Certfile,keyfile=Keyfile,cacertfile=CAcertfile}},Keys,Key_Groups,WhiteLists,Sources,RPZ);

read_config3([{key,{KName,Alg,Key}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  [KNameB] = ioc2rpz_fun:strs_to_binary([KName]),
  {ok,KeyDNSF}=ioc2rpz:domstr_to_bin(KNameB,0),
  KeyB=base64:decode(Key),
  read_config3(REST,RType,Srv,[#key{name=KNameB,alg=Alg,key=KeyB,name_bin=KeyDNSF,key_groups=[]}|Keys],Key_Groups,WhiteLists,Sources,RPZ);

read_config3([{key,{KName,Alg,Key,Groups}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  [KNameB] = ioc2rpz_fun:strs_to_binary([KName]),
  {ok,KeyDNSF}=ioc2rpz:domstr_to_bin(KNameB,0),
  KeyB=base64:decode(Key),
  read_config3(REST,RType,Srv,[#key{name=KNameB,alg=Alg,key=KeyB,name_bin=KeyDNSF,key_groups=Groups}|Keys],Key_Groups,WhiteLists,Sources,RPZ);

read_config3([{key_group,{GName,Keys}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,[#key_group{name=GName,keys=Keys}|Key_Groups],WhiteLists,Sources,RPZ);

%%% No UserId, max count
read_config3([{whitelist,{Name,AXFR,REGEX}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,Key_Groups,[#source{name=Name,axfr_url=AXFR,regex=REGEX,hotcache_time=?HotCacheTime,hotcacheixfr_time=?HotCacheTimeIXFR,pid=[]}|WhiteLists],Sources,RPZ);
read_config3([{source,{Name,AXFR,IXFR,REGEX}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,Key_Groups,WhiteLists,[#source{name=Name,axfr_url=AXFR,ixfr_url=parse_ixfr_url(AXFR,IXFR),regex=REGEX,hotcache_time=?HotCacheTime,hotcacheixfr_time=?HotCacheTimeIXFR,pid=[],ioc_type="mixed",keep_in_cache=false}|Sources],RPZ);
%%% With UserId, max count
read_config3([{whitelist,{Name,AXFR,REGEX,UserID,Max_Count}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,Key_Groups,[#source{name=Name,axfr_url=AXFR,regex=REGEX,userid=UserID,max_ioc=Max_Count,hotcache_time=?HotCacheTime,hotcacheixfr_time=?HotCacheTimeIXFR,pid=[],ioc_type="mixed",keep_in_cache=false}|WhiteLists],Sources,RPZ);
read_config3([{source,{Name,AXFR,IXFR,REGEX,UserID,Max_Count}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,Key_Groups,WhiteLists,[#source{name=Name,axfr_url=AXFR,ixfr_url=parse_ixfr_url(AXFR,IXFR),regex=REGEX,userid=UserID,max_ioc=Max_Count,hotcache_time=?HotCacheTime,hotcacheixfr_time=?HotCacheTimeIXFR,pid=[],ioc_type="mixed",keep_in_cache=false}|Sources],RPZ);
%%% With UserId, max count, hotcache_time, hotcacheixfr_time
read_config3([{whitelist,{Name,AXFR,REGEX,UserID,Max_Count,HotCacheTime,HotCacheTimeIXFR}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,Key_Groups,[#source{name=Name,axfr_url=AXFR,regex=REGEX,userid=UserID,max_ioc=Max_Count,hotcache_time=HotCacheTime,hotcacheixfr_time=HotCacheTimeIXFR,pid=[]}|WhiteLists],Sources,RPZ);
read_config3([{source,{Name,AXFR,IXFR,REGEX,UserID,Max_Count,HotCacheTime,HotCacheTimeIXFR}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,Key_Groups,WhiteLists,[#source{name=Name,axfr_url=AXFR,ixfr_url=parse_ixfr_url(AXFR,IXFR),regex=REGEX,userid=UserID,max_ioc=Max_Count,hotcache_time=HotCacheTime,hotcacheixfr_time=HotCacheTimeIXFR,pid=[],ioc_type="mixed",keep_in_cache=false}|Sources],RPZ);

%%% TODO remove old configs
%%% with ioc_type, keep_in_cache added on 2021-08-31
read_config3([{whitelist,{Name,AXFR,REGEX,UserID,Max_Count,HotCacheTime,HotCacheTimeIXFR,IocType,KeepInCache}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,Key_Groups,[#source{name=Name,axfr_url=AXFR,regex=REGEX,userid=UserID,max_ioc=Max_Count,hotcache_time=HotCacheTime,hotcacheixfr_time=HotCacheTimeIXFR,pid=[],ioc_type=IocType,keep_in_cache=KeepInCache}|WhiteLists],Sources,RPZ);
read_config3([{source,{Name,AXFR,IXFR,REGEX,UserID,Max_Count,HotCacheTime,HotCacheTimeIXFR,IocType,KeepInCache}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  read_config3(REST,RType,Srv,Keys,Key_Groups,WhiteLists,[#source{name=Name,axfr_url=AXFR,ixfr_url=parse_ixfr_url(AXFR,IXFR),regex=REGEX,userid=UserID,max_ioc=Max_Count,hotcache_time=HotCacheTime,hotcacheixfr_time=HotCacheTimeIXFR,pid=[],ioc_type=IocType,keep_in_cache=KeepInCache}|Sources],RPZ);


read_config3([{rpz,{Zone, Refresh, Retry, Expiration, Neg_ttl, Cache, Wildcards, Action, AKeys, IOCType, AXFR_Time, IXFR_Time, Sources, NotifyList, Whitelist}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,SourcesC,RPZ) ->
  {ok,ZoneB} = ioc2rpz:domstr_to_bin(list_to_binary(Zone),0),
  AKeysX=[ioc2rpz:domstr_to_bin(list_to_binary(X),0)|| X <- AKeys, is_list(X) ], AKeysB=[X || {_,X} <- AKeysX],
	KeyGroups=lists:append([ Y || {groups, Y} <- [ X || X <- AKeys, is_tuple(X) ], is_list(Y) ]),
  SOATimers = <<Refresh:32,Retry:32,Expiration:32,Neg_ttl:32>>,
  %TODO update config to support protocol
  %temporary fix for issue #35
  NotifyListIP = [{udp,ioc2rpz_fun:str_to_ip(IPStr)} || IPStr <- NotifyList ],
  case {Cache,load_zone_info(#rpz{zone=ZoneB,axfr_time=AXFR_Time, zone_str=Zone,ixfr_time=AXFR_Time, cache=Cache})} of
    {"true",[ready = Status,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time,IOC_count,Rules_count, ready,_Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time]} -> ok;
    {"true",[ready= Status,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time,IOC_count,Rules_count, notready| _ ]} -> IXFR_Update_time=0, Serial_IXFR=0, NZ_Update_Time=0;
    {"true",[notready = Status|_]} -> Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0,NZ_Update_Time=0,IOC_count=0,Rules_count=0;
    _ -> Status = notready, Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0,NZ_Update_Time=0,IOC_count=0,Rules_count=0
  end,
  ZAction = case Action of
   Action when Action=="nodata";Action=="passthru";Action=="drop";Action=="tcp-only";Action=="nxdomain";Action=="blockns" -> list_to_binary(Action);
   [{LAction,LData}] when LAction=="redirect_domain" -> {list_to_binary(LAction),binary:split(list_to_binary(LData),<<".">>,[global])};
   [{LAction,LData}] when LAction=="redirect_ip" -> {list_to_binary(LAction),ioc2rpz_fun:ip_to_bin(LData)};
   _ -> ioc2rpz_fun:read_local_actions(Action)
  end,
  read_config3(REST,RType,Srv,Keys,Key_Groups,WhiteLists,SourcesC,[#rpz{zone=ZoneB, zone_str=Zone, soa_timers=SOATimers, cache=list_to_binary(Cache), wildcards=list_to_binary(Wildcards), action=ZAction, akeys=AKeysB, ioc_type=list_to_binary(IOCType), axfr_time=AXFR_Time, ixfr_time=IXFR_Time, sources=Sources, notifylist=NotifyListIP, whitelist=Whitelist, serial=Serial, status=Status, update_time=Update_time, ixfr_update_time=IXFR_Update_time, ixfr_nz_update_time=NZ_Update_Time, serial_ixfr=Serial_IXFR, key_groups=KeyGroups, ioc_count=IOC_count, rule_count=Rules_count}|RPZ]);

%% Terminal clause — startup: validate and insert all config into ETS.
read_config3([],startup,Srv,Keys,_Key_Groups,WhiteLists,Sources,RPZ)  ->
	Keys_V = [ validateCFGKeys(Y) || Y <- Keys ],
  [ ets:insert_new(cfg_table, {[key,X#key.name_bin],X#key.name,X#key.alg,X#key.key}) || X <- Keys_V ],
	[ ets:insert_new(cfg_table, {[key_group,Y,Z],Z}) || {Y,Z} <- lists:flatten([ gen_group_array(Y#key.name_bin,Y#key.key_groups) || Y <- Keys_V ]) ],

  SrvV = validateCFGSrv(Srv), ets:insert_new(cfg_table, {srv,SrvV#srv.server,SrvV#srv.email,SrvV#srv.mkeys,SrvV#srv.acl,SrvV#srv.cert,SrvV}),

%  WhiteLists_V=[ X || X <- [ validateCFGWL(Y) || Y <- WhiteLists ] ],
  WhiteLists_Used=lists:merge([ X#rpz.whitelist || X <- RPZ]),
  WhiteLists_V=[ validateCFGWL(Y) || Y <- WhiteLists, lists:member(Y#source.name,WhiteLists_Used) ],
  [ ets:insert_new(cfg_table, {[source,X#source.name],X}) || X <- WhiteLists_V ],
%  Sources_V=[ X || X <- [ validateCFGSrc(Y) || Y <- Sources ] ],
  Sources_Used=lists:merge([ X#rpz.sources || X <- RPZ]),
  Sources_V=[ validateCFGSrc(Y) || Y <- Sources, lists:member(Y#source.name,Sources_Used) ],
  [ ets:insert_new(cfg_table, {[source,X#source.name],X}) || X <- Sources_V ],

  [ ets:insert_new(cfg_table, {[rpz,X#rpz.zone],X#rpz.zone,X}) || X <- [ validateCFGRPZ(Y,Sources_V,WhiteLists_V) || Y <- RPZ ] ],
  {ok,RPZ,Keys,Srv};

%% Terminal clause — updTkeys: update TSIG keys and key groups only.
% Update TSIG Keys w/o refreshing zones.
read_config3([],updTkeys,Srv,Keys,_Key_Groups,_WhiteLists,_Sources,RPZ)  -> %Simplify key management with key_groups
% update TKEYs
  Keys_C=ets:match(cfg_table, {[key,'$1'],'$2','$3','$4'}),
  Keys_V=[ validateCFGKeys(Y) || Y <- Keys ],
  [ ets:delete(cfg_table, [key,X]) || [X,Y,_,_] <- Keys_C, not lists:member(Y, [ Z#key.name || Z <- Keys_V ]) ],
  [ ets:insert(cfg_table, {[key,X#key.name_bin],X#key.name,X#key.alg,X#key.key}) || X <- Keys_V ],

	ets:match_delete(cfg_table,{[key_group,'_','_'],'_'}),
	[ ets:insert_new(cfg_table, {[key_group,Y,Z],Z}) || {Y,Z} <- lists:flatten([ gen_group_array(Y#key.name_bin,Y#key.key_groups) || Y <- Keys_V ]) ],

% Update SRV Management TSIG Keys
  SrvV = validateCFGSrv(Srv), ets:update_element(cfg_table, srv, [{4, SrvV#srv.mkeys}]), ets:update_element(cfg_table, srv, [{7, SrvV}]),
% Update RPZs TSIG Keys
% TODO validate is a key is exists
  RPZ_C=[ X || [X] <- ets:match(cfg_table, {[rpz,'_'],'_','$3'})],
  [ ets:update_element(cfg_table, [rpz,X#rpz.zone], [{3, X#rpz{akeys=(lists:keyfind(X#rpz.zone,3,RPZ))#rpz.akeys}}]) || X <- RPZ_C ],
  ok;

%% Terminal clause — reload: diff existing config against new, apply changes.
%% Computes added/removed/updated sets for sources, whitelists, and RPZ zones.
%% Forces AXFR on updated zones and cleans up removed entries.
read_config3([],reload,Srv,Keys,_Key_Groups,WhiteLists,Sources,RPZ)  ->
  RPZ_C=[ X || [X] <- ets:match(cfg_table, {[rpz,'_'],'_','$3'})],
  [ ets:update_element(cfg_table, [rpz,X#rpz.zone], [{3, X#rpz{serial_new=-1, status=updating, update_time=-1}}]) || X <- RPZ_C ],

  Keys_C=ets:match(cfg_table, {[key,'$1'],'$2','$3','$4'}),
  Keys_V=[ validateCFGKeys(Y) || Y <- Keys ],
  [ ets:delete(cfg_table, [key,X]) || [X,Y,_,_] <- Keys_C, not lists:member(Y, [ Z#key.name || Z <- Keys_V ]) ],
  [ ets:insert(cfg_table, {[key,X#key.name_bin],X#key.name,X#key.alg,X#key.key}) || X <- Keys_V ],

	ets:match_delete(cfg_table,{[key_group,'_','_'],'_'}),
	[ ets:insert_new(cfg_table, {[key_group,Y,Z],Z}) || {Y,Z} <- lists:flatten([ gen_group_array(Y#key.name_bin,Y#key.key_groups) || Y <- Keys_V ]) ],

  SrvV = validateCFGSrv(Srv), ets:insert(cfg_table, {srv,SrvV#srv.server,SrvV#srv.email,SrvV#srv.mkeys,SrvV#srv.acl, SrvV#srv.cert,SrvV}),

  SW=ets:match(cfg_table, {[source,'_'],'$2'}),
  WhiteLists_C=[X||[X] <- SW,X#source.ixfr_url == undefined ],
  Sources_C=[X||[X] <- SW,X#source.ixfr_url /= undefined ],

  WhiteLists_Used=lists:merge([ X#rpz.whitelist || X <- RPZ]),
  Sources_Used=lists:merge([ X#rpz.sources || X <- RPZ]),

  WhiteLists_V=[ validateCFGWL(Y) || Y <- WhiteLists, lists:member(Y#source.name,WhiteLists_Used) ],
  Sources_V=[ validateCFGSrc(Y) || Y <- Sources, lists:member(Y#source.name,Sources_Used) ],


  WhiteLists_D = [ X || X <- WhiteLists_C, not lists:member(X#source.name, [ Z#source.name || Z <- WhiteLists_V ]) ],
  Sources_D = [ X || X <- Sources_C, not lists:member(X#source.name, [ Z#source.name || Z <- Sources_V ]) ],

  WhiteLists_N = [ X || X <- WhiteLists_V, not lists:member(X#source.name, [ Z#source.name || Z <- WhiteLists_C ]) ],
  Sources_N = [ X || X <- Sources_V, not lists:member(X#source.name, [ Z#source.name || Z <- Sources_C ]) ],

  WhiteLists_UPD = [ X || X <- WhiteLists_V, X#source.axfr_url /= (checkSrcRec(lists:keyfind(X#source.name,2,WhiteLists_C)))#source.axfr_url,lists:member(X#source.name, [ Z#source.name || Z <- WhiteLists_C ]) ],

  Sources_UPD = [ X || X <- Sources_V, ((X#source.axfr_url /= (checkSrcRec(lists:keyfind(X#source.name,2,Sources_C)))#source.axfr_url) or (X#source.ixfr_url /= (checkSrcRec(lists:keyfind(X#source.name,2,Sources_C)))#source.ixfr_url)) and (lists:member(X#source.name, [ Z#source.name || Z <- Sources_C ])) ],

  [ ets:insert(cfg_table, {[source,X#source.name],X}) || X <- WhiteLists_N ++ Sources_N ++ WhiteLists_UPD ++ Sources_UPD ],

  [ ets:delete(cfg_table, [source,X#source.name]) || X <- WhiteLists_D ++ Sources_D ],
  [ ets:delete(rpz_hotcache_table, {X#source.name,Y}) || X <- WhiteLists_UPD ++ Sources_UPD ++ WhiteLists_D ++ Sources_D, Y <- [axfr,ixfr] ],


  RPZ_V= [ Z || Z <- [ validateCFGRPZ(Y,Sources_V,WhiteLists_V) || Y <- RPZ ], Z /= [] ],
  RPZ_D = [ X || X <- RPZ_C, not lists:member(X#rpz.zone, [ Z#rpz.zone || Z <- RPZ_V ]) ],
  RPZ_N = [ X || X <- RPZ_V, not lists:member(X#rpz.zone, [ Z#rpz.zone || Z <- RPZ_C ]) ],

%TODO TKEYS and Groups should be checked
  RPZ_UPD = [ X || X <- RPZ_V, not checkRPZEq(X,lists:keyfind(X#rpz.zone,3,RPZ_C)),lists:member(X#rpz.zone, [ Z#rpz.zone || Z <- RPZ_C ]) ] ++
            [ X || X <- RPZ_V, ioc2rpz_fun:intersection(X#rpz.whitelist,[Z#source.name || Z <- WhiteLists_UPD]) /= [] ] ++
            [ X || X <- RPZ_V, ioc2rpz_fun:intersection(X#rpz.sources,[Z#source.name || Z <- Sources_UPD]) /= [] ],


  [ ioc2rpz_fun:logMessage("Zone ~p was updated. Terminating ~p.~n",[X#rpz.zone_str,X#rpz.pid]) || X <- RPZ_UPD, X#rpz.status == updating ],
  [ ioc2rpz_fun:logMessage("Zone ~p was removed. Terminating ~p.~n",[X#rpz.zone_str,X#rpz.pid]) || X <- RPZ_D, X#rpz.status == updating ],
  [ exit(X#rpz.pid,rpzRemoved) || X <- RPZ_D, X#rpz.status == updating], %TODO 2025-01-11 replace by supervisor:terminate_child(SupervisorPid, X#rpz.pid). Where to get supervisor?
  [ exit(X#rpz.pid,rpzUpdated) || X <- RPZ_UPD, X#rpz.status == updating],

  [ ets:delete(cfg_table, [rpz,X#rpz.zone]) || X <- RPZ_D ],
  [ ets:insert(cfg_table, {[rpz,X#rpz.zone],X#rpz.zone,X}) || X <- RPZ_V ],
  [ ets:match_delete(rpz_hotcache_table,{{pkthotcache,X#rpz.zone,'_'},'_','_'}) || X <- RPZ_D ++ RPZ_UPD ],

  ioc2rpz_db:clean_DB(RPZ_D), %Remove deleted zones 

  [ ets:update_element(cfg_table, [rpz,X#rpz.zone], [{3, X#rpz{status=forceAXFR}}]) || X <- RPZ_UPD ], %forceaxfr

  [ ioc2rpz_fun:logMessage("Whitelist ~p was added.~n",[X#source.name]) || X <- WhiteLists_N ],
  [ ioc2rpz_fun:logMessage("Whitelist ~p was updated.~n",[X#source.name]) || X <- WhiteLists_UPD ],
  [ ioc2rpz_fun:logMessage("Whitelist ~p was removed.~n",[X#source.name]) || X <- WhiteLists_D ],

  [ ioc2rpz_fun:logMessage("Source ~p was added.~n",[X#source.name]) || X <- Sources_N ],
  [ ioc2rpz_fun:logMessage("Source ~p was updated.~n",[X#source.name]) || X <- Sources_UPD ],
  [ ioc2rpz_fun:logMessage("Source ~p was removed.~n",[X#source.name]) || X <- Sources_D ],

  [ ioc2rpz_fun:logMessage("Zone ~p was added.~n",[X#rpz.zone_str]) || X <- RPZ_N ],
  [ ioc2rpz_fun:logMessage("Zone ~p was updated.~n",[X#rpz.zone_str]) || X <- RPZ_UPD ],
  [ ioc2rpz_fun:logMessage("Zone ~p was removed.~n",[X#rpz.zone_str]) || X <- RPZ_D ],
  
  %%%2025-01-11 
  %%% 
  %%% to avoid race conditions, commenting this line out. By default all zones should be updated by a scheduler every 60 seconds
  %%% 
  %update_all_zones(false),
  ok;

%% Terminal clause — include: return accumulated values to caller.
read_config3([],include,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ)  ->
	{ok,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ};

%% Catch-all clause: log unknown config terms and continue parsing.
read_config3([UTerm|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  ioc2rpz_fun:logMessage("Unknown configuration term ~p~n", [UTerm]),
  read_config3(REST,RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ).


%% @doc Compare two RPZ records for equality on configuration-relevant fields.
%%
%% Returns `true' if zone, SOA timers, cache, wildcards, action, ioc_type,
%% sources, and whitelist are all identical; `false' otherwise.
%% Used during reload to detect which zones need a forced AXFR.
checkRPZEq(R1,R2) when R1#rpz.zone == R2#rpz.zone,R1#rpz.soa_timers == R2#rpz.soa_timers,R1#rpz.cache == R2#rpz.cache,R1#rpz.wildcards == R2#rpz.wildcards,R1#rpz.action == R2#rpz.action,R1#rpz.ioc_type == R2#rpz.ioc_type,R1#rpz.sources == R2#rpz.sources,R1#rpz.whitelist == R2#rpz.whitelist ->
  true;

checkRPZEq(_R1,_R2) ->
  false.

%% @doc Safe accessor for source records during reload diffing.
%%
%% Returns the record unchanged if it has a defined name; otherwise returns
%% an empty `#source{}' to avoid badmatch on `lists:keyfind' returning `false'.
checkSrcRec(Rec) when Rec#source.name /= undefined ->
	Rec;
checkSrcRec(_Rec) ->
	#source{}.

%% @doc Validate a TSIG key record.
%%
%% Placeholder — currently returns the key unchanged. Future implementations
%% should verify key data format, algorithm support, and base64 encoding.
%%
%% @param Keys  A `#key{}' record.
%% @returns The validated (currently unchanged) `#key{}' record.
validateCFGKeys(Keys) -> %Check if key is good
  Keys.

%% @doc Validate the server configuration record.
%%
%% Placeholder — currently returns the server record unchanged. Future
%% implementations should verify management key references, ACL format,
%% email syntax, and certificate file existence/readability.
%%
%% @param Srv  A `#srv{}' record.
%% @returns The validated (currently unchanged) `#srv{}' record.
validateCFGSrv(Srv) -> %Check: MGMT Keys, ACL, email and cert
  Srv.

%% @doc Validate a whitelist source record.
%%
%% Placeholder — currently returns the whitelist unchanged. Future
%% implementations should verify regex compilation and AXFR URL
%% availability/syntax.
%%
%% @param WL  A whitelist `#source{}' record.
%% @returns The validated (currently unchanged) `#source{}' record.
validateCFGWL(WL) -> %Check: RegEx and AXFR URL availability. If URL is not available - log it and accept
  WL.

%% @doc Validate an IOC source record.
%%
%% Placeholder — currently returns the source unchanged. Future
%% implementations should verify regex compilation and URL
%% availability/syntax for both AXFR and IXFR URLs.
%%
%% @param Src  A `#source{}' record.
%% @returns The validated (currently unchanged) `#source{}' record.
validateCFGSrc(Src) -> %Check: RegEx and URLs availability. If URL is not available - log it and accept
  Src.

%% @doc Validate an RPZ zone record against available sources and whitelists.
%%
%% Checks that all source names referenced by the RPZ exist in the validated
%% sources list. If any source is missing, logs an error and returns an empty
%% list (the zone is not loaded). Whitelist validation is checked but currently
%% does not block loading.
%%
%% @param RPZ  An `#rpz{}' record.
%% @param S    List of validated `#source{}' records (IOC sources).
%% @param W    List of validated `#source{}' records (whitelists).
%% @returns The `#rpz{}' record if valid, or `[]' if sources are missing.
validateCFGRPZ(RPZ,S,W) -> %Check: Sources, Whitelists
  SV = not lists:member(false, [ lists:member(X, [ Z#source.name || Z <- S ])  || X <- RPZ#rpz.sources  ]),
  WV = not lists:member(false, [ lists:member(X, [ Z#source.name || Z <- W ])  || X <- RPZ#rpz.whitelist ]),
  if not SV -> % or not WV
    ioc2rpz_fun:logMessage("RPZ ~p was not loaded. No sources. Sources ~p Whitelists ~p.~n",[RPZ#rpz.zone_str,SV,WV]),
    [];
    true -> RPZ
  end.

%% @doc Expand a key's group memberships into `{GroupName, KeyNameBin}' pairs.
%% @private
gen_group_array(Value, Groups) ->
	[{X, Value} || X <- Groups].

%% @doc Resolve the IXFR URL, substituting `[:AXFR:]' placeholders with the AXFR URL.
%%
%% If `IXFR' is empty (`""'), falls back to the AXFR URL. Otherwise, splits
%% on `[:AXFR:]' tokens and replaces them with the actual AXFR URL string.
%%
%% @param AXFR  The AXFR URL string.
%% @param IXFR  The IXFR URL string (may contain `[:AXFR:]' placeholders).
%% @returns Resolved IXFR URL as a list of strings.
parse_ixfr_url(AXFR,"") -> %empty IXFR
  AXFR;

parse_ixfr_url(AXFR,IXFR) ->
  [ if X == "[:AXFR:]" -> AXFR; true -> X end || X <- re:split(IXFR,"(\\[:[^:]+:\\])",[{return,list},trim]), X /=[]].

%% @doc Load persisted zone info (AXFR + IXFR) for a zone from the database.
%%
%% Combines results from {@link load_axfr_zone_info/1} and
%% {@link load_ixfr_zone_info/1} to determine the zone's current status,
%% serial, and timing information.
%%
%% @param Zone  An `#rpz{}' record with at least `zone', `axfr_time',
%%              `zone_str', `ixfr_time', and `cache' fields populated.
%% @returns A flat list of zone state values used during config parsing.
load_zone_info(Zone) ->
  load_axfr_zone_info(Zone) ++ load_ixfr_zone_info(Zone).

load_axfr_zone_info(Zone) ->
  load_axfr_zone_info(?DBStorage,Zone).

load_axfr_zone_info(ets,Zone) ->
  CTime=ioc2rpz_fun:curr_serial(),%erlang:system_time(seconds),
  case ioc2rpz_db:get_zone_info(Zone,axfr) of %ets:match(rpz_axfr_table,{{axfr_rpz_cfg,Zone#rpz.zone},'$1','$2','$3','$4','$5','$6','$7'})
    [[_,Serial,Soa_timers,Cache,Wildcards,Sources,Ioc_md5,Update_time,IOC_count,Rules_count]] when (Update_time+Zone#rpz.axfr_time)>CTime ->
      ioc2rpz_fun:logMessage("Get AXFR zone ~p serial ~p status ready. Last update ~p ~n",[Zone#rpz.zone_str,Serial,Update_time]),
      [ready,Serial,Soa_timers,Cache,Wildcards,Sources,Ioc_md5,Update_time,IOC_count,Rules_count];
    [[_,Serial,Soa_timers,Cache,Wildcards,Sources,Ioc_md5,Update_time,IOC_count,Rules_count]] when Zone#rpz.cache == "true" ->
      ioc2rpz_fun:logMessage("Get AXFR zone ~p serial ~p status notready ~n",[Zone#rpz.zone_str,Serial]),
      [notready,Serial,Soa_timers,Cache,Wildcards,Sources,Ioc_md5,Update_time,IOC_count,Rules_count];
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
  case ioc2rpz_db:get_zone_info(Zone,ixfr) of
    [[_,Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time]] when (IXFR_Update_time+Zone#rpz.ixfr_time)>CTime ->
      ioc2rpz_fun:logMessage("Get IXFR zone ~p serial ~p status ready ~n",[Zone#rpz.zone_str,Serial_IXFR]),
      [ready,Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time];
    [[_,Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time]] when Zone#rpz.cache == "true"  ->
      ioc2rpz_fun:logMessage("Get IXFR zone ~p serial ~p status notready ~n",[Zone#rpz.zone_str,Serial_IXFR]),
      [notready,Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time];
    _NonCache when Zone#rpz.cache == "false" ->
      [];
    _Else ->
      ioc2rpz_fun:logMessage("Get IXFR zone ~p serial 0 status notready ~n",[Zone#rpz.zone_str]),
      []
  end;
load_ixfr_zone_info(mnesia,_Zone) ->
  ok.

%% @doc Check if a process is alive, treating `undefined' as alive.
%%
%% Used to detect stale zone-update PIDs stored in RPZ records. Returns
%% `true' for `undefined' (no process was ever started) so the zone is
%% considered eligible for a new update.
%% @private
my_process_is_alive(undefined)->
  true;
my_process_is_alive(Pid)->
  is_process_alive(Pid).

%% @doc Trigger zone updates for all cached RPZ zones.
%%
%% When called with `true', forces a full AXFR update on every cached zone
%% regardless of expiry. When called with `false', only updates zones whose
%% AXFR or IXFR refresh interval has elapsed, or zones marked `forceAXFR'
%% (e.g., after a config reload).
%%
%% For each eligible zone, spawns a new process via {@link update_zone_full/1}
%% or {@link update_zone_inc/1}. Skips zones that are already in `updating'
%% status with a live process to avoid duplicate concurrent updates.
%%
%% Called periodically by `timer:apply_interval' set up in {@link init/1}.
%%
%% @param Force  `true' to force-update all zones; `false' for expiry-based.
%% @returns `ok'.
update_all_zones(true) -> %force update all zones
  AllRPZ = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
  [ spawn_opt(ioc2rpz_sup,update_zone_full,[X],[{fullsweep_after,0}]) || [X] <- AllRPZ,  X#rpz.cache == <<"true">>],
	ok;
update_all_zones(false) -> %update expired zones
  CTime=ioc2rpz_fun:curr_serial(),%erlang:system_time(seconds),
  AllRPZ = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
  [ioc2rpz_fun:logMessage("update_all_zones(false). Start full zone update Zone ~p serial ~p full refresh time ~p, Ctime ~p cache ~p status ~p ~n",[X#rpz.zone_str,X#rpz.ixfr_update_time, X#rpz.ixfr_time,CTime, X#rpz.cache, X#rpz.status]) || [X] <- AllRPZ,((((X#rpz.update_time + X#rpz.axfr_time) < CTime) and ((X#rpz.status /= updating) or ((X#rpz.status == updating) and not my_process_is_alive(X#rpz.pid)) )) or (X#rpz.status == forceAXFR)) and (X#rpz.cache == <<"true">>)],
  [ spawn_opt(ioc2rpz_sup,update_zone_full,[X],[{fullsweep_after,0}]) || [X] <- AllRPZ,((((X#rpz.update_time + X#rpz.axfr_time) < CTime) and ((X#rpz.status /= updating) or ((X#rpz.status == updating) and not my_process_is_alive(X#rpz.pid)) )) or (X#rpz.status == forceAXFR)) and (X#rpz.cache == <<"true">>) ],
  [ioc2rpz_fun:logMessage("update_all_zones(false). Start incremental update Zone ~p serial ~p full refresh time ~p, Ctime ~p cache ~p status ~p ~n",[X#rpz.zone_str,X#rpz.ixfr_update_time, X#rpz.ixfr_time,CTime, X#rpz.cache, X#rpz.status]) || [X] <- AllRPZ, ((X#rpz.update_time + X#rpz.axfr_time) > CTime) and ((X#rpz.ixfr_update_time + X#rpz.ixfr_time) < CTime) and (X#rpz.cache == <<"true">>) and (X#rpz.status /= updating) and (X#rpz.ixfr_time /= 0)],
  [ spawn_opt(ioc2rpz_sup,update_zone_inc,[X],[{fullsweep_after,0}]) || [X] <- AllRPZ, ((X#rpz.update_time + X#rpz.axfr_time) > CTime) and ((X#rpz.ixfr_update_time + X#rpz.ixfr_time) < CTime) and (X#rpz.cache == <<"true">>) and (X#rpz.status /= updating) and (X#rpz.ixfr_time /= 0) ],
	ok.


%% @doc Perform a full AXFR zone update for a single RPZ zone.
%%
%% Sets the zone status to `updating' in `cfg_table', rebuilds the zone
%% from IOC sources, and updates the serial, rule/IOC counts, and timestamps.
%% If the zone content is unchanged (same MD5), only the check timestamp is
%% updated. Otherwise, cached packets are deleted, DNS NOTIFY is sent to
%% secondaries, and zone data is persisted.
%%
%% @param Zone  An `#rpz{}' record for the zone to update.
%% @returns `ok'.
update_zone_full(Zone) ->
  Pid=self(),
  CTime=ioc2rpz_fun:curr_serial_60(),%CTime=erlang:system_time(seconds),
  ioc2rpz_fun:logMessage("Zone ~p serial ~p, refresh time ~p current status ~p ~n",[Zone#rpz.zone_str,Zone#rpz.serial, Zone#rpz.axfr_time, Zone#rpz.status]),
  [[NSServ,MailAddr|_Rest]] = ets:match(cfg_table,{srv,'$2','$3','$4','$5','$6','$7'}),
  SOA = <<NSServ/binary,MailAddr/binary,(ioc2rpz_fun:curr_serial()):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOA)):16, SOA/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  ioc2rpz_fun:logMessage("Updating zone ~p full ~n",[Zone#rpz.zone_str]),
  ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{serial_new=CTime, status=updating, update_time=CTime, pid=Pid}}]),
  {Status,MD5, NRules, NIOCs} = ioc2rpz:send_zone_live(<<>>,cache,Zone#rpz{serial=CTime},<<>>,<<(Zone#rpz.zone)/binary,0:32>>, SOAREC,NSRec,[],[]),
  if Status == updateSOA ->
      ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=ready, serial_new=0, ioc_md5=MD5, update_time=CTime, ixfr_update_time=CTime, ixfr_nz_update_time=CTime, pid=undefined}}]),
      ioc2rpz_fun:logMessage("Zone ~p is the same. Checked in ~p seconds, check timestamp ~p ~n",[Zone#rpz.zone_str, (ioc2rpz_fun:curr_serial()- CTime), CTime]);
    true ->
      %if Zone#rpz.serial_ixfr == 0 -> Serial_IXFR=CTime; true -> Serial_IXFR=Zone#rpz.serial_ixfr end,
      ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{serial=CTime, status=ready, serial_new=0, ioc_md5=MD5, update_time=CTime, ixfr_update_time=CTime, ixfr_nz_update_time=CTime, serial_ixfr=CTime, pid=undefined,ioc_count=NIOCs, rule_count=NRules}}]),
      ioc2rpz_db:delete_db_pkt(Zone),
      %erlang:garbage_collect(), %TODO check if need
      ioc2rpz:send_notify(Zone),
      ioc2rpz_fun:logMessage("Zone ~p updated in ~p seconds, new serial ~p, ~p rules, ~p indicators.~n",[Zone#rpz.zone_str, (ioc2rpz_fun:curr_serial_60() - CTime), CTime, NRules, NIOCs])
  end,
  ioc2rpz_db:saveZones(),
  ok.


%% @doc Trigger incremental (IXFR) zone updates for all cached zones.
%%
%% `true' forces incremental update on all cached zones. `false' only
%% updates zones whose IXFR interval has elapsed and that are not
%% currently updating.
update_all_zones_inc(true) -> %force inc update all zones
  AllRPZ = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
  [ spawn(ioc2rpz_sup,update_zone_inc,[X]) || [X] <- AllRPZ,  X#rpz.cache == <<"true">>],
	ok;
update_all_zones_inc(false) -> %update inc expired zones
  CTime=ioc2rpz_fun:curr_serial(),%erlang:system_time(seconds),
  AllRPZ = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
  [io:fwrite(group_leader(),"Zone ~p serial ~p full refresh time ~p cache ~p status ~p ~n",[X#rpz.zone_str,X#rpz.ixfr_update_time, X#rpz.ixfr_time, X#rpz.cache, X#rpz.status]) || [X] <- AllRPZ, (X#rpz.ixfr_update_time + X#rpz.ixfr_time) < CTime,  X#rpz.cache == <<"true">>, X#rpz.status /= updating, X#rpz.ixfr_time /= 0],
  [ spawn(ioc2rpz_sup,update_zone_inc,[X]) || [X] <- AllRPZ,(X#rpz.ixfr_update_time + X#rpz.ixfr_time) < CTime,  X#rpz.cache == <<"true">>, X#rpz.status /= updating, X#rpz.ixfr_time /= 0 ],
	ok.

%% @doc Perform an incremental (IXFR) zone update for a single RPZ zone.
%%
%% Fetches new IOC indicators via IXFR sources, writes new records to the
%% IXFR table, and if changes are detected, rebuilds the full AXFR zone
%% cache. Sends DNS NOTIFY to secondaries on successful update.
%%
%% @param Zone  An `#rpz{}' record for the zone to update incrementally.
%% @returns `ok'.
update_zone_inc(Zone) ->
  %io:fwrite(group_leader(),"Zone ~p IOC  ~p ~n",[Zone#rpz.zone_str,IOC]),
  Pid=self(),
	ioc2rpz_fun:logMessage("Process PID ~p incremental update ~p started ~n",[Pid, Zone#rpz.zone_str]),
  NRbefore=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3'},[],['true']}]),
  CTime=ioc2rpz_fun:curr_serial_60(), %erlang:system_time(seconds),
  ioc2rpz_fun:logMessage("Updating zone ~p inc. Last IXFR update ~p seconds ago, last non-zero update ~p seconds ago~n",[Zone#rpz.zone_str,(CTime - Zone#rpz.ixfr_update_time),(CTime-Zone#rpz.ixfr_nz_update_time)]),
  ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=updating, ixfr_update_time=CTime, pid=Pid}}]),
  case {ioc2rpz:mrpz_from_ioc(Zone#rpz{serial=CTime},ixfr),ioc2rpz_db:read_db_record(Zone,CTime,updated)} of
    {[],[]} -> % No new records, no expired records
      ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=ready, ixfr_update_time=CTime, pid=undefined}}]); %, ixfr_update_time=CTime
    {IOC,_} ->  %TODO double check that we really have an update. It looks like We have full file and TIDE send the same response.
      case ioc2rpz_db:write_db_record(Zone#rpz{serial=CTime},IOC,ixfr) of % New IOC were added or update
        {ok,0} ->
					?logDebugMSG("Zone ~p was not updated.  State: Ready~n",[Zone#rpz.zone_str]),
					ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=ready, ixfr_update_time=CTime, pid=undefined}}]); %, ixfr_update_time=CTime
        {ok,NewIOCs} ->
					?logDebugMSG("Rebuilding AXFR zone ~p. New IOCs ~p~n",[Zone#rpz.zone_str,NewIOCs]),
          {ok, NRules, NIOCs} = rebuild_axfr_zone(Zone#rpz{serial=CTime}),
					?logDebugMSG("AXFR zone ~p was rebuilded. ~p rules ~p indicators. Parsed ~p indicators.~n",[Zone#rpz.zone_str, NRules, NIOCs,length(IOC)]),
          NRafter=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3'},[],['true']}]),
          ioc2rpz_fun:logMessage("Zone ~p records before ~p after ~p. ~n",[Zone#rpz.zone_str, NRbefore, NRafter]),
          ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=ready, serial=CTime, ixfr_update_time=CTime, ixfr_nz_update_time=CTime, pid=undefined, ioc_count=NIOCs, rule_count=NRules}}]),
          ioc2rpz_db:delete_db_pkt(Zone),
          ioc2rpz_db:saveZones(),
          ioc2rpz:send_notify(Zone);
        {Error,Msg} ->
          ioc2rpz_fun:logMessage("Error ~p while updating ~p. Message: ~p~n",[Error,Zone#rpz.zone_str,Msg])
      end
  end,
	ioc2rpz_fun:logMessage("Process PID ~p incremental update ~p finished in ~p seconds ~n",[Pid, Zone#rpz.zone_str, (ioc2rpz_fun:curr_serial_60()-CTime)]),
	ok.

%% @doc Rebuild the full AXFR zone cache from the current IXFR record set.
%%
%% Reads all active IOC records from the database, constructs SOA/NS records,
%% and regenerates the zone packet cache. Used after an incremental update
%% adds new indicators.
%%
%% @param Zone  An `#rpz{}' record with the current serial.
%% @returns `{ok, NRules, NIOCs}' with the count of rules and indicators.
rebuild_axfr_zone(Zone) ->
  IOCs = ioc2rpz_db:read_db_record(Zone,0,active),
  %ioc2rpz_fun:logMessage("rebuild AXFR IOCs ~p ~n",[IOCs]),
  IOC = [{X,Exp,IoCType} || [X,_,Exp,IoCType] <- IOCs],
  [[NSServ,MailAddr|_Rest]] = ets:match(cfg_table,{srv,'$2','$3','$4','$5','$6','$7'}),
  SOA = <<NSServ/binary,MailAddr/binary,(ioc2rpz_fun:curr_serial()):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOA)):16, SOA/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  {ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})$"),
  Questions = <<(Zone#rpz.zone)/binary,0:32>>,
  PktHLen = 12+byte_size(Questions),
  T_ZIP_L=ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]), % нужны ли {read_concurrency, true}, {write_concurrency, true} ???
	%T_ZIP_L=init_T_ZIP_L(Zone),
  {ok, NRules, NIOCs} = ioc2rpz:send_packets(<<>>,IOC, [], 0, 0, true, <<>>, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,[],0,cache,0,false,no),
  ioc2rpz_fun:logMessage("Zone ~p, # of rules ~p, # of IOCs ~p ~n", [Zone#rpz.zone_str, NRules, NIOCs]),
  ets:delete(T_ZIP_L),
  {ok, NRules, NIOCs}.
