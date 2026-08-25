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
-include_lib("eunit/include/eunit.hrl").
-export([start_ioc2rpz_sup/1,stop_ioc2rpz_sup/0,update_all_zones/1,update_zone_full/1,
        update_zone_inc/1,reload_config3/1,read_config3/1,load_hotsources/1]).
-export([init/1]).
-export([track_enabled/2, source_index_list/1]).

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
%%   <li>Starts the database supervisor and initialises ETS/mnesia storage
%%       (including the `cfg_table' and `rate_limits' ETS tables)</li>
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
  %the rate limiting table is created by ioc2rpz_db:init_db/3 above, together
  %with the other named tables, so it has the database supervisor as its heir
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

  %cleanup expired rate-limit entries to prevent unbounded ETS growth
  timer:apply_interval(?RATE_LIMIT_WINDOW,ioc2rpz_fun,cleanup_rate_limit_table,[]),

  %cleanup expired hot-cache packet entries to prevent unbounded ETS growth
  timer:apply_interval(?HotCacheTime * 1000,ioc2rpz_db,cleanup_hotcache,[]),

  ioc2rpz_fun:logMessage("ioc2rpz supervisor started ~n", []),

% Check if a certificate was configured
	[[Cert]] = ets:match(cfg_table,{srv,'_','_','_','_','$6','_'}),
  %ioc2rpz_fun:logMessage("cert '~p' ~n", [Cert]),
  %record a baseline fingerprint of the certificate files so a later config
  %reload can detect certificate renewals and restart the TLS listeners (task 34)
  store_cert_hash(),
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
    restart => permanent,
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
  %On a full reload, pick up renewed TLS certificates by restarting the TLS
  %listeners if the certificate files changed on disk (task 34).
  case Action of
    reload -> maybe_reload_cert();
    _ -> ok
  end,
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
  check_config_permissions(Filename),
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
  check_config_permissions(Filename),
  case file:consult(Filename) of
    {ok,CFG} -> read_config3(CFG,Action,#srv{},[],[],[],[],[]);
    {error, Error} when is_atom(Error) -> ioc2rpz_fun:logMessage("Error ~p opening or reading ~p ~n", [Error, Filename]);
    {error, Reason} -> ioc2rpz_fun:logMessage("Error in configuration file ~p. ~p ~p ~n", [Filename,Reason, file:format_error(Reason)])
  end.

%% @doc Logs a security warning if the configuration file OR its containing
%% directory is world-writable (task 20). A world-writable config can be
%% silently tampered with by any local user; equally, a world-writable parent
%% directory (without the sticky bit) lets any local user rename/replace the
%% file even when the file itself is not writable — the same tampering risk.
%% Both are checked on startup, reload, and for each included file. This is
%% non-fatal — parsing continues regardless. A missing/unreadable file is left
%% for `file:consult/1' to report.
%%
%% @param Filename Path to the configuration file to check.
%% @returns `ok'.
check_config_permissions(Filename) ->
  check_file_world_writable(Filename),
  check_dir_world_writable(filename:dirname(Filename)),
  ok.

%% @doc Warn if the config file itself is world-writable.
check_file_world_writable(Filename) ->
  case file:read_file_info(Filename) of
    {ok, FileInfo} ->
      case FileInfo#file_info.mode band 8#002 of
        0 -> ok;
        _ ->
          ioc2rpz_fun:logMessage("WARNING: configuration file ~p is world-writable (mode ~.8.0b). A world-writable config can be tampered with by any local user; run 'chmod o-w ~s' to restrict access.~n", [Filename, FileInfo#file_info.mode, Filename])
      end;
    {error, _Reason} -> ok
  end.

%% @doc Warn if the directory containing the config file is world-writable
%% WITHOUT the sticky bit. Such a directory lets any local user rename/replace
%% files inside it (including the config), regardless of the file's own mode.
%% A world-writable directory WITH the sticky bit set (e.g. /tmp, mode 1777)
%% only allows the owner to delete/rename its own files, so it is not flagged.
check_dir_world_writable(Dir) ->
  case file:read_file_info(Dir) of
    {ok, DirInfo} ->
      WorldWritable = (DirInfo#file_info.mode band 8#002) /= 0,
      Sticky        = (DirInfo#file_info.mode band 8#1000) /= 0,
      case WorldWritable andalso not Sticky of
        false -> ok;
        true  ->
          ioc2rpz_fun:logMessage("WARNING: configuration directory ~p is world-writable without the sticky bit (mode ~.8.0b). Any local user can replace files in it (including the config); run 'chmod o-w ~s' to restrict access.~n", [Dir, DirInfo#file_info.mode, Dir])
      end;
    {error, _Reason} -> ok
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


%%% Extended server clause with an optional global track_sources default (off | auto | on).
%%% A 5-element {srv,{...}} tuple won't collide with the 4-element clause below; Erlang matches
%%% clauses in order. Absent (4-element tuple) ⇒ track_sources stays at its `off` default.
read_config3([{srv,{Serv,Email,MKeys,ACL,TrackSources}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,Sources,RPZ) ->
  {ok,ServB}=ioc2rpz:domstr_to_bin(list_to_binary(Serv),0),
  {ok,EmailB}=ioc2rpz:domstr_to_bin(list_to_binary(Email),0),
  MKeysX=[ioc2rpz:domstr_to_bin(list_to_binary(X),0)|| X <- MKeys, is_list(X)], MKeysB=[X || {_,X} <- MKeysX], %keys group support
	KeyGroups=lists:append([ Y || {groups, Y} <- [ X || X <- MKeys, is_tuple(X) ], is_list(Y) ]),
  TrackSourcesV=validate_track_sources(TrackSources),
  read_config3(REST,RType,Srv#srv{server=ServB,email=EmailB,mkeys=MKeysB,acl=ACL,key_groups=KeyGroups,track_sources=TrackSourcesV},Keys,Key_Groups,WhiteLists,Sources,RPZ);

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


%%% Extended 16-field rpz clause carrying an explicit per-feed track_sources
%%% value as the trailing (16th) tuple element (auto | true | false). It does
%%% everything the 15-field clause below does, but also sets #rpz.track_sources
%%% to the validated value. A 16-element {rpz,{...}} tuple won't collide with
%%% the 15-element clause below; Erlang matches clauses in order. An
%%% unrecognised value defaults to `undefined` (⇒ inherit the server global
%%% default). Existing 15-field configs are unaffected (R2).
read_config3([{rpz,{Zone0, Refresh, Retry, Expiration, Neg_ttl, Cache, Wildcards, Action, AKeys, IOCType, AXFR_Time, IXFR_Time, Sources, NotifyList, Whitelist, TrackSources}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,SourcesC,RPZ) ->
  %RFC 4343: zone names are case-insensitive. Canonicalise to lower case here so
  %the cfg_table key matches the (also lower-cased) query name in
  %ioc2rpz:rpz_zone/1 whatever case the config file or the client used.
  Zone = zone_name_lowcase(Zone0),
  {ok,ZoneB} = ioc2rpz:domstr_to_bin(list_to_binary(Zone),0),
  AKeysX=[ioc2rpz:domstr_to_bin(list_to_binary(X),0)|| X <- AKeys, is_list(X) ], AKeysB=[X || {_,X} <- AKeysX],
	KeyGroups=lists:append([ Y || {groups, Y} <- [ X || X <- AKeys, is_tuple(X) ], is_list(Y) ]),
  SOATimers = <<Refresh:32,Retry:32,Expiration:32,Neg_ttl:32>>,
  %TODO update config to support protocol
  %temporary fix for issue #35
  NotifyListIP = [{udp,ioc2rpz_fun:str_to_ip(IPStr)} || IPStr <- NotifyList ],
  ZoneInfoReq = #rpz{zone=ZoneB,axfr_time=AXFR_Time, zone_str=Zone,ixfr_time=AXFR_Time, cache=Cache},
  case {Cache,load_zone_info(ZoneInfoReq)} of
    {"true",[ready = Status0,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time,IOC_count,Rules_count, ready,_Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time]} -> ok;
    {"true",[ready= Status0,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time,IOC_count,Rules_count, notready| _ ]} -> IXFR_Update_time=0, Serial_IXFR=0, NZ_Update_Time=0;
    {"true",[notready = Status0|_]} -> Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0,NZ_Update_Time=0,IOC_count=0,Rules_count=0;
    _ -> Status0 = notready, Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0,NZ_Update_Time=0,IOC_count=0,Rules_count=0
  end,
  %% Task 3.2 (R6): if this cached zone's source list changed since the cached
  %% masks were built, force an AXFR so masks are re-derived. No-op for
  %% notready/non-cached zones and when the signature matches.
  Status = maybe_force_source_axfr(Cache, Status0, ZoneInfoReq, Sources),
  ZAction = case Action of
   Action when Action=="nodata";Action=="passthru";Action=="drop";Action=="tcp-only";Action=="nxdomain";Action=="blockns" -> list_to_binary(Action);
   [{LAction,LData}] when LAction=="redirect_domain" -> {list_to_binary(LAction),binary:split(list_to_binary(LData),<<".">>,[global])};
   [{LAction,LData}] when LAction=="redirect_ip" -> {list_to_binary(LAction),ioc2rpz_fun:ip_to_bin(LData)};
   _ -> ioc2rpz_fun:read_local_actions(Action)
  end,
  TrackSourcesV=validate_feed_track_sources(TrackSources),
  read_config3(REST,RType,Srv,Keys,Key_Groups,WhiteLists,SourcesC,[#rpz{zone=ZoneB, zone_str=Zone, soa_timers=SOATimers, cache=list_to_binary(Cache), wildcards=list_to_binary(Wildcards), action=ZAction, akeys=AKeysB, ioc_type=list_to_binary(IOCType), axfr_time=AXFR_Time, ixfr_time=IXFR_Time, sources=Sources, notifylist=NotifyListIP, whitelist=Whitelist, serial=Serial, status=Status, update_time=Update_time, ixfr_update_time=IXFR_Update_time, ixfr_nz_update_time=NZ_Update_Time, serial_ixfr=Serial_IXFR, key_groups=KeyGroups, ioc_count=IOC_count, rule_count=Rules_count, track_sources=TrackSourcesV}|RPZ]);

%%% Existing 15-field rpz clause (source attribution not specified). It builds
%%% #rpz{...} WITHOUT setting track_sources, so the field keeps its record
%%% default of `undefined` (⇒ inherit the server global default,
%%% #srv.track_sources, which is `off` unless configured). This preserves
%%% backward compatibility: existing config files load unchanged and behave as
%%% off — no tracking, no rebuilds, unchanged API (R2/R4).
read_config3([{rpz,{Zone0, Refresh, Retry, Expiration, Neg_ttl, Cache, Wildcards, Action, AKeys, IOCType, AXFR_Time, IXFR_Time, Sources, NotifyList, Whitelist}}|REST],RType,Srv,Keys,Key_Groups,WhiteLists,SourcesC,RPZ) ->
  %see the 16-field clause above: zone names are canonicalised to lower case
  Zone = zone_name_lowcase(Zone0),
  {ok,ZoneB} = ioc2rpz:domstr_to_bin(list_to_binary(Zone),0),
  AKeysX=[ioc2rpz:domstr_to_bin(list_to_binary(X),0)|| X <- AKeys, is_list(X) ], AKeysB=[X || {_,X} <- AKeysX],
	KeyGroups=lists:append([ Y || {groups, Y} <- [ X || X <- AKeys, is_tuple(X) ], is_list(Y) ]),
  SOATimers = <<Refresh:32,Retry:32,Expiration:32,Neg_ttl:32>>,
  %TODO update config to support protocol
  %temporary fix for issue #35
  NotifyListIP = [{udp,ioc2rpz_fun:str_to_ip(IPStr)} || IPStr <- NotifyList ],
  ZoneInfoReq = #rpz{zone=ZoneB,axfr_time=AXFR_Time, zone_str=Zone,ixfr_time=AXFR_Time, cache=Cache},
  case {Cache,load_zone_info(ZoneInfoReq)} of
    {"true",[ready = Status0,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time,IOC_count,Rules_count, ready,_Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time]} -> ok;
    {"true",[ready= Status0,Serial,_Soa_timersC,_CacheC,_WildcardsC,_SourcesC,_Ioc_md5,Update_time,IOC_count,Rules_count, notready| _ ]} -> IXFR_Update_time=0, Serial_IXFR=0, NZ_Update_Time=0;
    {"true",[notready = Status0|_]} -> Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0,NZ_Update_Time=0,IOC_count=0,Rules_count=0;
    _ -> Status0 = notready, Update_time=0, IXFR_Update_time=0, Serial_IXFR=0, Serial=0,NZ_Update_Time=0,IOC_count=0,Rules_count=0
  end,
  %% Task 3.2 (R6): if this cached zone's source list changed since the cached
  %% masks were built, force an AXFR so masks are re-derived. No-op for
  %% notready/non-cached zones and when the signature matches.
  Status = maybe_force_source_axfr(Cache, Status0, ZoneInfoReq, Sources),
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

  %% Capture the OLD server record BEFORE overwriting it so a change to the
  %% global track_sources default is detectable below: feeds that INHERIT the
  %% default (track_sources=undefined) can flip their effective tracking state
  %% without any per-feed edit, and must then be force-rebuilt to (re)derive
  %% source masks (design §12). Without this the masks stay 0 and the API
  %% reports "(unavailable)" for multi-source feeds until an unrelated AXFR runs.
  OldSrv = case ets:match(cfg_table,{srv,'_','_','_','_','_','$7'}) of [[OS]] -> OS; _ -> #srv{} end,
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

  WhiteLists_UPD = [ X || X <- WhiteLists_V, lists:member(X#source.name, [ Z#source.name || Z <- WhiteLists_C ]), srcChanged(X, checkSrcRec(lists:keyfind(X#source.name,2,WhiteLists_C))) ],

  Sources_UPD = [ X || X <- Sources_V, lists:member(X#source.name, [ Z#source.name || Z <- Sources_C ]), srcChanged(X, checkSrcRec(lists:keyfind(X#source.name,2,Sources_C))) ],

  [ ets:insert(cfg_table, {[source,X#source.name],X}) || X <- WhiteLists_N ++ Sources_N ++ WhiteLists_UPD ++ Sources_UPD ],

  [ ets:delete(cfg_table, [source,X#source.name]) || X <- WhiteLists_D ++ Sources_D ],
  [ ets:delete(rpz_hotcache_table, {X#source.name,Y}) || X <- WhiteLists_UPD ++ Sources_UPD ++ WhiteLists_D ++ Sources_D, Y <- [axfr,ixfr] ],


  RPZ_V= [ Z || Z <- [ validateCFGRPZ(Y,Sources_V,WhiteLists_V) || Y <- RPZ ], Z /= [] ],
  RPZ_D = [ X || X <- RPZ_C, not lists:member(X#rpz.zone, [ Z#rpz.zone || Z <- RPZ_V ]) ],
  RPZ_N = [ X || X <- RPZ_V, not lists:member(X#rpz.zone, [ Z#rpz.zone || Z <- RPZ_C ]) ],

%TODO TKEYS and Groups should be checked
  RPZ_UPD = [ X || X <- RPZ_V, not checkRPZEq(X,lists:keyfind(X#rpz.zone,3,RPZ_C)),lists:member(X#rpz.zone, [ Z#rpz.zone || Z <- RPZ_C ]) ] ++
            [ X || X <- RPZ_V, ioc2rpz_fun:intersection(X#rpz.whitelist,[Z#source.name || Z <- WhiteLists_UPD]) /= [] ] ++
            [ X || X <- RPZ_V, ioc2rpz_fun:intersection(X#rpz.sources,[Z#source.name || Z <- Sources_UPD]) /= [] ] ++
            %% Source-attribution (R2/R6): force an AXFR for any existing feed whose
            %% EFFECTIVE tracking state changed — either its own track_sources flag
            %% was edited, or it inherits the server global default which changed.
            %% checkRPZEq intentionally ignores track_sources, so this is the only
            %% path that rebuilds masks when tracking is toggled. Comparing the
            %% effective boolean (not the raw flag) avoids needless rebuilds when the
            %% result is unchanged (e.g. auto<->on both resolving to true).
            [ X || X <- RPZ_V, track_state_changed(X, RPZ_C, OldSrv, SrvV) ],


  [ ioc2rpz_fun:logMessage("Zone ~p was updated. Terminating ~p.~n",[X#rpz.zone_str,X#rpz.pid]) || X <- RPZ_UPD, X#rpz.status == updating ],
  [ ioc2rpz_fun:logMessage("Zone ~p was removed. Terminating ~p.~n",[X#rpz.zone_str,X#rpz.pid]) || X <- RPZ_D, X#rpz.status == updating ],
  [ exit(X#rpz.pid,rpzRemoved) || X <- RPZ_D, X#rpz.status == updating], %TODO 2025-01-11 replace by supervisor:terminate_child(SupervisorPid, X#rpz.pid). Where to get supervisor?
  [ exit(X#rpz.pid,rpzUpdated) || X <- RPZ_UPD, X#rpz.status == updating],

  %% Task 25: preserve runtime stats (counts/serial/timestamps) across reload by
  %% merging the pre-reload values from RPZ_C onto the freshly-parsed records.
  %% This is independent of the save_zone_info -> load_zone_info roundtrip (which
  %% only covers cached zones and zeroes non-cached/online ones). New zones have
  %% no match in RPZ_C and keep their parsed values; force-update zones keep the
  %% carried-over last-completed counts (status=forceAXFR signals they are stale).
  RPZ_V_M = [ merge_rpz_stats(X, RPZ_C) || X <- RPZ_V ],

  [ ets:delete(cfg_table, [rpz,X#rpz.zone]) || X <- RPZ_D ],
  [ ets:insert(cfg_table, {[rpz,X#rpz.zone],X#rpz.zone,X}) || X <- RPZ_V_M ],
  [ ets:match_delete(rpz_hotcache_table,{{pkthotcache,X#rpz.zone,'_'},'_','_'}) || X <- RPZ_D ++ RPZ_UPD ],

  ioc2rpz_db:clean_DB(RPZ_D), %Remove deleted zones 

  [ ets:update_element(cfg_table, [rpz,X#rpz.zone], [{3, (merge_rpz_stats(X, RPZ_C))#rpz{status=forceAXFR}}]) || X <- RPZ_UPD ], %forceaxfr

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

%% @doc Returns `true' if a freshly-parsed feed's EFFECTIVE source-tracking state
%% differs from what it was before the reload, so the feed must be force-rebuilt
%% (AXFR) to (re)derive per-source masks. Source masks are only (re)computed
%% during a full AXFR; IXFR never backfills masks for already-present indicators
%% (accepted limitation R6). `checkRPZEq/2' deliberately excludes `track_sources',
%% so this is the dedicated detector that covers BOTH a per-feed flag edit and an
%% inherited change from the server global default (`#srv.track_sources').
%%
%% The comparison is on the resolved boolean from {@link track_enabled/2} (old
%% record + old #srv vs new record + new #srv), not the raw flag, so toggling
%% between two values that resolve to the same effective state (e.g. `auto' and
%% `on' for a multi-source feed) does NOT trigger a needless rebuild. A brand-new
%% zone (no match in `OldList') returns `false' here — it is a fresh AXFR anyway
%% (handled via RPZ_N).
%%
%% @param New     The freshly-parsed `#rpz{}' record.
%% @param OldList The pre-reload snapshot of `#rpz{}' records (`RPZ_C').
%% @param OldSrv  The pre-reload `#srv{}' record (old global default).
%% @param NewSrv  The freshly-parsed `#srv{}' record (new global default).
%% @returns boolean() — `true' when the effective tracking state changed.
track_state_changed(New, OldList, OldSrv, NewSrv) ->
  case lists:keyfind(New#rpz.zone, #rpz.zone, OldList) of
    false -> false;
    Old when is_record(Old, rpz) ->
      track_enabled(Old, OldSrv) =/= track_enabled(New, NewSrv)
  end.

%% @doc Safe accessor for source records during reload diffing.
%%
%% Returns the record unchanged if it has a defined name; otherwise returns
%% an empty `#source{}' to avoid badmatch on `lists:keyfind' returning `false'.
checkSrcRec(Rec) when Rec#source.name /= undefined ->
	Rec;
checkSrcRec(_Rec) ->
	#source{}.

%% @doc Returns `true' if a source's data-affecting configuration changed and it
%% therefore has to be re-pulled / invalidated in the hot cache on reload.
%%
%% Compares the download URLs plus every field that changes how the feed is
%% parsed or limited: the extraction `regex', the `ioc_type', and `max_ioc'.
%% Previously only the AXFR/IXFR URLs were compared, so editing just the regex
%% (or ioc_type/max_ioc) of an existing source was silently ignored on reload.
%% Fields that do not affect the produced IOC set (cache timers, keep_in_cache)
%% are intentionally excluded.
%%
%% @param New  Freshly-parsed `#source{}' record from the config file.
%% @param Old  Currently-loaded `#source{}' record from `cfg_table'.
%% @returns `true' if any data-affecting field differs, `false' otherwise.
srcChanged(New, Old) ->
	New#source.axfr_url /= Old#source.axfr_url orelse
	New#source.ixfr_url /= Old#source.ixfr_url orelse
	New#source.regex    /= Old#source.regex orelse
	New#source.ioc_type /= Old#source.ioc_type orelse
	New#source.max_ioc  /= Old#source.max_ioc.

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
  SourceNames = [ Z#source.name || Z <- S ],
  WLNames = [ Z#source.name || Z <- W ],
  MissingSources = [ X || X <- RPZ#rpz.sources, not lists:member(X, SourceNames) ],
  MissingWL = [ X || X <- RPZ#rpz.whitelist, not lists:member(X, WLNames) ],
  SV = MissingSources == [],
  if not SV -> % only missing sources block loading; missing whitelists are logged for visibility
    ioc2rpz_fun:logMessage("RPZ ~p was not loaded. Missing sources: ~p. Missing whitelists: ~p.~n",[RPZ#rpz.zone_str,MissingSources,MissingWL]),
    [];
    true -> RPZ
  end.

%% @doc Merge runtime statistics fields from the pre-reload record onto a
%% freshly-parsed `#rpz{}' record (task 25).
%%
%% On a configuration reload the `#rpz{}' records are rebuilt from the config
%% file (counts/serial/timestamps come from `load_zone_info/1', which only
%% restores cached zones and returns zeroes for non-cached/online or
%% not-yet-persisted zones). To keep `/api/v1/stats/rpz' meaningful across a
%% reload, this carries the previous `ioc_count', `rule_count', `serial',
%% `serial_ixfr', `update_time', `ixfr_update_time', and `ixfr_nz_update_time'
%% from the matching old record onto the new one. The zone is matched by
%% `#rpz.zone'. A new zone (no match in `OldList') is returned unchanged so its
%% parsed/zeroed values stand until the scheduler populates them.
%%
%% @param New     The freshly-parsed `#rpz{}' record.
%% @param OldList The pre-reload snapshot of `#rpz{}' records (`RPZ_C').
%% @returns The `New' record with stats fields overridden from the old record,
%%          or `New' unchanged if no matching zone exists.
merge_rpz_stats(New, OldList) ->
  case lists:keyfind(New#rpz.zone, #rpz.zone, OldList) of
    false -> New;
    Old when is_record(Old, rpz) ->
      New#rpz{ioc_count           = Old#rpz.ioc_count,
              rule_count          = Old#rpz.rule_count,
              serial              = Old#rpz.serial,
              serial_ixfr         = Old#rpz.serial_ixfr,
              update_time         = Old#rpz.update_time,
              ixfr_update_time    = Old#rpz.ixfr_update_time,
              ixfr_nz_update_time = Old#rpz.ixfr_nz_update_time}
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

%% @doc Validate the server-level global `track_sources' default.
%%
%% Accepts `off | auto | on'. Any other value falls back to `off' (with a log
%% message) to stay backward compatible and never enable tracking unexpectedly.
%%
%% @param V  The raw value from the server config tuple.
%% @returns `off | auto | on'.
validate_track_sources(off)  -> off;
validate_track_sources(auto) -> auto;
validate_track_sources(on)   -> on;
validate_track_sources(V) ->
  ioc2rpz_fun:logMessage("Invalid server track_sources value ~p, defaulting to off~n", [V]),
  off.

%% @doc Validate the per-feed `track_sources' value.
%%
%% Accepts `auto | true | false'. Any other value falls back to `undefined'
%% (with a log message) so the feed inherits the server global default
%% (#srv.track_sources).
%%
%% @param V  The raw value from the extended rpz config tuple.
%% @returns `auto | true | false | undefined'.
validate_feed_track_sources(auto)  -> auto;
validate_feed_track_sources(true)  -> true;
validate_feed_track_sources(false) -> false;
validate_feed_track_sources(V) ->
  ioc2rpz_fun:logMessage("Invalid feed track_sources value ~p, defaulting to undefined (inherit global default)~n", [V]),
  undefined.

%% @doc Canonicalises a configured zone name to lower case (RFC 4343).
%%
%% Both `#rpz.zone' (the wire-format cfg_table/database key) and `#rpz.zone_str'
%% are derived from this value, and `ioc2rpz:rpz_zone/1' looks the zone up with
%% the lower-cased query name, so an upper-case name in the config file would
%% otherwise be unreachable. Non-list values are passed through untouched for
%% the config validator to report.
%%
%% @param Zone The zone name as written in the configuration file.
%% @returns The zone name in lower case.
-spec zone_name_lowcase(string()) -> string().
zone_name_lowcase(Zone) when is_list(Zone) ->
  binary_to_list(ioc2rpz_fun:bin_to_lowcase(list_to_binary(Zone)));

zone_name_lowcase(Zone) ->
  Zone.

%% @doc Resolve the effective source-attribution tracking flag for a zone.
%%
%% Implements the effective-state resolution from design §3.1b. Precedence:
%% the per-feed #rpz.track_sources value wins when set; otherwise the server
%% global default #srv.track_sources applies. The built-in default is `off'
%% (#srv.track_sources defaults to `off'), so an unconfigured feed on a server
%% with no global setting does not track.
%%
%% The resolved value is then mapped to a boolean:
%% <ul>
%%   <li>`off' | `false' ⇒ false (tracking disabled)</li>
%%   <li>`on'  | `true'  ⇒ true  (tracking forced on)</li>
%%   <li>`auto' ⇒ true only for multi-source feeds
%%       (`length(sources) > 1', R3)</li>
%% </ul>
%%
%% Finally the >63-source capacity decision (R7, design §8) is applied via
%% {@link capacity_ok/1}. As of task 14 the DEFAULT above the fixnum threshold is
%% to TRACK using a binary-bitmap mask (see {@link ioc2rpz_fun:mask_repr_for/1}),
%% so `capacity_ok/1' no longer force-disables large feeds; it returns `true'.
%% The result is therefore `Tracked andalso capacity_ok(Zone)'.
%%
%% @param Zone  A #rpz{} record (per-feed track_sources + sources list).
%% @param Srv   A #srv{} record (server global track_sources default).
%% @returns boolean() — whether source attribution is effectively enabled.
track_enabled(Zone, Srv) ->
  case Zone#rpz.track_sources of
    undefined -> Eff = Srv#srv.track_sources;
    V         -> Eff = V
  end,
  Tracked = case Eff of
    off   -> false;
    false -> false;
    on    -> true;
    true  -> true;
    auto  -> length(Zone#rpz.sources) > 1
  end,
  Tracked andalso capacity_ok(Zone).

%% @doc Capacity decision for positional source masks (design §8, R7).
%%
%% A per-zone positional mask is a single Erlang integer (fixnum) while the
%% number of sources stays within `?MaskFixnumBits' (63). Those feeds always
%% pass. As of task 14, feeds ABOVE the fixnum threshold are no longer
%% force-disabled: the default is to keep TRACKING them using a binary-bitmap
%% mask representation (see {@link ioc2rpz_fun:mask_repr_for/1} and the
%% ioc2rpz_fun mask abstraction), which costs ~ceil(N/8) bytes per indicator
%% instead of a growing bignum. This helper therefore returns `true' for both
%% cases and delegates the oversize decision to {@link capacity_over_threshold_ok/1}.
%%
%% @param Zone  A #rpz{} record.
%% @returns boolean() — whether tracking is permitted for this feed's source count.
capacity_ok(Zone) ->
  case length(Zone#rpz.sources) =< ?MaskFixnumBits of
    true  -> true;
    false -> capacity_over_threshold_ok(Zone)
  end.

%% @doc Decide whether a feed with MORE than `?MaskFixnumBits' sources is tracked
%% (design §8, R7). The default is `true' (track via a binary-bitmap mask, with a
%% logged warning emitted on the build path — observability task 16). The
%% configurable FALLBACK is to disable tracking for such feeds and store
%% `SrcMask = 0'.
%%
%% HOOK (configurable disable): the fallback would be wired to a server/per-feed
%% config flag (e.g. a `#srv.track_sources_oversize = bitmap | disable' setting,
%% or a per-feed variant). When that flag resolves to `disable' this function
%% should return `false'. The config plumbing for that flag is intentionally left
%% out here to keep this change minimal; the default (bitmap/track) is what
%% design §8 specifies, and the integer mask stays correct for any index in the
%% meantime (Erlang integers are arbitrary precision).
%%
%% @param Zone  A #rpz{} record with more than `?MaskFixnumBits' sources.
%% @returns boolean() — `true' (track, default) unless a disable fallback is set.
capacity_over_threshold_ok(_Zone) ->
  true.

%% @doc Build the 0-based index/source-name list for a zone.
%%
%% Each source's position in `Zone#rpz.sources' is the bit index used by the
%% positional source mask (bit `i' ⇒ the `i'-th source). This helper pairs each
%% source name with its 0-based index, e.g. for `["a","b","c"]' it returns
%% `[{0,"a"},{1,"b"},{2,"c"}]'. Used by the build path to tag indicators with
%% `1 bsl Index' and by the API to resolve mask bits back to source names.
%%
%% @param Zone  A #rpz{} record whose `sources' list is indexed.
%% @returns `[{Index :: non_neg_integer(), SourceName :: term()}]'.
source_index_list(Zone) ->
  Sources = Zone#rpz.sources,
  N = length(Sources),
  lists:zip(lists:seq(0, N - 1), Sources).

%% @doc Read the persisted source-list signature from the zone's IXFR cfg row.
%%
%% Source masks are per-zone positional (bit `i' = `i'-th entry of
%% `#rpz.sources'), so the ordered source-name signature is stored alongside the
%% zone's IXFR config (see {@link ioc2rpz_db:save_zone_info/1}). This helper
%% returns the stored signature binary when present, or `undefined' when there
%% is no cfg row or the row is a legacy 5-field row without a signature
%% (pre-upgrade cached zone — task 15). Only cached zones populate
%% `rpz_ixfr_table', so non-cached zones return `undefined'.
%%
%% @param Zone  An `#rpz{}' record with at least `zone' populated.
%% @returns the stored signature `binary()' or `undefined'.
load_source_signature(Zone) ->
  case ioc2rpz_db:get_zone_info(Zone,ixfr) of
    [[_,_Serial,_Serial_IXFR,_IXFR_Update_time,_NZ_Update_Time,SrcSig]] -> SrcSig;
    _ -> undefined
  end.

%% @doc Decide whether the configured source list differs from the one that
%% produced the cached masks (design §7.2, R6).
%%
%% Compares the freshly-configured, ordered source names against the persisted
%% signature. A missing/`undefined' stored signature (legacy pre-upgrade row, or
%% no row) is treated as a mismatch so exactly one AXFR rebuild re-derives the
%% masks (design §12, task 15). Otherwise the signatures are compared for
%% equality; any add/remove/reorder of sources changes the signature.
%%
%% @param Sources  The configured, ordered source list (`#rpz.sources').
%% @param StoredSig  The persisted signature `binary()' or `undefined'.
%% @returns boolean() — `true' when a rebuild is required.
source_list_changed(_Sources, undefined) -> true;
source_list_changed(Sources, StoredSig) ->
  ioc2rpz_db:source_signature(Sources) =/= StoredSig.

%% @doc Override a loaded zone status to `forceAXFR' when its source list
%% changed since the cached masks were built (design §7.2, R6).
%%
%% Only cached zones (`Cache == "true"') store masks and read the IXFR cfg row,
%% so only they can detect a source-list change. When such a zone would
%% otherwise load as `ready' but the configured source list no longer matches
%% the persisted signature (including a legacy row with no signature, treated as
%% unknown ⇒ one rebuild), the status is overridden to `forceAXFR' and the
%% change is logged. In every other case the status is returned unchanged: a
%% `notready' zone AXFRs anyway, and non-cached zones don't store masks.
%%
%% @param Cache    The zone `cache' config value (`"true"' | `"false"').
%% @param Status   The status derived from the loaded zone info.
%% @param Zone     An `#rpz{}' record (used to read the stored signature/log).
%% @param Sources  The configured, ordered source list.
%% @returns the (possibly overridden) status atom.
maybe_force_source_axfr("true", ready, Zone, Sources) ->
  case source_list_changed(Sources, load_source_signature(Zone)) of
    true ->
      ioc2rpz_fun:logMessage("Zone ~p source list changed; forcing AXFR rebuild to re-derive source masks~n",[Zone#rpz.zone_str]),
      forceAXFR;
    false ->
      ready
  end;
maybe_force_source_axfr(_Cache, Status, _Zone, _Sources) ->
  Status.

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
    %% New 6-field row: trailing source-list signature (_SrcSig) is ignored
    %% here; task 3.2 compares it against the configured sources to force AXFR.
    [[_,Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time,_SrcSig]] when (IXFR_Update_time+Zone#rpz.ixfr_time)>CTime ->
      ioc2rpz_fun:logMessage("Get IXFR zone ~p serial ~p status ready ~n",[Zone#rpz.zone_str,Serial_IXFR]),
      [ready,Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time];
    [[_,Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time,_SrcSig]] when Zone#rpz.cache == "true"  ->
      ioc2rpz_fun:logMessage("Get IXFR zone ~p serial ~p status notready ~n",[Zone#rpz.zone_str,Serial_IXFR]),
      [notready,Serial,Serial_IXFR,IXFR_Update_time,NZ_Update_Time];
    %% Legacy 5-field row (pre-upgrade cached zones). Task 15 covers full
    %% migration tolerance; keep loading working here.
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

%% @doc Atomically claim an RPZ zone for updating, preventing duplicate
%% concurrent updates (race-condition fix, task 27 / issue 1.19).
%%
%% Uses `ets:select_replace/2' (an atomic compare-and-swap) to flip the zone's
%% `status' field to `updating' and record the caller as the owning `pid', but
%% only when the zone is not already being updated:
%% <ul>
%%   <li>If `status' is not `updating' (e.g. `ready'/`forceAXFR'), it is claimed
%%       atomically.</li>
%%   <li>If `status' is `updating' but the recorded pid is dead (a stale/leaked
%%       update), the entry is atomically reclaimed (matched on the exact dead
%%       pid so a concurrent claimer cannot double-claim).</li>
%%   <li>If `status' is `updating' with a live pid, the claim fails.</li>
%% </ul>
%%
%% Called at the start of {@link update_zone_full/1} and {@link update_zone_inc/1}
%% so every spawn path (periodic `update_all_zones', forced updates, REST/DNS
%% management) is de-duplicated at a single authoritative point.
%%
%% @param ZoneBin The zone name in DNS wire format (`#rpz.zone').
%% @returns `true' if the zone was claimed by this process, `false' otherwise.
claim_zone_for_update(ZoneBin) ->
  Key = [rpz, ZoneBin],
  case ets:lookup(cfg_table, Key) of
    [{Key, _ZBin, R}] ->
      %% A zone is claimable unless it is already 'updating' with a live owner.
      Claimable = (R#rpz.status /= updating) orelse (not my_process_is_alive(R#rpz.pid)),
      case Claimable of
        false ->
          false;
        true ->
          NewR = R#rpz{status = updating, pid = self()},
          %% Optimistic compare-and-swap: replace the record only if it is still
          %% byte-for-byte what we just read ('$3' == R). If a concurrent process
          %% changed it (e.g. claimed it first), select_replace returns 0 and this
          %% claim fails — guaranteeing only one updater proceeds.
          MS = [{ {'$1', '$2', '$3'},
                  [{'==', '$1', {const, Key}}, {'==', '$3', {const, R}}],
                  [{{'$1', '$2', {const, NewR}}}] }],
          ets:select_replace(cfg_table, MS) == 1
      end;
    _ ->
      false
  end.

%% @doc Compute a fingerprint of the configured TLS certificate files.
%% Returns `undefined' when no certificate is configured.
cert_files() ->
  case ets:match(cfg_table,{srv,'_','_','_','_','$6','_'}) of
    [[Cert]] when Cert /= [], Cert /= undefined ->
      [ F || F <- [Cert#cert.certfile, Cert#cert.keyfile, Cert#cert.cacertfile], F /= undefined, F /= [] ];
    _ ->
      []
  end.

cert_files_hash([]) ->
  undefined;
cert_files_hash(Files) ->
  crypto:hash(sha256, [ case file:read_file(F) of {ok,B} -> B; _ -> <<>> end || F <- Files ]).

cert_files_readable(Files) ->
  lists:all(fun(F) -> filelib:is_regular(F) end, Files).

%% @doc Store the current certificate fingerprint as a baseline (no restart).
store_cert_hash() ->
  case cert_files() of
    [] -> ok;
    Files -> ets:insert(cfg_table, {cert_files_hash, cert_files_hash(Files)}), ok
  end.

%% @doc On config reload, detect a changed certificate and restart the TLS
%% listeners so renewed certificates are picked up without a full restart
%% (task 34 / issue 1.10).
maybe_reload_cert() ->
  case cert_files() of
    [] -> ok;
    Files ->
      case cert_files_readable(Files) of
        false ->
          ioc2rpz_fun:logMessage("TLS certificate files missing or unreadable on reload; keeping current listeners~n", []);
        true ->
          NewHash = cert_files_hash(Files),
          OldHash = case ets:lookup(cfg_table, cert_files_hash) of
                      [{cert_files_hash, H}] -> H;
                      _ -> undefined
                    end,
          if NewHash /= OldHash ->
              ets:insert(cfg_table, {cert_files_hash, NewHash}),
              case OldHash of
                undefined -> ok; %first observation: just record the baseline
                _ ->
                  ioc2rpz_fun:logMessage("TLS certificate files changed; restarting TLS listeners~n", []),
                  restart_tls_listeners()
              end;
            true ->
              ok
          end
      end
  end.

%% @doc Terminate and restart the DoT and REST HTTPS listener supervisors so
%% they re-read the certificate from disk.
restart_tls_listeners() ->
  lists:foreach(fun(Child) ->
    case supervisor:terminate_child(?MODULE, Child) of
      ok ->
        case supervisor:restart_child(?MODULE, Child) of
          {ok, _}    -> ioc2rpz_fun:logMessage("Restarted ~p with the new certificate~n", [Child]);
          {ok, _, _} -> ioc2rpz_fun:logMessage("Restarted ~p with the new certificate~n", [Child]);
          {error, R} -> ioc2rpz_fun:logMessage("Failed to restart ~p after certificate change: ~p~n", [Child, R])
        end;
      {error, not_found} -> ok; %listener not configured (e.g. no cert at startup)
      {error, R}         -> ioc2rpz_fun:logMessage("Could not terminate ~p for certificate reload: ~p~n", [Child, R])
    end
  end, [ioc2rpz_tls_sup_v6, ioc2rpz_rest_tls_sup_v6]).

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
  case claim_zone_for_update(Zone#rpz.zone) of
    false ->
      ioc2rpz_fun:logMessage("Zone ~p is already being updated by a live process; skipping duplicate full update~n",[Zone#rpz.zone_str]),
      ok;
    true ->
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
      ioc2rpz_db:delete_old_db_pkt(Zone#rpz{serial=CTime}),
      %erlang:garbage_collect(), %TODO check if need
      ioc2rpz:send_notify(Zone),
      ioc2rpz_fun:logMessage("Zone ~p updated in ~p seconds, new serial ~p, ~p rules, ~p indicators.~n",[Zone#rpz.zone_str, (ioc2rpz_fun:curr_serial_60() - CTime), CTime, NRules, NIOCs])
  end,
  ioc2rpz_db:saveZones(),
  ok
  end.


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
  case claim_zone_for_update(Zone#rpz.zone) of
    false ->
      ioc2rpz_fun:logMessage("Zone ~p is already being updated by a live process; skipping duplicate incremental update~n",[Zone#rpz.zone_str]),
      ok;
    true ->
  %io:fwrite(group_leader(),"Zone ~p IOC  ~p ~n",[Zone#rpz.zone_str,IOC]),
  Pid=self(),
	ioc2rpz_fun:logMessage("Process PID ~p incremental update ~p started ~n",[Pid, Zone#rpz.zone_str]),
  NRbefore=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3','_'},[],['true']}]),
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
          NRafter=ets:select_count(rpz_ixfr_table,[{{{ioc,Zone#rpz.zone,'$1','_'},'$2','$3','_'},[],['true']}]),
          ioc2rpz_fun:logMessage("Zone ~p records before ~p after ~p. ~n",[Zone#rpz.zone_str, NRbefore, NRafter]),
          ets:update_element(cfg_table, [rpz,Zone#rpz.zone], [{3, Zone#rpz{status=ready, serial=CTime, ixfr_update_time=CTime, ixfr_nz_update_time=CTime, pid=undefined, ioc_count=NIOCs, rule_count=NRules}}]),
          ioc2rpz_db:delete_old_db_pkt(Zone#rpz{serial=CTime}),
          ioc2rpz_db:saveZones(),
          ioc2rpz:send_notify(Zone);
        {Error,Msg} ->
          ioc2rpz_fun:logMessage("Error ~p while updating ~p. Message: ~p~n",[Error,Zone#rpz.zone_str,Msg])
      end
  end,
	ioc2rpz_fun:logMessage("Process PID ~p incremental update ~p finished in ~p seconds ~n",[Pid, Zone#rpz.zone_str, (ioc2rpz_fun:curr_serial_60()-CTime)]),
	ok
  end.

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

%%%%
%%%% EUnit tests
%%%%

%% Verifies the atomic claim_zone_for_update/1 used to prevent duplicate
%% concurrent zone updates (task 27 / issue 1.19).
claim_zone_for_update_test() ->
  catch ets:delete(cfg_table),
  ets:new(cfg_table, [ordered_set, public, named_table]),
  Z = <<4,"test",3,"rpz",0>>,
  R = #rpz{zone=Z, zone_str="test.rpz", status=ready, pid=undefined},
  ets:insert(cfg_table, {[rpz,Z], Z, R}),
  %% 1) a 'ready' zone is claimed; status->updating, pid->self()
  C1 = claim_zone_for_update(Z),
  [{[rpz,Z],Z,R1}] = ets:lookup(cfg_table,[rpz,Z]),
  %% 2) a second claim fails — zone is 'updating' with a live pid (self())
  C2 = claim_zone_for_update(Z),
  %% 3) a zone stuck 'updating' with a dead pid is reclaimable
  DeadPid = spawn(fun() -> ok end),
  timer:sleep(20),
  ets:insert(cfg_table, {[rpz,Z], Z, R#rpz{status=updating, pid=DeadPid}}),
  C3 = claim_zone_for_update(Z),
  %% 4) an unknown zone cannot be claimed
  C4 = claim_zone_for_update(<<5,"bogus">>),
  ets:delete(cfg_table),
  [ ?assert(C1 =:= true),
    ?assert(R1#rpz.status =:= updating),
    ?assert(R1#rpz.pid =:= self()),
    ?assert(C2 =:= false),
    ?assert(C3 =:= true),
    ?assert(C4 =:= false) ].

%% Verifies merge_rpz_stats/2 (task 25): runtime counts/serial/timestamps from
%% the pre-reload record are carried onto the freshly-parsed record, matched by
%% zone; a new zone (no match) is returned unchanged.
merge_rpz_stats_test() ->
  Z = <<4,"test",3,"rpz",0>>,
  %% Old (pre-reload) record carries real runtime stats.
  Old = #rpz{zone=Z, zone_str="test.rpz", status=ready,
             ioc_count=1000, rule_count=1500, serial=42, serial_ixfr=43,
             update_time=100, ixfr_update_time=110, ixfr_nz_update_time=105},
  %% New (freshly-parsed) record has zeroed stats (as load_zone_info would yield
  %% for a non-cached zone) but a different status.
  New = #rpz{zone=Z, zone_str="test.rpz", status=forceAXFR,
             ioc_count=0, rule_count=0, serial=0, serial_ixfr=0,
             update_time=0, ixfr_update_time=0, ixfr_nz_update_time=0},
  Merged = merge_rpz_stats(New, [Old]),
  %% A zone with no match in the old list is returned unchanged.
  Znew = <<3,"new",3,"rpz",0>>,
  NewOnly = New#rpz{zone=Znew},
  Unchanged = merge_rpz_stats(NewOnly, [Old]),
  [ %% stats fields taken from Old
    ?assert(Merged#rpz.ioc_count =:= 1000),
    ?assert(Merged#rpz.rule_count =:= 1500),
    ?assert(Merged#rpz.serial =:= 42),
    ?assert(Merged#rpz.serial_ixfr =:= 43),
    ?assert(Merged#rpz.update_time =:= 100),
    ?assert(Merged#rpz.ixfr_update_time =:= 110),
    ?assert(Merged#rpz.ixfr_nz_update_time =:= 105),
    %% non-stats fields preserved from New (status is NOT overridden by the merge)
    ?assert(Merged#rpz.status =:= forceAXFR),
    %% unmatched zone returned unchanged (zeroed stats kept)
    ?assert(Unchanged#rpz.ioc_count =:= 0),
    ?assert(Unchanged#rpz.serial =:= 0) ].

%% Verifies track_enabled/2 effective-state resolution (design §3.1b, R2/R3/R7):
%% per-feed value precedence, inheritance of the server global default when the
%% feed is `undefined', `auto' being multi-source only, and the >63-source
%% capacity force-disable.
track_enabled_test() ->
  Srv_off  = #srv{track_sources=off},
  Srv_on   = #srv{track_sources=on},
  Srv_auto = #srv{track_sources=auto},
  Multi  = ["s0","s1","s2"],
  Single = ["s0"],
  %% >63 sources (64 sources) to exercise the capacity check.
  Big = [lists:flatten(io_lib:format("s~p",[I])) || I <- lists:seq(0,63)],
  %% Per-feed value wins regardless of the server default.
  ZfeedTrue  = #rpz{sources=Single, track_sources=true},
  ZfeedFalse = #rpz{sources=Multi,  track_sources=false},
  ZfeedAutoM = #rpz{sources=Multi,  track_sources=auto},
  ZfeedAutoS = #rpz{sources=Single, track_sources=auto},
  %% undefined ⇒ inherit the server global default.
  ZinhMulti  = #rpz{sources=Multi,  track_sources=undefined},
  ZinhSingle = #rpz{sources=Single, track_sources=undefined},
  %% >63 sources (Big has 64): as of task 14 these are TRACKED BY DEFAULT using a
  %% binary-bitmap mask (design §8), no longer force-disabled by capacity_ok/1.
  ZbigTrue   = #rpz{sources=Big, track_sources=true},
  ZbigAuto   = #rpz{sources=Big, track_sources=auto},
  [ %% per-feed forced true/false
    ?assert(track_enabled(ZfeedTrue,  Srv_off) =:= true),
    ?assert(track_enabled(ZfeedFalse, Srv_on)  =:= false),
    %% per-feed auto: multi-source true, single-source false
    ?assert(track_enabled(ZfeedAutoM, Srv_off) =:= true),
    ?assert(track_enabled(ZfeedAutoS, Srv_on)  =:= false),
    %% undefined inherits global off ⇒ false regardless of source count
    ?assert(track_enabled(ZinhMulti,  Srv_off) =:= false),
    ?assert(track_enabled(ZinhSingle, Srv_off) =:= false),
    %% undefined inherits global on ⇒ true (even single-source)
    ?assert(track_enabled(ZinhSingle, Srv_on)  =:= true),
    ?assert(track_enabled(ZinhMulti,  Srv_on)  =:= true),
    %% undefined inherits global auto ⇒ multi-source only
    ?assert(track_enabled(ZinhMulti,  Srv_auto) =:= true),
    ?assert(track_enabled(ZinhSingle, Srv_auto) =:= false),
    %% >63 sources are now TRACKED BY DEFAULT (task 14, design §8): a binary
    %% bitmap mask covers larger feeds, so forced-true and auto-multi both
    %% resolve to true rather than being force-disabled by the capacity check.
    ?assert(track_enabled(ZbigTrue, Srv_off) =:= true),
    ?assert(track_enabled(ZbigAuto, Srv_off) =:= true) ].

%% Verifies track_state_changed/4 (reload forced-AXFR on tracking toggle): a feed
%% whose EFFECTIVE tracking state changes — via its own flag or an inherited
%% change to the server global default — is detected so masks are rebuilt; a
%% no-op toggle (same effective result) and a brand-new zone are NOT flagged.
track_state_changed_test() ->
  Z = <<4,"test",3,"rpz",0>>,
  Multi  = ["s0","s1"],
  SrvOff  = #srv{track_sources=off},
  SrvAuto = #srv{track_sources=auto},
  %% per-feed flag edited off(inherit)->on
  OldInherit = #rpz{zone=Z, sources=Multi, track_sources=undefined},
  NewOn      = #rpz{zone=Z, sources=Multi, track_sources=true},
  %% global default changed off->auto for an inheriting multi-source feed
  %% no effective change: auto (multi ⇒ true) vs forced true
  NewAuto    = #rpz{zone=Z, sources=Multi, track_sources=auto},
  [ %% per-feed off->on under an unchanged (off) global ⇒ changed
    ?assert(track_state_changed(NewOn, [OldInherit], SrvOff, SrvOff) =:= true),
    %% inherited off->auto (multi-source ⇒ effective true) ⇒ changed
    ?assert(track_state_changed(OldInherit, [OldInherit], SrvOff, SrvAuto) =:= true),
    %% auto (multi ⇒ true) -> forced true: same effective state ⇒ NOT changed
    ?assert(track_state_changed(NewOn, [NewAuto], SrvOff, SrvOff) =:= false),
    %% no change at all ⇒ false
    ?assert(track_state_changed(OldInherit, [OldInherit], SrvOff, SrvOff) =:= false),
    %% brand-new zone (no match in the old list) ⇒ false (fresh AXFR anyway)
    ?assert(track_state_changed(NewOn, [], SrvOff, SrvOff) =:= false) ].

%% Verifies source_index_list/1 produces 0-based {Index, SourceName} pairs.
source_index_list_test() ->
  Zempty  = #rpz{sources=[]},
  Zsingle = #rpz{sources=["only"]},
  Zmulti  = #rpz{sources=["a","b","c"]},
  [ ?assert(source_index_list(Zempty)  =:= []),
    ?assert(source_index_list(Zsingle) =:= [{0,"only"}]),
    ?assert(source_index_list(Zmulti)  =:= [{0,"a"},{1,"b"},{2,"c"}]) ].

%% Verifies source_list_changed/2 (task 3.2, R6): a matching stored signature
%% ⇒ no change (false); a differing signature ⇒ change (true); a missing
%% (`undefined') stored signature (legacy pre-upgrade row / no row) ⇒ treated as
%% a change (true) so exactly one AXFR rebuild re-derives masks.
source_list_changed_test() ->
  Sources    = [<<"abuse-ch">>, <<"internal-list">>],
  MatchSig   = ioc2rpz_db:source_signature(Sources),
  ReorderSig = ioc2rpz_db:source_signature([<<"internal-list">>, <<"abuse-ch">>]),
  OtherSig   = ioc2rpz_db:source_signature([<<"abuse-ch">>]),
  [ %% matching signature ⇒ no change
    ?assert(source_list_changed(Sources, MatchSig)   =:= false),
    %% reordered/different membership ⇒ change (positional masks invalidated)
    ?assert(source_list_changed(Sources, ReorderSig) =:= true),
    ?assert(source_list_changed(Sources, OtherSig)   =:= true),
    %% missing/undefined stored signature ⇒ change (force one rebuild)
    ?assert(source_list_changed(Sources, undefined)  =:= true) ].
