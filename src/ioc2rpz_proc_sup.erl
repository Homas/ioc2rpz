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

%IOC2RPZ TCP Supervisor
-module(ioc2rpz_proc_sup).
-behaviour(supervisor).
-include_lib("kernel/include/file.hrl").
-include_lib("ioc2rpz.hrl").
-export([start_ioc2rpz_proc_sup/1,stop_ioc2rpz_proc_sup/0, start_socket/1, empty_listeners/1]).
-export([init/1]).

start_ioc2rpz_proc_sup([Proc,IP,Proto]) ->
	supervisor:start_link({local, Proc}, ?MODULE, [Proc,IP,Proto]). %list_to_atom(atom_to_list(?MODULE) ++ atom_to_list(Proto))

stop_ioc2rpz_proc_sup() ->
  ioc2rpz_fun:logMessage("ioc2rpz tcp recieved stop message ~n", []),
  ioc2rpz_fun:logMessage("ioc2rpz tcp is terminating ~n", []),
  gen_server:stop(?MODULE).

init([Proc,IPStr,Proto]) when Proc == tcp_sup; Proc == tcp6_sup -> %DNS TCP
  Pid=self(),
  {ok, TCPSocket} = open_tcp_sockets(IPStr, Proto) ,
	spawn_opt(ioc2rpz_proc_sup,empty_listeners,[Proc],[link,{fullsweep_after,0}]),
  ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [Proc]),
  {ok, {{simple_one_for_one, 60, 3600}, [{ioc2rpz, {ioc2rpz, start_ioc2rpz, [TCPSocket, [Pid,Proc,no]]}, temporary, 1000, worker, [ioc2rpz]}]}};


init([Proc,IPStr,Proto]) when Proc == udp_sup; Proc == udp6_sup -> %DNS UDP
  ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [udp_sup]),
  {ok, {{one_for_one, 60, 3600}, [{ioc2rpz, {ioc2rpz_udp, start_ioc2rpz_udp, [IPStr, [Proto]]}, temporary, 1000, worker, [ioc2rpz_udp]}]}};


init([Proc,IPStr,Proto]) when Proc == tls_sup; Proc == tls6_sup -> %DoT
  Pid=self(),
  {ok, TLSSocket} = open_tls_sockets(IPStr, Proto) ,
	spawn_opt(ioc2rpz_proc_sup,empty_listeners,[Proc],[link,{fullsweep_after,0}]),
  ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [Proc]),
  {ok, {{simple_one_for_one, 60, 3600}, [{ioc2rpz, {ioc2rpz, start_ioc2rpz, [TLSSocket, [Pid,Proc,yes]]}, temporary, 1000, worker, [ioc2rpz]}]}};


init([Proc,_IPStr,_Proto]) when Proc == rest_tls_sup; Proc == rest_tls6_sup -> %REST
	[[Cert]] = ets:match(cfg_table,{srv,'_','_','_','_','$6','_'}),
	Ciphers=ioc2rpz_fun:get_cipher_suites(?TLSVersion),
	Dispatch = cowboy_router:compile([{'_', [
				{"/", ioc2rpz_rest, [root]},
				{"/api/[:api_ver]/stats/serv", ioc2rpz_rest, [stats_serv]},
				{"/api/[:api_ver]/stats/rpz", ioc2rpz_rest, [stats_rpz]},
				{"/api/[:api_ver]/stats/source", ioc2rpz_rest, [stats_source]},
				{"/api/[:api_ver]/update/all_rpz", ioc2rpz_rest, [update_all_rpz]},
				{"/api/[:api_ver]/update/:rpz", ioc2rpz_rest, [update_rpz]},
				{"/api/[:api_ver]/mgmt/reload_cfg", ioc2rpz_rest, [reload_cfg]},
				{"/api/[:api_ver]/mgmt/update_tkeys", ioc2rpz_rest, [update_tkeys]},
				{"/api/[:api_ver]/cache/sources/clear/all", ioc2rpz_rest, [cache_sources_clear_all]},
				{"/api/[:api_ver]/cache/sources/clear/:source", ioc2rpz_rest, [cache_sources_clear_one]},
				{"/api/[:api_ver]/cache/sources/load/all", ioc2rpz_rest, [cache_sources_load_all]},
				%%%refresh sources (clear cache and load sources)
				%%%refresh all RPZ
				{"/api/[:api_ver]/mgmt/terminate", ioc2rpz_rest, [terminate]},
				{"/api/[:api_ver]/feed/:rpz", ioc2rpz_rest, [get_rpz]}, %pull feed
				{"/api/[:api_ver]/ioc/:ioc", ioc2rpz_rest, [get_ioc]}, %check ioc
				{'_', ioc2rpz_rest, [catch_all]}
					]}]),
	{ok, _} = cowboy:start_tls(https, [{port, ?PortREST},{certfile, Cert#cert.certfile}, {keyfile, Cert#cert.keyfile}, {ciphers, Ciphers}], #{env => #{dispatch => Dispatch}}),
	%{cacertfile, Cert#cert.cacertfile},
  ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [Proc]),
  {ok, {{one_for_one, 10, 10}, []}};

init([Proc,_IPStr,_Proto]) when Proc == doh_sup; Proc == doh6_sup -> %DoH
	[[Cert]] = ets:match(cfg_table,{srv,'_','_','_','_','$6','_'}),
	Ciphers=ioc2rpz_fun:get_cipher_suites(?TLSVersion),
	Dispatch = cowboy_router:compile([{'_', [
				{"/", ioc2rpz_doh, [root]},
				{"/dns-query", ioc2rpz_doh, [dns_query]},
				{'_', ioc2rpz_doh, [catch_all]}
					]}]),
	{ok, _} = cowboy:start_tls(doh, [{port, ?PortDoH},{certfile, Cert#cert.certfile}, {keyfile, Cert#cert.keyfile}, {ciphers, Ciphers}], #{env => #{dispatch => Dispatch}}),
	%{cacertfile, Cert#cert.cacertfile},
  ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [Proc]),
  {ok, {{one_for_one, 10, 10}, []}}.

open_tcp_sockets(IPStr,Proto) when IPStr /= "", IPStr /= [] ->
  {ok,IP}=inet:parse_address(IPStr),
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{ip, IP},{active,once}, binary, Proto]),
  {ok, TCPSocket};

open_tcp_sockets(_IPStr,Proto) ->
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{active,once}, binary, Proto]),  %{ipv6_v6only,true}
  {ok, TCPSocket}.


open_tls_sockets(IPStr,Proto) when IPStr /= "", IPStr /= [] ->
  {ok,IP}=inet:parse_address(IPStr),
	[[Cert]] = ets:match(cfg_table,{srv,'_','_','_','_','$6','_'}),
	Ciphers=ioc2rpz_fun:get_cipher_suites(?TLSVersion),
	{ok, TLSSocket} = ssl:listen(?PortTLS, [{ip, IP},{active,once}, binary, Proto, {certfile, Cert#cert.certfile}, {keyfile, Cert#cert.keyfile}, {ciphers, Ciphers} ]), %,{cacertfile, Cert#cert.cacertfile}
  {ok, TLSSocket};

open_tls_sockets(_IPStr,Proto) ->
	[[Cert]] = ets:match(cfg_table,{srv,'_','_','_','_','$6','_'}),
	Ciphers=ioc2rpz_fun:get_cipher_suites(?TLSVersion),
	{ok, TLSSocket} = ssl:listen(?PortTLS, [{active,once}, binary, Proto, {certfile, Cert#cert.certfile}, {keyfile, Cert#cert.keyfile}, {ciphers, Ciphers}]), %,{cacertfile, Cert#cert.cacertfile}
  {ok, TLSSocket}.

start_socket(Proc) ->
  supervisor:start_child(Proc, []).

empty_listeners(Proc) ->
  [start_socket(Proc) || _ <- lists:seq(1,5)],
  ok.
