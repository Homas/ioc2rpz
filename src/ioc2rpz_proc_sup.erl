%Copyright 2017-2019 Vadim Pavlov ioc2rpz[at]gmail[.]com
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
	
init([Proc,IPStr,Proto]) when Proc == tcp_sup; Proc == tcp6_sup ->
  Pid=self(),
  {ok, TCPSocket} = open_sockets(IPStr, Proto) ,
	spawn_opt(ioc2rpz_proc_sup,empty_listeners,[Proc],[link,{fullsweep_after,0}]),
  ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [Proc]),
  {ok, {{simple_one_for_one, 60, 3600}, [{ioc2rpz, {ioc2rpz, start_ioc2rpz, [TCPSocket, [Pid,Proc]]}, temporary, 1000, worker, [ioc2rpz]}]}};
		

init([Proc,IPStr,Proto]) when Proc == udp_sup; Proc == udp6_sup ->
  Pid=self(),
  ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [udp_sup]),
	
	%ioc2rpz_udp:start_ioc2rpz_udp(IPStr, [inet6]),
	
  {ok, {{one_for_one, 60, 3600}, [{ioc2rpz, {ioc2rpz_udp, start_ioc2rpz_udp, [IPStr, [Proto]]}, temporary, 1000, worker, [ioc2rpz_udp]}]}}.


open_sockets(IPStr,Proto) when IPStr /= "", IPStr /= [] ->
  {ok,IP}=inet:parse_address(IPStr),
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{ip, IP},{active,once}, binary, Proto]),
  {ok, TCPSocket};

open_sockets(IPStr,Proto) ->
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{active,once}, binary, Proto]),  %{ipv6_v6only,true}
  {ok, TCPSocket}.
  

start_socket(Proc) ->
  supervisor:start_child(Proc, []).

empty_listeners(Proc) ->
  [start_socket(Proc) || _ <- lists:seq(1,5)],
  ok.