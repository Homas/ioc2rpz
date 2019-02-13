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

%IOC2RPZ UDP Worker

-module(ioc2rpz_udp).
-behaviour(gen_server).

-include_lib("ioc2rpz.hrl").

-export([start_ioc2rpz_udp/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_ioc2rpz_udp(IP,Params) ->
  gen_server:start_link(?MODULE, [IP,Params], []).

init([IP,[IPver|_]=Params]) when IP /=""->
  {ok, UDPSocket} = gen_udp:open(53, [{ip, IP},IPver,binary, {active, true},{read_packets, 100},{recbuf, 65535}]),
  {ok, #state{socket=UDPSocket, params=Params}};

init([_IP,[IPver|_]=Params]) ->
  {ok, UDPSocket} = gen_udp:open(53, [IPver,binary, {active, true},{read_packets, 100},{recbuf, 65535}]),
  {ok, #state{socket=UDPSocket, params=Params}}.


handle_cast(shutdown, State) ->
%    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, RIP, RPort, Pkt}, State) ->
    spawn(ioc2rpz,parse_dns_request,[Socket, Pkt, #proto{proto=udp, rip=RIP, rport=RPort}]),
    %ioc2rpz_fun:logMessage("get udp ~p ~p ~p ~n",[RIP, RPort, Pkt]),
    {noreply, State};

handle_info(E, State) ->
  ioc2rpz_fun:logMessage("unexpected: ~p ~n", [E]),
  {noreply, State}.

handle_call(_E, _From, State) ->
  {noreply, State}.

terminate(_Reason, _Tab) ->
%  ioc2rpz_db:tab2file([]),
  ok.
code_change(_OldVersion, Tab, _Extra) ->
  {ok, Tab}.
