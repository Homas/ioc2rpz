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

%IOC2RPZ TCP Worker

-module(ioc2rpz).
-behaviour(gen_server).

-include_lib("ioc2rpz.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_ioc2rpz/2,send_notify/1,send_packets/20,domstr_to_bin/2,send_zone_live/9,mrpz_from_ioc/2,parse_dns_request/3,ip_to_str/1,dombin_to_str/1]).


%-compile([export_all]).

start_ioc2rpz(Socket,Params) ->
  gen_server:start_link(?MODULE, [Socket,Params], []).

init([Socket,[Pid,Proc,TLS]]) ->
  ?logDebugMSG("ioc2rpz ~p child started ~n", [Proc]),
  gen_server:cast(self(), accept),
  {ok, #state{socket=Socket, tls=TLS, params=[Pid,Proc]}}.

%%%TCP accept
handle_cast(accept, State = #state{socket=ListenSocket, tls=no, params=[Pid,Proc]}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  %% Boot a new listener to replace this one.
  ioc2rpz_proc_sup:start_socket(Proc),
  {noreply, State#state{socket=AcceptSocket, tls=no, params=[Pid,Proc]}};

%%%TLS accept
handle_cast(accept, State = #state{socket=ListenSocket, tls=yes, params=[Pid,Proc]}) ->
  {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket),
  {ok, AcceptSocket} = ssl:handshake(TLSTransportSocket),
  %% Boot a new listener to replace this one.
  ioc2rpz_proc_sup:start_socket(Proc),
  {noreply, State#state{socket=AcceptSocket, tls=yes, params=[Pid,Proc]}};

handle_cast(_, State) ->
  {noreply, State}.

handle_info({tcp, Socket, <<_:2/binary,Pkt1/binary>>=_Pkt}, State = #state{socket=_ListenSocket, params=_Params}) ->

  %HCS = ioc2rpz_sup:db_table_info(ets,rpz_hotcache_table,memory) * 8,
  %if HCS > 106400 ->
  %  fprof:trace(start),
  %  io:format(group_leader(),"Second run. Start profiling.~n",[]),
  %  parse_dns_request(Socket, Pkt, Params),
  %  fprof:trace(stop);
  %  true -> parse_dns_request(Socket, Pkt, Params)
  %end,

%  fprof:trace(start),
  {ok,{R_ip,R_port}}=inet:peername(Socket),
  parse_dns_request(Socket, Pkt1, #proto{proto=tcp, tls=no, rip=R_ip, rport=R_port}),
%  fprof:trace(stop),
  {noreply, State};

handle_info({ssl, Socket, <<_:2/binary,Pkt1/binary>>=_Pkt}, State = #state{socket=_ListenSocket, params=_Params}) ->
%  fprof:trace(start),
  {ok,{R_ip,R_port}}=ssl:peername(Socket),
  parse_dns_request(Socket, Pkt1, #proto{proto=tcp, tls=yes, rip=R_ip, rport=R_port}),
%  fprof:trace(stop),
  {noreply, State};


handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
  {stop, normal, State};

handle_info({ssl_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({ssl_error, _Socket, _}, State) ->
  {stop, normal, State};


handle_info(E, State) ->
  ioc2rpz_fun:logMessage("unexpected: ~p ~n", [E]),
  {noreply, State}.

handle_call(_E, _From, State) ->
%  io:format("ioc2rpz accept connection~n"),
  {noreply, State}.
terminate(Reason, _Tab) ->
%  ioc2rpz_db:tab2file([]),
  ok.
code_change(_OldVersion, Tab, _Extra) ->
  {ok, Tab}.

%% Send a message back to the client
send_dns(Socket,Pkt,[Proto,Args]) when Proto#proto.proto == tcp, Proto#proto.tls == no ->
  case send_dns_tcp(Socket,Pkt, Args) of
   {error, Reason} -> ioc2rpz_fun:logMessage("~p:~p:~p. send_dns_tcp. error: ~p ~n",[?MODULE, ?FUNCTION_NAME, ?LINE, Reason]),
                      {error, Reason};
   ok -> ok
  end;

send_dns(Socket,Pkt,[Proto,Args]) when Proto#proto.proto == tcp, Proto#proto.tls == yes ->
  case send_dns_tls(Socket,Pkt, Args) of
   {error, Reason} -> ioc2rpz_fun:logMessage("~p:~p:~p. send_dns_tls. error: ~p ~n",[?MODULE, ?FUNCTION_NAME, ?LINE, Reason]),
                      {error, Reason};
   ok -> ok
  end;

send_dns(Socket,Pkt,[Proto,Args]) when Proto#proto.proto == udp ->
  send_dns_udp(Socket, Proto#proto.rip, Proto#proto.rport, Pkt, Args).

send_dns_tcp(Socket, Pkt, addlen) -> %used to send the first or an only packet
  gen_tcp:send(Socket, [<<(byte_size(Pkt)):16>>,Pkt]),
  ok = inet:setopts(Socket, [{active, once}]); 

send_dns_tcp(Socket, Pkt, []) -> %used to pass intermediate packets
  gen_tcp:send(Socket, Pkt),
  ok = inet:setopts(Socket, [{active, once}]).

send_dns_tls(Socket, Pkt, addlen) -> %used to send the first or an only packet
  ssl:send(Socket, [<<(byte_size(Pkt)):16>>,Pkt]),
% The connection will not be reused and a child will be terminated
% TODO check compliance with DoT
  ok = ssl:setopts(Socket, [{active, once}]); 

send_dns_tls(Socket, Pkt, []) -> %used to pass intermediate packets
  ssl:send(Socket, Pkt),
% The connection will not be reused and a child will be terminated
% TODO check compliance with DoT
  ok = ssl:setopts(Socket, [{active, once}]).

send_dns_udp(Socket, Dst, Port, Pkt, _Args) ->
  ok = gen_udp:send(Socket, Dst, Port, Pkt).

parse_dns_request(Socket, Data, Proto) when byte_size(Data) =< 12 ->
%%% Bad DNS packet
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(101),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto]);

parse_dns_request(Socket, Data, Proto) when Proto#proto.rport == 53; Proto#proto.rport == 853 ->
%%% DDoS attempt
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(501),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto]);

  
parse_dns_request(Socket, <<DNSId:2/binary, _:1, OptB:7, _:1, OptE:3, _:4, QDCOUNT:2/big-unsigned-unit:8,ANCOUNT:2/big-unsigned-unit:8,NSCOUNT:2/binary,ARCOUNT:2/binary, Rest/binary>> = _Data, Proto) when QDCOUNT /= 1 -> %_:2/binary, ;ANCOUNT /= 0
%%% Bad DNS request. QDCount != 1
  [QName,<<QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8, _Other_REC/binary>>] = binary:split(Rest,<<0>>),
  QStr=dombin_to_str(QName),
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(102),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass)]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?SERVFAIL:4>>, <<QDCOUNT:2,ANCOUNT:2,NSCOUNT:2,ARCOUNT:2>>, Rest, [], Proto);

parse_dns_request(Socket, <<PH:4/bytes, QDCOUNT:2/big-unsigned-unit:8,ANCOUNT:2/big-unsigned-unit:8,NSCOUNT:2/big-unsigned-unit:8,ARCOUNT:2/big-unsigned-unit:8, Rest/binary>> = _Data, Proto) when QDCOUNT == 1, ANCOUNT == 0 -> %_DataLen:2/big-unsigned-unit:8,
  STime=erlang:system_time(millisecond), %nanosecond, microsecond, millisecond, second
  <<DNSId:2/binary, _:1, OptB:7, _:1, OptE:3, _:4>> = PH,

  [QName,<<QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8, Other_REC/binary>>] = binary:split(Rest,<<0>>),
  Question = <<QName/binary,0:8,QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8>>,
  QStr=dombin_to_str(QName),

  {RRRes,DNSRR,TSIG,SOA,RAWN} = parse_rr(NSCOUNT, ARCOUNT, Other_REC),
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(202),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name)]),

  [[NSServ,MailAddr,MKeysT,ACL,_Cert,Srv]] = ets:match(cfg_table,{srv,'$2','$3','$4','$5','$6','$7'}),
	MKeys=lists:flatten([ MKeysT,[ ets:match(cfg_table,{[key_group,X,'_'],'$3'}) || X <- Srv#srv.key_groups ] ]),

  MGMTIP=ioc2rpz_fun:ip_in_list(ip_to_str(Proto#proto.rip),ACL),
%%%%in response AA flag should be 1 if there no error
  case {QName, QType, QClass,RRRes} of
%ioc2rpz statistics
    {<<_,"ioc2rpz-status">>,?T_TXT,?C_CHAOS,ok} when MGMTIP andalso Proto#proto.proto == tcp andalso ?MGMToDNS == true ->
      {TSIGV,TSIG1} = validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT-1,Question,RAWN,TSIG,MKeys),
      case TSIGV of
        noauth -> send_status(Socket,[Question,DNSId,OptB,OptE,[]], Proto);
        valid ->  send_status(Socket,[Question,DNSId,OptB,OptE,TSIG1], Proto);
        TSIGV -> send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, TSIG, ["ioc2rpz-status request failed",[TSIGV],QStr, QType, QClass], Proto)
      end;

%Reload configuration file
    {<<_,"ioc2rpz-reload-cfg">>,?T_TXT,?C_CHAOS,ok} when MGMTIP, Proto#proto.proto == tcp, ?MGMToDNS == true ->
      {TSIGV,TSIG1} = validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT-1,Question,RAWN,TSIG,MKeys),
      TXT = <<"ioc2rpz configuration was reloaded">>,
      case TSIGV of
        noauth -> ok = ioc2rpz_sup:reload_config3(reload), send_txt_response(Socket,[Question,DNSId,OptB,OptE,[]],TXT, Proto);
        valid ->  ok = ioc2rpz_sup:reload_config3(reload), send_txt_response(Socket,[Question,DNSId,OptB,OptE,TSIG1],TXT, Proto);
        TSIGV -> send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, TSIG, ["ioc2rpz-reload-cfg request failed",[TSIGV],QStr, QType, QClass], Proto)
      end;


%Reload tkeys from configuration
    {<<_,"ioc2rpz-update-tkeys">>,?T_TXT,?C_CHAOS,ok} when MGMTIP, Proto#proto.proto == tcp, ?MGMToDNS == true ->
      {TSIGV,TSIG1} = validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT-1,Question,RAWN,TSIG,MKeys),
      TXT = <<"ioc2rpz tkeys were updated">>,
      case TSIGV of
        noauth -> ok = ioc2rpz_sup:reload_config3(updTkeys), send_txt_response(Socket,[Question,DNSId,OptB,OptE,[]],TXT, Proto);
        valid ->  ok = ioc2rpz_sup:reload_config3(updTkeys), send_txt_response(Socket,[Question,DNSId,OptB,OptE,TSIG1],TXT, Proto);
        TSIGV -> send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, TSIG, ["ioc2rpz-update-tkeys request failed",[TSIGV],QStr, QType, QClass], Proto)
      end;

%Terminate ioc2rpz
    {<<_,"ioc2rpz-terminate">>,?T_TXT,?C_CHAOS,ok} when MGMTIP, Proto#proto.proto == tcp, ?MGMToDNS == true ->
      {TSIGV,TSIG1} = validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT-1,Question,RAWN,TSIG,MKeys),
%      [SuperVPID|_]=Params,
%      TXT = <<"ioc2rpz is terminating. PID: ",(list_to_binary(pid_to_list(SuperVPID)))/binary>>,
      TXT = <<"ioc2rpz is terminating.">>,
      case TSIGV of
        noauth -> send_txt_response(Socket,[Question,DNSId,OptB,OptE,[]],TXT, Proto),ioc2rpz_sup:stop_ioc2rpz_sup();
        valid ->  send_txt_response(Socket,[Question,DNSId,OptB,OptE,TSIG1],TXT, Proto),ioc2rpz_sup:stop_ioc2rpz_sup(); %exit(SuperVPID, kill)
        TSIGV -> send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, TSIG, ["ioc2rpz-terminate request failed",[TSIGV],QStr, QType, QClass], Proto)
      end;

%Update all zones/force AXFR for all zones
    {<<_,"ioc2rpz-update-all-rpz">>,?T_TXT,?C_CHAOS,ok} when MGMTIP, Proto#proto.proto == tcp, ?MGMToDNS == true ->
      {TSIGV,TSIG1} = validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT-1,Question,RAWN,TSIG,MKeys),
      TXT = <<"ioc2rpz forced AXFR for all zones">>,
      case TSIGV of
        noauth -> ioc2rpz_sup:update_all_zones(true), send_txt_response(Socket,[Question,DNSId,OptB,OptE,[]],TXT, Proto);
        valid ->  ioc2rpz_sup:update_all_zones(true), send_txt_response(Socket,[Question,DNSId,OptB,OptE,TSIG1],TXT, Proto);
        TSIGV -> send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, TSIG, ["ioc2rpz-update-all-rpz request failed",[TSIGV],QStr, QType, QClass], Proto)
      end;

%Update a zone/force AXFR for a zone
    {_,?T_TXT,?C_CHAOS,ok} when MGMTIP, Proto#proto.proto == tcp, ?MGMToDNS == true ->
      {TSIGV,TSIG1} = validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT-1,Question,RAWN,TSIG,MKeys),
      ZoneName = <<QName/binary,0:8>>,
      case ets:select(cfg_table, [{{[rpz,ZoneName],'$1','$2'},[],['$2']}]) of
        [Zone]  ->
          TXT = <<"ioc2rpz forced AXFR for ",(list_to_binary(Zone#rpz.zone_str))/binary>>,
          case TSIGV of
            noauth -> spawn_opt(ioc2rpz_sup,update_zone_full,[Zone],[{fullsweep_after,0}]), send_txt_response(Socket,[Question,DNSId,OptB,OptE,[]],TXT, Proto);
            valid ->  spawn_opt(ioc2rpz_sup,update_zone_full,[Zone],[{fullsweep_after,0}]), send_txt_response(Socket,[Question,DNSId,OptB,OptE,TSIG1],TXT, Proto);
            TSIGV -> send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, TSIG, ["ioc2rpz force AXFR request failed",[TSIGV],QStr, QType, QClass], Proto)
          end;
        _ ->
          TXT = <<(list_to_binary(QStr))/binary," is not configured">>,
          case TSIGV of
            noauth -> send_txt_response(Socket,[Question,DNSId,OptB,OptE,[]],TXT, Proto);
            valid ->  send_txt_response(Socket,[Question,DNSId,OptB,OptE,TSIG1],TXT, Proto);
            TSIGV -> send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, TSIG, [TXT,[TSIGV],QStr, QType, QClass], Proto)
          end
      end;

%Not permitted MGMT request
    {_,?T_TXT,?C_CHAOS,ok} when MGMTIP == false ->
          ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(301),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),""]),
          send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto);


%Sample zone
    {<<_,"sample-zone",7,"ioc2rpz">>, _, ?C_IN,ok} when MGMTIP andalso Proto#proto.proto == tcp andalso (QType == ?T_AXFR orelse QType == ?T_IXFR)  ->
      {TSIGV,TSIG1} = validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT-1,Question,RAWN,TSIG,MKeys),
      case TSIGV of
        noauth -> send_sample_zone(Socket, DNSId, OptB, OptE, Question, MailAddr, NSServ, [], Proto);
        valid -> send_sample_zone(Socket, DNSId, OptB, OptE, Question, MailAddr, NSServ, TSIG1, Proto); %add TSIG
        TSIGV -> send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, TSIG, ["sample-zone transfer failed",[TSIGV],QStr, QType, QClass], Proto)
      end;

%RPZs
%    {_,_,?C_IN,ok} when QType == ?T_SOA;QType == ?T_AXFR, NSCOUNT == 0,Proto#proto.proto == tcp; QType == ?T_IXFR, NSCOUNT == 1 -> %TODO check the guard
    {_,_,?C_IN,ok} when QType == ?T_SOA orelse (((QType == ?T_AXFR andalso NSCOUNT == 0) orelse (QType == ?T_IXFR andalso NSCOUNT == 1))  andalso Proto#proto.proto == tcp) -> %TODO check the guard
      ZoneName = <<QName/binary,0:8>>,
      case ets:select(cfg_table, [{{[rpz,ZoneName],'$1','$2'},[],['$2']}]) of
        [Zone]  ->
						%TODO pull all keys from key groups
						ZKeys=lists:flatten([ Zone#rpz.akeys,[ ets:match(cfg_table,{[key_group,X,'_'],'$3'}) || X <- Zone#rpz.key_groups ] ]),
            {TSIGV,TSIG1} = validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT-1,Question,RAWN,TSIG,ZKeys),
            case {QType,TSIGV} of
              {QType,noauth} when QType == ?T_SOA;QType == ?T_IXFR,Proto#proto.proto == udp -> send_SOA(Socket, Zone, DNSId, OptB, OptE, Question, MailAddr, NSServ, [], Proto);
              {QType,valid} when QType == ?T_SOA;QType == ?T_IXFR,Proto#proto.proto == udp -> send_SOA(Socket, Zone, DNSId, OptB, OptE, Question, MailAddr, NSServ, TSIG1, Proto);
              {_,noauth} -> send_zone(Zone#rpz.cache,Socket,{Question,DNSId,OptB,OptE,<<QDCOUNT:2,ANCOUNT:2,NSCOUNT:2,ARCOUNT:2>>,Rest,Zone, QType,NSServ,MailAddr,[],SOA}, Proto),
                  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(201),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),"",(erlang:system_time(millisecond)-STime)]);
              {_,valid} ->
                  send_zone(Zone#rpz.cache,Socket,{Question,DNSId,OptB,OptE,<<QDCOUNT:2,ANCOUNT:2,NSCOUNT:2,ARCOUNT:2>>,Rest,Zone,QType,NSServ,MailAddr,TSIG1,SOA}, Proto),
                  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(201),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),(erlang:system_time(millisecond)-STime)]);
                  %%%%%%%%%% double check TSIG1 was replaced with TSIG
              {_,TSIGV} -> send_TSIG_error(TSIGV, Socket, DNSId, OptB, OptE, Question, TSIG, ["zone transfer failed ~n",[Zone#rpz.zone_str,TSIGV],QStr, QType, QClass], Proto)
            end;
        _ ->
          ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(120),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),""]),
          send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto)
      end;
    {_,_,_,badTSIGposition} ->
    %rfc2845 If a TSIG record is present in any other position, the packet is dropped and a response with RCODE 1 (FORMERR) MUST be returned.
      ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(108),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),<<>>,""]),
      send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?FORMERR:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto);
    _ ->
      ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(102),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass)]),
      send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto)
  end,
  ok.


send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, TSIG, [MSG,_TSGV,QStr, QType, QClass], Proto) ->
 %%% request not signed
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(103),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),"",MSG]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?REFUSED:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto);

send_TSIG_error(keynotfound, Socket, DNSId, OptB, OptE, Question, TSIG, [MSG,_TSGV,QStr, QType, QClass], Proto) ->
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(104),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),MSG]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>, <<1:16,0:16,0:16,1:16>>,<<Question/binary,(TSIG#dns_TSIG_RR.name)/binary,?RT_TSIG:16/big-unsigned,?C_ANY:16/big-unsigned,0:32,(TSIG#dns_TSIG_RR.rdlength-TSIG#dns_TSIG_RR.mac_len):16/big-unsigned,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time)/binary,(TSIG#dns_TSIG_RR.fudge)/binary,0:16,DNSId/binary,?TSIG_BADKEY:16/big-unsigned,0:16>>, [], Proto);

send_TSIG_error(badmac, Socket, DNSId, OptB, OptE, Question, TSIG, [MSG,_TSGV,QStr, QType, QClass], Proto) ->
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(105),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),MSG]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>, <<1:16,0:16,0:16,1:16>>,<<Question/binary,(TSIG#dns_TSIG_RR.name)/binary,?RT_TSIG:16/big-unsigned,?C_ANY:16/big-unsigned,0:32,(TSIG#dns_TSIG_RR.rdlength-TSIG#dns_TSIG_RR.mac_len):16/big-unsigned,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time)/binary,(TSIG#dns_TSIG_RR.fudge)/binary,0:16,DNSId/binary,?TSIG_BADSIG:16/big-unsigned,0:16>>, [], Proto);

send_TSIG_error(badtimegoodmac, Socket, DNSId, OptB, OptE, Question, TSIG, [MSG,_TSGV,QStr, QType, QClass], Proto) ->
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(106),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),MSG]),
  CTime=erlang:system_time(seconds),
  Pkt = <<DNSId:2/binary,1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4,1:16,0:16,0:16,0:16, Question/binary>>,
  PKT = <<(TSIG#dns_TSIG_RR.mac_len):2/big-unsigned-unit:8,(TSIG#dns_TSIG_RR.mac)/binary,Pkt/binary,(TSIG#dns_TSIG_RR.name)/binary,0:8,255:8,0:32,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,?TSIG_BADTIME:16/big-unsigned,6:16,CTime:48>>,
  MAC = case TSIG#dns_TSIG_RR.alg_str of %TODO вынести в функцию
    "md5" -> crypto:hmac(md5,TSIG#dns_TSIG_RR.key,PKT);
    "sha256" -> crypto:hmac(sha256,TSIG#dns_TSIG_RR.key,PKT);
    "sha512" -> crypto:hmac(sha512,TSIG#dns_TSIG_RR.key,PKT)
  end,
  MAC_LEN=byte_size(MAC),
  DATA = <<(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,MAC_LEN:2/big-unsigned-unit:8,MAC/binary,(TSIG#dns_TSIG_RR.oid):16,?TSIG_BADTIME:16/big-unsigned,6:16,CTime:48>>,
  DLEN=byte_size(DATA),
  TSIGR = <<(TSIG#dns_TSIG_RR.name)/binary,0:8,250:8,0:8,255:8,0:32,DLEN:2/big-unsigned-unit:8,DATA/binary>>,
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>,<<1:16,0:16,0:16,1:16>>,<<Question/binary,TSIGR/binary>>, [], Proto);


send_TSIG_error(_, Socket, DNSId, OptB, OptE, Question, TSIG, [MSG,MSPG,QStr, QType, QClass], Proto) ->
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(107),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),MSG, MSPG]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?SERVFAIL:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto).


validate_REQ(_PH,_QDCOUNT,_ANCOUNT,_NSCOUNT,_ARCOUNT,_Question,_DNSRR,TSIG, KEYS)  when TSIG#dns_TSIG_RR.name == <<>>, KEYS /= [] ->
  {notsig,[]};

validate_REQ(_PH,_QDCOUNT,_ANCOUNT,_NSCOUNT,_ARCOUNT,_Question,_DNSRR,TSIG, KEYS)  when TSIG#dns_TSIG_RR.name == <<>>, KEYS == [] ->
  {noauth,[]};

validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT,Question,DNSRR,TSIG, KEYS) when TSIG#dns_TSIG_RR.name /= <<>> ->
  case {ets:select(cfg_table, [{{[key,TSIG#dns_TSIG_RR.name],'$1','$2','$3'},[],[['$1','$2','$3']]}]), lists:member(TSIG#dns_TSIG_RR.name,KEYS)} of
    {[[KeyName,Alg, KEY]],true} when Alg == "md5",TSIG#dns_TSIG_RR.alg == <<8:8,"hmac-md5",7:8,"sig-alg",3:8,"reg",3:8,"int",0:8>>; Alg == "sha256",TSIG#dns_TSIG_RR.alg == <<11:8,"hmac-sha256",0:8>>; Alg == "sha512",TSIG#dns_TSIG_RR.alg == <<11:8,"hmac-sha512",0:8>>  ->
        ?logDebugMSG("Found Key ... ",[]),
        LTime=erlang:system_time(seconds), RTimeL = binary:decode_unsigned(TSIG#dns_TSIG_RR.time) - binary:decode_unsigned(TSIG#dns_TSIG_RR.fudge), RTimeH = binary:decode_unsigned(TSIG#dns_TSIG_RR.time) + binary:decode_unsigned(TSIG#dns_TSIG_RR.fudge),
        PKT = <<PH/binary,QDCOUNT:2/big-unsigned-unit:8,ANCOUNT:2/big-unsigned-unit:8,NSCOUNT:2/big-unsigned-unit:8,ARCOUNT:2/big-unsigned-unit:8,Question/binary,DNSRR/binary,(TSIG#dns_TSIG_RR.name)/binary,0,255,0,0,0,0,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,(TSIG#dns_TSIG_RR.error)/binary,(TSIG#dns_TSIG_RR.olen):2/big-unsigned-unit:8,(TSIG#dns_TSIG_RR.odata)/binary>>,
        CH_MAC = case Alg of %TODO вынести в функцию
          "md5" -> crypto:hmac(md5,KEY,PKT);
          "sha256" -> crypto:hmac(sha256,KEY,PKT);
          "sha512" -> crypto:hmac(sha512,KEY,PKT)
        end,
        case CH_MAC == TSIG#dns_TSIG_RR.mac of
          true when LTime >= RTimeL, LTime =< RTimeH -> ?logDebugMSG("Good timestamp ... Valid MAC~n",[]),      {valid,TSIG#dns_TSIG_RR{alg_str=Alg,key=KEY}};
          false when LTime >= RTimeL, LTime =< RTimeH -> ?logDebugMSG("Good timestamp ... NOT Valid MAC~n",[]), {badmac,[]};
            %TODO 4.5.2 cache client time and if later request contains early time -> BADTIME
          true -> ?logDebugMSG("Bad timestamp ... Valid MAC ~n",[]),  {badtimegoodmac,TSIG#dns_TSIG_RR{alg_str=Alg,key=KEY}};
          false -> ?logDebugMSG("Bad timestamp ... NOT Valid MAC ~n",[]), {badmac,[]}
        end;
    {_,_} ->
        ?logDebugMSG("Key NOT found ~p ~p~n",[dombin_to_str(TSIG#dns_TSIG_RR.name), dombin_to_str(TSIG#dns_TSIG_RR.alg)]),
        {keynotfound,TSIG}
  end.

%Sign the packet
add_TSIG(Pkt, TSIG) ->
  if TSIG#dns_TSIG_RR.time_only == true ->
      PKT = <<(TSIG#dns_TSIG_RR.mac_len):2/big-unsigned-unit:8,(TSIG#dns_TSIG_RR.mac)/binary,Pkt/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary>>;
    true ->
      PKT = <<(TSIG#dns_TSIG_RR.mac_len):2/big-unsigned-unit:8,(TSIG#dns_TSIG_RR.mac)/binary,Pkt/binary,(TSIG#dns_TSIG_RR.name)/binary,0:8,255:8,0:32,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,0:16,0:16>>
  end,
  MAC = case TSIG#dns_TSIG_RR.alg_str of %TODO вынести в функцию
    "md5" -> crypto:hmac(md5,TSIG#dns_TSIG_RR.key,PKT);
    "sha256" -> crypto:hmac(sha256,TSIG#dns_TSIG_RR.key,PKT);
    "sha512" -> crypto:hmac(sha512,TSIG#dns_TSIG_RR.key,PKT)
  end,
  LTime = <<(erlang:system_time(seconds)):6/big-unsigned-unit:8>>, % why should we resend client's time?
  %LTime = <<(TSIG#dns_TSIG_RR.time):6/binary>>,
  MAC_LEN=byte_size(MAC),
  DATA = <<(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,MAC_LEN:2/big-unsigned-unit:8,MAC/binary,(TSIG#dns_TSIG_RR.oid):16,0:16,0:16>>,
  DLEN=byte_size(DATA),
  {ok,<<(TSIG#dns_TSIG_RR.name)/binary,0:8,250:8,0:8,255:8,0:32,DLEN:2/big-unsigned-unit:8,DATA/binary>>,TSIG#dns_TSIG_RR{mac_len=MAC_LEN,mac=MAC,time_only=true,time=LTime}}. %END Sign the packet


parse_rr(0, 0, <<>>) ->
  {ok,[],#dns_TSIG_RR{name = <<>>},#dns_SOA_RR{name = <<>>,serial=0},<<>>};

parse_rr(NSCOUNT, ARCOUNT, RAW) ->
  parse_rr(NSCOUNT, ARCOUNT, RAW, [],#dns_SOA_RR{name = <<>>,serial=0}, <<>>).

parse_rr(0, 0, <<>>, RR, RAWN, SOA) ->
  {ok,RR,#dns_TSIG_RR{name = <<>>},SOA,RAWN};

parse_rr(NSCOUNT, ARCOUNT, <<Zip:8,_/binary>> = RAW, RR, SOA, RAWN) ->
  if Zip >= 192 ->
      <<RNAME:2/binary, RType:2/big-unsigned-unit:8,RClass:2/big-unsigned-unit:8,RTTL:4/big-unsigned-unit:8,DLen:2/big-unsigned-unit:8,REST/binary>> = RAW;
    true ->
      %TODO переделать, так как может зиповаться часть записи/FQDN и в этом случае нужен весь пакет, чтобы получить запись
      [RNAME0,<<RType:2/big-unsigned-unit:8,RClass:2/big-unsigned-unit:8,RTTL:4/big-unsigned-unit:8,DLen:2/big-unsigned-unit:8,REST/binary>>] = binary:split(RAW,<<0>>),
      RNAME = <<RNAME0/binary,0:8>>
  end,
  case {RType, RClass} of
    {?RT_TSIG, ?C_ANY} -> %TSIG Record
      if DLen < byte_size(REST) -> {badTSIGposition,[],[]};
        true ->
          [TSIG_ALG,<<TSIG_TIME:6/bytes,TSIG_FUDGE:2/bytes,MACLEN:2/big-unsigned-unit:8,REST1/binary>>] = binary:split(REST,<<0>>),
          <<TSIG_MAC:MACLEN/bytes,TSIG_OID:2/big-unsigned-unit:8,TSIG_ERR:2/bytes,TSIG_OTHER_LEN:2/big-unsigned-unit:8,REST2/binary>> = REST1,
          <<TSIG_OTHER:TSIG_OTHER_LEN/bytes>> = REST2,
          {ok,RR,#dns_TSIG_RR{name=RNAME,type=RType,class=RClass,rdlength=DLen, alg = <<TSIG_ALG/binary,0>>, time=TSIG_TIME, fudge=TSIG_FUDGE, mac_len=MACLEN, mac=TSIG_MAC, oid=TSIG_OID,error=TSIG_ERR,olen=TSIG_OTHER_LEN,odata=TSIG_OTHER,time_only=false},SOA,RAWN}
      end;

    {?T_SOA, ?C_IN} -> %SOA Record
%-record(dns_SOA_RR, {name, type, class, ttl, rdlength, mname, rname, serial, refresh, retry, expire, minimum}).
      <<RDATA:DLen/bytes,RAW2/binary>> = REST,
      [MName,REST2] = binary:split(REST,<<0>>),
      [RName,<<Serial:32/big-unsigned, Refresh:32/big-unsigned, Retry:32/big-unsigned, Expire:32/big-unsigned, Minimum:32/big-unsigned,_/binary>>] = binary:split(REST2,<<0>>),
      if NSCOUNT > 0 -> NSCOUNT1=NSCOUNT-1, ARCOUNT1=ARCOUNT; true -> ARCOUNT1=ARCOUNT-1,NSCOUNT1=NSCOUNT end,
      %?logDebugMSG("SOA Serial ~p ~n",[Serial]),
      SOA2=#dns_SOA_RR{name=RNAME, type=RType, class=RClass, ttl=RTTL, rdlength=DLen, mname=MName, rname=RName, serial=Serial, refresh=Refresh, retry=Retry, expire=Expire, minimum=Minimum},
      parse_rr(NSCOUNT1, ARCOUNT1, RAW2, RR ++ [SOA2], SOA2, <<RAWN/binary,RNAME/binary, RType:2/big-unsigned-unit:8,RClass:2/big-unsigned-unit:8,RTTL:4/big-unsigned-unit:8,DLen:2/big-unsigned-unit:8,RDATA/binary>>);

    {?T_OPT, _ } -> %OPT Record
      %?logDebugMSG("Got OPT  RR. Type ~p UDP Payload ~p RCODE ~p RDLEN ~p ~n",[RType,RClass, RTTL, DLen]),
      <<RDATA:DLen/bytes,RAW2/binary>> = REST,
      %EXTCode - High bits for Rcode (RFC1035), EDNSVer=0 according with RFC6891, DO - Can handle DNSEC (RFC3225), Z - must be 0
      %<<EXTCode:8, EDNSVer:8, DO:1, Z:15>> = RTTL,
      if NSCOUNT > 0 -> NSCOUNT1=NSCOUNT-1, ARCOUNT1=ARCOUNT; true -> ARCOUNT1=ARCOUNT-1,NSCOUNT1=NSCOUNT end,
      parse_rr(NSCOUNT1, ARCOUNT1, RAW2, RR ++ [#dns_RR{name=RNAME, type=RType, class=RClass, ttl=RTTL, rdlength=DLen, rdata=RDATA}], SOA, <<RAWN/binary,RNAME/binary, RType:2/big-unsigned-unit:8,RClass:2/big-unsigned-unit:8,RTTL:4/big-unsigned-unit:8,DLen:2/big-unsigned-unit:8,RDATA/binary>>);

    _Else ->
      ioc2rpz_fun:logMessage("Got unsuported  RR. Type ~p Class ~p  NSCount ~p, ARCount ~p ~n",[RType,RClass, NSCOUNT, ARCOUNT]),
      <<RDATA:DLen/bytes,RAW2/binary>> = REST,
      if NSCOUNT > 0 -> NSCOUNT1=NSCOUNT-1, ARCOUNT1=ARCOUNT; true -> ARCOUNT1=ARCOUNT-1,NSCOUNT1=NSCOUNT end,
      parse_rr(NSCOUNT1, ARCOUNT1, RAW2, RR ++ [#dns_RR{name=RNAME, type=RType, class=RClass, ttl=RTTL, rdlength=DLen, rdata=RDATA}], SOA, <<RAWN/binary,RNAME/binary, RType:2/big-unsigned-unit:8,RClass:2/big-unsigned-unit:8,RTTL:4/big-unsigned-unit:8,DLen:2/big-unsigned-unit:8,RDATA/binary>>)
  end.



%Send SERVFAIL/REFUSED/NXDOMAIN
send_REQST(Socket, DNSId, Opt, RH, Question, TSIG, Proto) ->
  %Pkt1 = list_to_binary([DNSId, Opt, RH, Rest]), % <<1:1, OptB:7, 1:1, OptE:3, Status:4>>
  %PktLen = byte_size(Pkt1),
  %Pkt = [<<PktLen:16>>,Pkt1],

%  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?SERVFAIL:4>>, <<1:16,0:16,0:16,0:16>>, Questions, Proto);

  if TSIG /= [] ->
    {ok,TSIGRR,_}=add_TSIG(list_to_binary([DNSId, Opt, RH, Question]),TSIG),
            Pkt1 = list_to_binary([DNSId, Opt, <<1:16,0:16,0:16,1:16>>, Question, TSIGRR]);
    true -> Pkt1 = list_to_binary([DNSId, Opt, RH, Question])
  end,


  send_dns(Socket,Pkt1, [Proto,addlen]).
%END Send SERVFAIL/REFUSED/NXDOMAIN

%Send SOA
send_SOA(Socket, Zone, DNSId, OptB, OptE, Question, MailAddr, NSServ, TSIG, Proto) ->
  SOA = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,7200:32,3600:32,259001:32,7200:32>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOA)):16, SOA/binary>>,
  <<Opcode:4,_:1,TCRD:2>> = <<OptB:7>>,
  if TSIG /= [] ->
    {ok,TSIGRR,_}=add_TSIG(list_to_binary([DNSId, <<1:1, Opcode:4,1:1,TCRD:2, 0:1, OptE:3, ?NOERROR:4, 1:16,1:16,0:16,0:16>>, Question, SOAREC]),TSIG),
    Pkt1 = list_to_binary([DNSId, <<1:1, Opcode:4,1:1,TCRD:2, 0:1, OptE:3, ?NOERROR:4, 1:16,1:16,0:16,1:16>>, Question, SOAREC, TSIGRR]);
    true -> Pkt1 = <<DNSId/binary, 1:1, Opcode:4,1:1,TCRD:2, 1:1, OptE:3, ?NOERROR:4, 1:16,1:16,0:16,0:16, Question/binary, SOAREC/binary>>
  end,
  send_dns(Socket,Pkt1, [Proto,addlen]).
%END Send SOA

%Send sample zone
send_sample_zone(Socket, DNSId, OptB, OptE, Questions, MailAddr, NSServ, TSIG, Proto) ->
  SOA = <<NSServ/binary,MailAddr/binary,(ioc2rpz_fun:curr_serial()):32,7200:32,3600:32,259001:32,7200:32>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOA)):16, SOA/binary>>,
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,

%  T_ZIP_L=ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]),
	Zone=#rpz{zone_str=?ioc2rpzSampleRPZ},
	T_ZIP_L=init_T_ZIP_L(Zone),
  NXLoc=byte_size(list_to_binary([Questions,SOAREC, NSRec]))+12,
  {ok, _, NXRules,_} = gen_rpzrule(<<"nxdomain.net.",?ioc2rpzSampleRPZ,".">>,Zone,?TTL,<<"true">>,<<"nxdomain">>,[],NXLoc,T_ZIP_L),
  NDLoc=byte_size(list_to_binary(NXRules))+NXLoc,
  {ok, _, NDRules,_} = gen_rpzrule(<<"nodata.net.",?ioc2rpzSampleRPZ,".">>,Zone,?TTL,<<"true">>,<<"nodata">>,[],NDLoc,T_ZIP_L),
  PSSLoc=byte_size(list_to_binary(NDRules))+NDLoc,
  {ok, _, PSSRules,_} = gen_rpzrule(<<"passthru.net.",?ioc2rpzSampleRPZ,".">>,Zone,?TTL,<<"true">>,<<"passthru">>,[],PSSLoc,T_ZIP_L),
  DrDLoc=byte_size(list_to_binary(PSSRules))+PSSLoc,
  {ok, _, DrDRules,_} = gen_rpzrule(<<"drop.net.",?ioc2rpzSampleRPZ,".">>,Zone,?TTL,<<"true">>,<<"drop">>,[],DrDLoc,T_ZIP_L),
  TCPLoc=byte_size(list_to_binary(DrDRules))+DrDLoc,
  {ok, _, TCPRules,_} = gen_rpzrule(<<"tcp-only.net.",?ioc2rpzSampleRPZ,".">>,Zone,?TTL,<<"true">>,<<"tcp-only">>,[],TCPLoc,T_ZIP_L),
  RedirDLoc=byte_size(list_to_binary(TCPRules))+TCPLoc,
  {ok, _, RedirDRules,_} = gen_rpzrule(<<"redirect_domain_to_com_from.net.",?ioc2rpzSampleRPZ,".">>,Zone,?TTL,<<"true">>,{<<"redirect_domain">>,<<"redirect_domain_from_net_to.com">>},[],RedirDLoc,T_ZIP_L),
  RedirIPLoc=byte_size(list_to_binary(RedirDRules))+RedirDLoc,
  {ok, _, RedirIPRules,_} = gen_rpzrule(<<"redirect_domain_to.ip.",?ioc2rpzSampleRPZ,".">>,Zone,?TTL,<<"true">>,{<<"redirect_ip">>,<<10,42,42,42>>},[],RedirIPLoc,T_ZIP_L),
  RedirIP6Loc=byte_size(list_to_binary(RedirIPRules))+RedirIPLoc,
  {ok, _, RedirIP6Rules,_} = gen_rpzrule(<<"redirect_domain_to.ip6.",?ioc2rpzSampleRPZ,".">>,Zone,?TTL,<<"true">>,{<<"redirect_ip">>,<<10,42,0,0,0,0,0,0,0,0,0,0,0,0,10,42>>},[],RedirIP6Loc,T_ZIP_L),

  NDIPLoc=byte_size(list_to_binary(RedirIP6Rules))+RedirIP6Loc,
  {ok, _, NDIPv,_} = gen_rpzrule(reverse_IP(<<"10.42.42.42">>),Zone,?TTL,<<"false">>,<<"ip">>,[<<"nxdomain">>,[]],NDIPLoc,T_ZIP_L),
  PSSIPv6Loc=byte_size(list_to_binary(NDIPv))+NDIPLoc,
  {ok, _, PSSIPv6,_} = gen_rpzrule(reverse_IP(<<"fc00::/64">>),Zone,?TTL,<<"false">>,<<"ip">>,[<<"passthru">>,[]],PSSIPv6Loc,T_ZIP_L),

  NSDLoc=byte_size(list_to_binary(PSSIPv6))+PSSIPv6Loc,
  {ok, _, NSDRules,_} = gen_rpzrule("nsdname.com",Zone,?TTL,<<"false">>,<<"nsdname">>,[<<"nxdomain">>,[]],NSDLoc,T_ZIP_L),
  NSIPvLoc=byte_size(list_to_binary(NSDRules))+NSDLoc,
  {ok, _, NSIPv,_} = gen_rpzrule(reverse_IP(<<"10.42.42.42">>),Zone,?TTL,<<"false">>,<<"nsip">>,[<<"nxdomain">>,[]],NSIPvLoc,T_ZIP_L),
  NSIPv6Loc=byte_size(list_to_binary(NSIPv))+NSIPvLoc,
  {ok, _, NSIPv6,_} = gen_rpzrule(reverse_IP(<<"fc00::/64">>),Zone,?TTL,<<"false">>,<<"nsip">>,[<<"nxdomain">>,[]],NSIPv6Loc,T_ZIP_L),

%  SmRPZ = #rpz{axfr_url = ["http://data.netlab.360.com/feeds/dga/blackhole.txt"], wildcards = <<"true">>, action = <<"nxdomain">>, cl_rex = ""},
%  IOC = mrpz_from_ioc(SmRPZ#rpz.axfr_url,SmRPZ,?TTL,[]),
%  [io:fwrite(group_leader(),"IOC ~p ~n",[X]) || X <- IOC ],

  Rules=lists:flatten([NXRules, NDRules, PSSRules, DrDRules, TCPRules, RedirDRules, RedirIPRules, RedirIP6Rules,NDIPv,PSSIPv6, NSDRules, NSIPv, NSIPv6]), %IOCRules

  ACount=1+1+length(Rules)+1, %SOA, NS, Records, SOA
  %Pkt1 = list_to_binary([DNSId, <<1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16,ACount:16,0:16,0:16>>, Questions, SOAREC, NSRec, Rules, SOAREC]), %QCount, ACount, NCount, ACount ...
  if TSIG /= [] ->
    {ok,TSIGRR,_}=add_TSIG(list_to_binary([DNSId, <<1:1, OptB:7, 0:1, OptE:3, ?NOERROR:4, 1:16,ACount:16,0:16,0:16>>, Questions, SOAREC, NSRec, Rules, SOAREC]),TSIG),
    Pkt1 = list_to_binary([DNSId, <<1:1, OptB:7, 0:1, OptE:3, ?NOERROR:4, 1:16,ACount:16,0:16,1:16>>, Questions, SOAREC, NSRec, Rules, SOAREC, TSIGRR]);
    true -> Pkt1 = <<DNSId/binary, 1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16,ACount:16,0:16,0:16, Questions/binary, SOAREC/binary, NSRec/binary, Rules/binary, SOAREC/binary>>
  end,
  ets:delete(T_ZIP_L),
  send_dns(Socket,Pkt1, [Proto,addlen]).
  
%END Send sample zone

%Send server status
send_status(Socket,[Question,DNSId,OptB,OptE,TSIG], Proto) ->
  WS = erlang:system_info(wordsize),
  SCfg = list_to_binary(integer_to_list(ioc2rpz_db:db_table_info(cfg_table,size))),
  MSCfg = ioc2rpz_fun:conv_to_Mb(ioc2rpz_db:db_table_info(cfg_table,memory) * WS),
  SHC = list_to_binary(integer_to_list(ioc2rpz_db:db_table_info(rpz_hotcache_table,size))),
  MSHC = ioc2rpz_fun:conv_to_Mb(ioc2rpz_db:db_table_info(rpz_hotcache_table,memory) * WS),
  SAXFR = list_to_binary(integer_to_list(ioc2rpz_db:db_table_info(rpz_axfr_table,size))),
  MAXFR = ioc2rpz_fun:conv_to_Mb(ioc2rpz_db:db_table_info(rpz_axfr_table,memory) * WS),
  SIXFR = list_to_binary(integer_to_list(ioc2rpz_db:db_table_info(rpz_ixfr_table,size))),
  MIXFR = ioc2rpz_fun:conv_to_Mb(ioc2rpz_db:db_table_info(rpz_ixfr_table,memory) * WS),
%TODO
%Statistics per zone
%TODO

  Data1 = <<"ioc2rpz status: Cfg (", SCfg/binary, "/Rec, ", MSCfg/binary,"), Hot (", SHC/binary, "/Rec, ", MSHC/binary,"), AXFR (", SAXFR/binary, "/Rec, ", MAXFR/binary,") IXFR (", SIXFR/binary, "/Rec, ", MIXFR/binary,")">>,

  send_txt_response(Socket,[Question,DNSId,OptB,OptE,TSIG],Data1, Proto).
%END Send server status



%Send TXT response
send_txt_response(Socket,[Questions,DNSId,OptB,OptE,TSIG],Data, Proto) ->
%  Multiple TXT records
%  TXTRec=[gen_txt_rec(TXT)|| TXT <-ioc2rpz_fun:split_bin_bytes(Data,254)],
%  NRec=length(TXTRec),

% Single TXT record
  TXT=list_to_binary([ <<(byte_size(T)):8,T/binary>> || T <-ioc2rpz_fun:split_bin_bytes(Data,254)]),
  TXTRec = <<16#c00c:16, ?T_TXT:2/big-unsigned-unit:8, ?C_IN:2/big-unsigned-unit:8, ?TTL:32, (byte_size(TXT)):16, TXT/binary>>,
  NRec=1,
  if TSIG /= [] ->
    {ok,TSIGRR,_}=add_TSIG(list_to_binary([DNSId, <<1:1, OptB:7, 0:1, OptE:3, ?NOERROR:4, 1:16,NRec:16,0:16,0:16>>, Questions, TXTRec]),TSIG),
    Pkt1 = list_to_binary([DNSId, <<1:1, OptB:7, 0:1, OptE:3, ?NOERROR:4, 1:16,NRec:16,0:16,1:16>>, Questions, TXTRec, TSIGRR]);
    true -> Pkt1 = <<DNSId/binary, 1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16,NRec:16,0:16,0:16, Questions/binary, TXTRec/binary>>
  end,
  send_dns(Socket,Pkt1, [Proto,addlen]).

%END TXT response

gen_txt_rec(TXT) ->
  Len=byte_size(TXT),
  <<16#c00c:16, ?T_TXT:2/big-unsigned-unit:8, ?C_IN:2/big-unsigned-unit:8, ?TTL:32, (Len+1):16, Len:8, TXT/binary>>.


send_notify(Zone) ->
  %TODO wait for the confirmation
  Pkt = <<0:1, ?OP_NOTIFY, 1:1, 0:6, ?NOERROR:4, 1:16,0:16,0:16,0:16,(Zone#rpz.zone)/binary,?T_SOA:16,?C_IN:16>>,
  [ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(221),[IP,53,Proto,Zone#rpz.zone_str]) || {Proto,IP} <- Zone#rpz.notifylist ],
  [ spawn(ioc2rpz,send_notify,[IP,Pkt,Proto,0,Zone#rpz.zone_str]) || {Proto,IP} <- Zone#rpz.notifylist ].

send_notify(Dst,Pkt,udp,NRuns,Zone) -> % TODO NRuns - will be used to resend Notify if not confirmation was not received
  Port=rand:uniform(55535)+10000,
  case gen_udp:open(Port, [{active,false}]) of
  	{ok, Sock} ->
      DNSId = crypto:strong_rand_bytes(2),
      send_dns_udp(Sock, Dst, 53, [DNSId,Pkt],[]),
%      {Status,Pkt} = get_packet(Sock,Server,DNSId), %TODO wait for the response
      gen_udp:close(Sock);
    {error, eaddrinuse} when NRuns < 3 -> send_notify(Dst,Pkt,udp,NRuns+1,Zone);
  	{error, Reason} -> {Reason,[]}
  end;

send_notify(Dst,Pkt,tcp,NRuns,Zone) ->
  case gen_tcp:connect(Dst, 53, [{active, false}], ?TCPTimeout) of
    {ok, Socket} -> DNSId = crypto:strong_rand_bytes(2), send_dns_tcp(Socket, <<DNSId/binary,Pkt/binary>>, addlen), gen_tcp:close(Socket);
    {error, eaddrinuse} when NRuns < 3 -> send_notify(Dst,Pkt,tcp,NRuns+1,Zone);
  	{error, Reason} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(222),[Dst,53,"tcp",Zone,Reason]), {Reason,[]}
  end.

send_cached_zone(Socket,NSREC, SOAREC, TSIG, PktH, Questions, Pkts, Proto) -> %created becasue of concurent zone creation
  send_cached_zone(Socket,NSREC, SOAREC, TSIG, PktH, Questions, Pkts,0, Proto).
  
send_cached_zone(Socket,_NSREC, _SOAREC, _TSIG, _PktH, _Questions, [], _PktNum, Proto) ->
ok;

send_cached_zone(Socket, NSREC, SOAREC, TSIG, PktH, Questions, [{PktN,ANCOUNT,NSCOUNT,ARCOUNT,Pkt}|REST], PktNum, Proto) ->
  if PktNum==0 -> PktF=[SOAREC,NSREC,Pkt], Cnt=2; true -> PktF=Pkt, Cnt=0 end,
  if REST==[] -> PktL=[PktF,SOAREC],Cnt1=Cnt+1; true -> PktL=PktF, Cnt1=Cnt end,
  if TSIG /= [] ->
    {ok,TSIGRR,TSIG1}=add_TSIG(list_to_binary([PktH, <<(ANCOUNT+Cnt1):16,NSCOUNT:16,ARCOUNT:16>>, Questions, PktL]),TSIG),
    Pkt1 = list_to_binary([PktH, <<(ANCOUNT+Cnt1):16,NSCOUNT:16,(ARCOUNT+1):16>>, Questions, PktL, TSIGRR]);
    true -> Pkt1 = list_to_binary([PktH,<<(ANCOUNT+Cnt1):16,NSCOUNT:16,ARCOUNT:16>>, Questions, PktL]), TSIG1=TSIG
  end,
  case send_dns(Socket,Pkt1, [Proto,addlen]) of
    ok -> send_cached_zone(Socket, NSREC, SOAREC, TSIG1, PktH, Questions, REST,PktNum+1, Proto);
  	{error, Reason} -> {error, Reason}  
  end.

%Return cached zone
send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,_RH,_Rest,Zone,?T_AXFR,NSServ,MailAddr,TSIG,_SOA}, Proto) when Zone#rpz.status == ready;Zone#rpz.status == updating,Zone#rpz.serial /= 0 -> %AXFR
%  ioc2rpz_fun:logMessage("Zone ~p is cached ~p ~p ~n", [Zone#rpz.zone_str, Zone#rpz.serial,Zone#rpz.status ]),

  SOA = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOA)):16, SOA/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  Pkt=ioc2rpz_db:read_db_pkt(Zone),
  send_cached_zone(Socket, NSRec, SOAREC, TSIG, <<DNSId:2/binary ,1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>, Questions, Pkt, Proto);

send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,RH,Rest,Zone,?T_IXFR,NSServ,MailAddr,TSIG,SOA}, Proto) when Zone#rpz.serial=<SOA#dns_SOA_RR.serial ->
%If an IXFR query with the same or newer version number than that of the server is received, it is replied to with a single SOA record of the server's current version, just as in AXFR.
  SOAR = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOAR)):16, SOAR/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  PktH = [DNSId, <<1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>],
  if TSIG /= [] ->
    {ok,TSIGRR,_}=add_TSIG(list_to_binary([PktH, <<1:16,0:16,0:16>>, Questions, SOAREC]),TSIG),
    Pkt1 = list_to_binary([PktH, <<1:16,0:16,1:16>>, Questions, SOAREC, TSIGRR]);
    true -> Pkt1 = list_to_binary([PktH,<<1:16,0:16,0:16>>, Questions, SOAREC])
  end,
  send_dns(Socket,Pkt1, [Proto,addlen]);


send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,RH,Rest,Zone,?T_IXFR,NSServ,MailAddr,TSIG,SOA}, Proto) when Zone#rpz.serial==Zone#rpz.serial_ixfr,Zone#rpz.status == ready;Zone#rpz.serial==Zone#rpz.serial_ixfr,Zone#rpz.status == updating;SOA#dns_SOA_RR.serial<Zone#rpz.serial_ixfr,Zone#rpz.status == ready;SOA#dns_SOA_RR.serial<Zone#rpz.serial_ixfr,Zone#rpz.status == updating ->
%Serial_IXFR = Serial => do full zone transfer. SOA#dns_SOA_RR.serial less than Serial and we do not have the changes log
%  ioc2rpz_fun:logMessage("Serial = Serial IXFR ~p ~n",[Zone]),
  SOAR = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOAR)):16, SOAR/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  Pkt=ioc2rpz_db:read_db_pkt(Zone),
  send_cached_zone(Socket, NSRec, SOAREC, TSIG, <<DNSId:2/binary ,1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>, Questions, Pkt, Proto);

send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,RH,Rest,Zone,?T_IXFR,NSServ,MailAddr,TSIG,SOA}, Proto) when Zone#rpz.status == ready;Zone#rpz.status == updating -> %IXFR
  SOAR = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOARCL = <<NSServ/binary,MailAddr/binary,(SOA#dns_SOA_RR.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOAR)):16, SOAR/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  SOARECCL = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOARCL)):16, SOARCL/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  IOCexp=[ {X,Z} || [X,_Y,Z] <- ioc2rpz_db:read_db_record(Zone,SOA#dns_SOA_RR.serial,expired) ],
  IOCnew=[ {X,Z} || [X,_Y,Z] <- ioc2rpz_db:read_db_record(Zone,SOA#dns_SOA_RR.serial,new)],
%  ioc2rpz_fun:logMessage("Serial ~p /= Serial IXFR ~p Zone ~p Expired IOC ~p, New IOC ~p ~n",[Zone#rpz.serial,Zone#rpz.serial_ixfr,Zone#rpz.zone_str,IOCexp,IOCnew]),

  {ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})$"), %
  PktHLen = 12+byte_size(Questions),
%  T_ZIP_L=ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]), % нужны ли {read_concurrency, true}, {write_concurrency, true} ???
	T_ZIP_L=init_T_ZIP_L(Zone),
  %В момент переключения на добавления - SOARECCL обнуляем, таким образом отслеживаем, что мы добавили новую SOA
  send_packets(Socket,IOCexp ++ IOCnew, [], 0, 0, true, [DNSId, <<1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>], Questions, SOAREC,SOARECCL,Zone,MP,PktHLen,T_ZIP_L,TSIG,0,ixfr,0,false,Proto),
  ets:delete(T_ZIP_L),
  ok;

%Zone was not cached, but should be
send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,_RH,_Rest,Zone,QType,_NSServ,_MailAddr,TSIG,SOA}, Proto) when Zone#rpz.status == notready;Zone#rpz.status == updating ->
 % ioc2rpz_fun:logMessage("Zone ~p is not ready ~n", [Zone#rpz.zone_str]), 
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(121),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto,Zone#rpz.zone_str, ioc2rpz_fun:q_type(QType), "IN",dombin_to_str(TSIG#dns_TSIG_RR.name),""]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?SERVFAIL:4>>, <<1:16,0:16,0:16,0:16>>, Questions, TSIG, Proto);

%Non cachable zones
send_zone(_,Socket,{Questions,DNSId,OptB,OptE,_RH,_Rest,Zone,_QType,NSServ,MailAddr,TSIG,SOA}, Proto) ->
  SOAR = <<NSServ/binary,MailAddr/binary,(ioc2rpz_fun:curr_serial()):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOAR)):16, SOAR/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  CTime=ioc2rpz_fun:curr_serial_60(),
  case ets:match(rpz_hotcache_table,{{pkthotcache,Zone#rpz.zone,'_'},'$2','$3'}) of
    [[Timestamp,Pkt1]|REST] when CTime=<(Timestamp+?HotCacheTime) ->
      ioc2rpz_fun:logMessage("Found the zone in the hot cache~n",[]), %TODO remove debug
      Pkt = [binary_to_term(Pkt1) | [binary_to_term(X) || [_,X] <- REST]],
      %io:fwrite(group_leader(),"Zone ~p send cached ~n",[Zone#rpz.zone_str]),
      send_cached_zone(Socket, NSRec, SOAREC, TSIG, <<DNSId:2/binary ,1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>, Questions, Pkt, Proto);
    _Else ->
      %io:fwrite(group_leader(),"Zone ~p send life ~n",[Zone#rpz.zone_str]),
      send_zone_live(Socket,sendNhotcache,Zone#rpz{serial=CTime},[DNSId, <<1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>],Questions, SOAREC,NSRec,TSIG,Proto)
  end,
  ok.

send_zone_live(Socket,Op,Zone,PktH,Questions, SOAREC,NSRec,TSIG,Proto) ->
  IOC = mrpz_from_ioc(Zone,axfr),
  MD5=crypto:hash(md5,term_to_binary(IOC)),
  case {Op, Zone#rpz.ioc_md5} of
    {cache, MD5} -> {updateSOA, MD5,length(IOC)};
    _Else ->
      {ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})$"), %
      PktHLen = 12+byte_size(Questions),
      ioc2rpz_db:write_db_record(Zone,IOC,axfr),
      ioc2rpz_db:delete_old_db_record(Zone),
			T_ZIP_L=init_T_ZIP_L(Zone),
      send_packets(Socket,IOC, [], 0, 0, true, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,0,Op,0,true,Proto),
      ets:delete(T_ZIP_L),
      {ok,MD5,length(IOC)}
  end.

w_send_packets(PID, Zone) ->      
  receive 
    { ok, PID, ok } -> ok
      %ioc2rpz_fun:logMessage("Zone ~p Got message from ~p ~n",[Zone, PID])
  end.


% пустая зона
send_packets(Socket,[], [], 0, _ACount, _Zip, PktH, Questions, SOAREC,NSRec,Zone,_MP,_PktHLen,_T_ZIP_L,TSIG,PktN,DBOp,_SOANSSize,_IXFRNewR,Proto) -> %NSRec = Client SOA for IXFR
% Socket, IOCs, Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC, NSREC
  %Pkt1 = list_to_binary([PktH,<<2:16,0:16,0:16>>, Questions, SOAREC, SOAREC]),
   if (DBOp == send) or (DBOp == sendNcache) or (DBOp == sendNhotcache) or (DBOp == ixfr) ->
    %we do not expect empty IXFR response but just to be on the safe side will send to a client SOA, SOACL, SOA, SOA => empty zone update
    if (DBOp == ixfr) -> EndSOA=SOAREC, Cnt=1; true->EndSOA = <<>>,Cnt=0 end,
    if TSIG /= [] ->
      {ok,TSIGRR,_}=add_TSIG(list_to_binary([PktH, <<(3+Cnt):16,0:16,0:16>>, Questions, SOAREC, NSRec, SOAREC,EndSOA]),TSIG),
      Pkt1 = list_to_binary([PktH, <<(3+Cnt):16,0:16,1:16>>, Questions, SOAREC, NSRec, SOAREC,EndSOA, TSIGRR]); 
      true -> Pkt1 = list_to_binary([PktH,<<(3+Cnt):16,0:16,0:16>>, Questions, SOAREC, NSRec,EndSOA, SOAREC])
    end,
    %PktLen = byte_size(Pkt1),
    %Pkt2 = [<<PktLen:16>>,Pkt1], %send, cache, sendNcache, sendNhotcache
    send_dns(Socket,Pkt1, [Proto,addlen]);
    true -> ok
  end,
  %if IXFR -> пустой зоны должно не быть, но на всякий случай можно предусмотреть передачу только SOA
  if (DBOp == cache) or (DBOp == sendNcache) ->
    ioc2rpz_db:write_db_pkt(Zone, {0,3,0,0, []});
    true -> ok
  end,
  if DBOp == sendNhotcache ->
    CTime=ioc2rpz_fun:curr_serial_60(),%erlang:system_time(seconds),
    ets:insert(rpz_hotcache_table, {{pkthotcache,Zone#rpz.zone,PktN},CTime, term_to_binary({0,3,0,0, []},[{compressed,?Compression}])});
    true -> ok
  end;


send_packets(Socket,IOC, [], _ACount, _PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,0,IXFRNewR,Proto) when T_ZIP_L /= 0 -> % первый пакет
  SOANSSize = if PktN == 0 ->
    byte_size(<<SOAREC/binary,NSRec/binary>>);
    true -> 0
  end,
  %TODO split IOC by # cores and spawn for DBOp == cache
  %sequential
  %send_packets(Socket,IOC, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto);

  %concurrent
  if DBOp == cache ->
      [IOC1,IOC2]=ioc2rpz_fun:split(IOC,?IOCperProc),
      ParentPID = self(),
%      spawn_opt(ioc2rpz,send_packets,[Socket,IOC1, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC, NSRec, Zone, MP, PktHLen, 0, TSIG, PktN, DBOp, SOANSSize, IXFRNewR,Proto],[{fullsweep_after,0}]),
%  ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]) ---> init_T_ZIP_L(Zone) 
      PID=spawn_opt(fun() ->
        ParentPID ! {ok, self(), ioc2rpz:send_packets(Socket,IOC1, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC, NSRec, Zone, MP, PktHLen, init_T_ZIP_L(Zone), TSIG, PktN, DBOp, SOANSSize, IXFRNewR, Proto) }
        end
        ,[{fullsweep_after,0}]),
      %ioc2rpz_fun:logMessage("Zone ~p started ~p ~n",[Zone#rpz.zone_str, PID]),
      if IOC2 /= [] ->
        ioc2rpz:send_packets(<<>>,IOC2, [], 0, 0, true, <<>>, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,[],PktN+100,cache,0,false,Proto);
        true -> ok
      end,
      w_send_packets(PID, Zone#rpz.zone_str);
    true ->
      send_packets(Socket,IOC, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto)
  end;

%send_packets(Socket,IOC, [], _ACount, _PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,0,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) ->
%  %ioc2rpz_fun:logMessage("Zone ~p zip ~n",[Zone#rpz.zone_str]),
%  T_ZIP_L = ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]),
%  send_packets(Socket,IOC, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto);

% последний пакет, нужно отсылать
send_packets(Socket,[], Pkt, ACount, _PSize, _Zip, PktH, Questions, SOAREC,NSREC,Zone,_,_,_,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) ->
  case {PktN, IXFRNewR} of
    {0, false} -> PktF=[SOAREC,NSREC,Pkt,SOAREC], Cnt=4;
    {0, true} -> PktF=[SOAREC,NSREC,Pkt], Cnt=3;
    _Else -> PktF=Pkt, Cnt=1
  end,
  %ioc2rpz_fun:logMessage("Zone ~p, Last packet ACOUNT ~p, packets ~p ~n",[Zone#rpz.zone_str,ACount,(PktN+1)]), %TODO Debug
  if (DBOp == send) or (DBOp == sendNcache) or (DBOp == sendNhotcache) or (DBOp == ixfr) ->
    if TSIG /= [] ->
      {ok,TSIGRR,_}=add_TSIG(list_to_binary([PktH, <<(ACount+Cnt):16,0:16,0:16>>, Questions, PktF, SOAREC]),TSIG),
      Pkt1 = list_to_binary([PktH, <<(ACount+Cnt):16,0:16,1:16>>, Questions, PktF,SOAREC, TSIGRR]);
      true -> Pkt1 = list_to_binary([PktH,<<(ACount+Cnt):16,0:16,0:16>>, Questions, PktF, SOAREC])
    end,
    send_dns(Socket,Pkt1, [Proto,addlen]);
    true -> ok
  end,
  if (DBOp == cache) or (DBOp == sendNcache) ->
    ioc2rpz_db:write_db_pkt(Zone, {PktN,ACount,0,0, Pkt});
    true -> ok
  end,
  if DBOp == sendNhotcache ->
    CTime=ioc2rpz_fun:curr_serial_60(),
    ets:insert(rpz_hotcache_table, {{pkthotcache,Zone#rpz.zone,PktN},CTime, term_to_binary({PktN,ACount,0,0, Pkt},[{compressed,?Compression}])}); %2019-06-13 BUG in live zones # of records ACount+Cnt
    true -> ok
  end;

% превышен размер пакета, нужно отсылать
send_packets(Socket,Tail, Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) when PSize > ?DNSPktMax ->
  if PktN == 0 -> Pkt0=[SOAREC, NSRec, Pkt],SOANSSize0=0, Cnt=2; true -> Pkt0=Pkt,SOANSSize0=SOANSSize, Cnt=0 end,
  if (TSIG /= []) and ((DBOp == send) or (DBOp == sendNcache) or (DBOp == sendNhotcache) or (DBOp == ixfr)) ->
    {ok,TSIGRR,TSIG1}=add_TSIG(list_to_binary([PktH, <<(ACount+Cnt):16,0:16,0:16>>, Questions, Pkt0]),TSIG),
    Pkt1 = list_to_binary([PktH, <<(ACount+Cnt):16,0:16,1:16>>, Questions, Pkt0, TSIGRR]);
    true -> Pkt1 = list_to_binary([PktH,<<(ACount+Cnt):16,0:16,0:16>>, Questions, Pkt0]), TSIG1=TSIG
  end,
  PktLen = byte_size(Pkt1),
  Pkt2 = [<<PktLen:16>>,Pkt1],
  if (DBOp == send) or (DBOp == sendNcache) or (DBOp == sendNhotcache) or (DBOp == ixfr) ->
    %send_dns_tcp(Socket,Pkt2, []);
    send_dns(Socket,Pkt2, [Proto,[]]);
    true -> ok
  end,
  if (DBOp == cache) or (DBOp == sendNcache) ->
    ioc2rpz_db:write_db_pkt(Zone, {PktN,ACount,0,0, Pkt});
    true -> ok
  end,
  if DBOp == sendNhotcache ->
    CTime=ioc2rpz_fun:curr_serial_60(),
    ets:insert(rpz_hotcache_table, {{pkthotcache,Zone#rpz.zone,PktN},CTime, term_to_binary({PktN,ACount,0,0, Pkt},[{compressed,?Compression}])});
    true -> ok
  end,
  ets:delete_all_objects(T_ZIP_L),
  %ets:delete(T_ZIP_L),
	%T_ZIP_L1=init_T_ZIP_L(Zone),
  send_packets(Socket,Tail, <<>> , 0, 0, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG1,PktN+1,DBOp,SOANSSize0,IXFRNewR,Proto);

send_packets(Socket,[{IOC,IOCExp}|Tail], Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) when ((byte_size(IOC)+byte_size(Zone#rpz.zone))>=253) ->
  send_packets(Socket,Tail, Pkt , ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto);

send_packets(Socket,[{IOC,IOCExp}|Tail], Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) when Zone#rpz.ioc_type  == <<"ip">> ->
  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> SOASize=byte_size(SOAREC); true -> SOASize=0 end,
  if (IOCExp>Zone#rpz.serial) or (IOCExp==0) or (DBOp == ixfr) ->
      {ok, Cnt, Rules,_} = gen_rpzrule(reverse_IP(IOC),Zone,?TTL,<<"false">>,<<"ip">>,Zone#rpz.action,PktHLen+PSize+SOASize,T_ZIP_L); % Zone#rpz.zone_str - need t
      true -> Cnt=0, Rules=[]
  end,
  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> Rules1 = [SOAREC | Rules], Cnt1=Cnt+1, IXFRNewR1 = true; true -> Rules1=Rules, Cnt1=Cnt, IXFRNewR1 = IXFRNewR end,
  Pkt1 = list_to_binary([Pkt, Rules1]),
  PSize1 = byte_size(Pkt1)+SOANSSize,
  send_packets(Socket,Tail, Pkt1 , ACount+Cnt1, PSize1, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR1,Proto);

send_packets(Socket,[{IOC,IOCExp}|Tail], Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) when Zone#rpz.ioc_type == <<"fqdn">> -> % докидываем записи и пересчитываем размеры
  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> SOASize=byte_size(SOAREC); true -> SOASize=0 end,

  if (IOCExp>Zone#rpz.serial) or (IOCExp==0) or (DBOp == ixfr) ->
%      ioc2rpz_fun:logMessage("Domain ~p, Action ~p ~n",[IOC,Zone#rpz.action]), %TODO debug
      {ok, _, Rules, WRules} = gen_rpzrule(list_to_binary([IOC,".",Zone#rpz.zone_str,"."]),Zone,?TTL,<<"false">>,Zone#rpz.action,[],PktHLen+PSize+SOASize,T_ZIP_L), %LocData=последний[] % Zone#rpz.zone_str
      {ok,Rules2,Cnt}=gen_wildcard(Zone#rpz.wildcards,Rules, WRules,PSize+PktHLen+SOASize);
      true -> Cnt=0, Rules=[], Rules2=[]
  end,

  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> Rules1 = [SOAREC | Rules], Cnt1=Cnt+1, IXFRNewR1 = true; true -> Rules1=Rules, Cnt1=Cnt, IXFRNewR1 = IXFRNewR end,
  Pkt1 = list_to_binary([Pkt, Rules1, Rules2]),
  PSize1 = byte_size(Pkt1)+SOANSSize,
  send_packets(Socket,Tail,Pkt1,ACount+Cnt1,PSize1, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR1,Proto);

send_packets(Socket,[{IOC,IOCExp}|Tail], Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) -> % докидываем записи и пересчитываем размеры
  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> SOASize=byte_size(SOAREC); true -> SOASize=0 end,
  %ioc2rpz_fun:logMessage("Check ~p ~p ~p ~p ~p ~n",[Zone#rpz.zone_str, IOC,IOCExp,Zone#rpz.serial,DBOp]),
  if (IOCExp>Zone#rpz.serial) or (IOCExp==0) or (DBOp == ixfr)  ->
      case re:run(IOC,MP,[global,notempty,{capture,[1],binary}]) of
        {match,_} -> {ok, Cnt, Rules, WRules} = gen_rpzrule(reverse_IP(IOC),Zone,?TTL,<<"false">>,<<"ip">>,Zone#rpz.action,PktHLen+PSize+SOASize,T_ZIP_L), Rules2=[];% Cnt=1;
        _ -> {ok, _, Rules, WRules} = gen_rpzrule(list_to_binary([IOC,".",Zone#rpz.zone_str,"."]),Zone,?TTL,<<"false">>,Zone#rpz.action,[],PktHLen+PSize+SOASize,T_ZIP_L), {ok,Rules2,Cnt}=gen_wildcard(Zone#rpz.wildcards,Rules, WRules,PSize+PktHLen) % Zone#rpz.zone_str
      end;
      true -> Cnt=0, Rules=[], Rules2=[]
  end,
 
  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> Rules1 = [SOAREC | Rules], Cnt1=Cnt+1, IXFRNewR1 = true; true -> Rules1=Rules, Cnt1=Cnt, IXFRNewR1 = IXFRNewR end,
  Pkt1 = list_to_binary([Pkt, Rules1, Rules2]),
  PSize1 = byte_size(Pkt1)+SOANSSize,
  send_packets(Socket,Tail, Pkt1 , ACount+Cnt1, PSize1, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR1,Proto).

gen_wildcard(WCards, [Rules|RESTR], [WRules|RESTWR], PSize) ->
  {ok,Rul1,Cnt1}=gen_wildcard(WCards, Rules, WRules, PSize),
  {ok,Rul2,Cnt2}=gen_wildcard(WCards, RESTR, RESTWR, PSize),
  {ok,[Rul1|Rul2],Cnt1+Cnt2};

gen_wildcard(WCards, [], [], PSize) ->
  {ok,[],0};

gen_wildcard(<<"true">>, _Rules, WRules, PSize) when PSize < 16#3FFF ->
  R1Loc=(16#c000 bor PSize),
  Rules2=[<<1,"*",R1Loc:16>>, WRules],
  {ok,Rules2,2};

gen_wildcard(<<"true">>, Rules, _WRules, PSize) when PSize >= 16#3FFF ->
  {ok,[<<1,"*">>,Rules],2};

gen_wildcard(<<"false">>, _Rules, _WRules, _PSize) ->
  {ok,[],1}.

remove_WL(IOC,WL) when WL == [] ->
  ordsets:to_list(ordsets:from_list(IOC));

remove_WL(IOC,WL) ->
%TODO do not check expiration date, but save the largest expiration
%  ordsets:to_list(ordsets:subtract(ordsets:from_list(IOC), ordsets:from_list(WL))).
%медлеенее в 2 раза
  WLSet = gb_sets:from_list(WL),
  [X || {E,Exp} = X <- IOC, not gb_sets:is_element(E, WLSet)]. % TODO duplicates gb_sets vs ordsets

mrpz_from_ioc(Zone,UType) -> %Zone - RPZ zone
  remove_WL(mrpz_from_ioc(Zone#rpz.sources,Zone,UType,[]),mrpz_from_ioc(Zone#rpz.whitelist,Zone,axfr,[])).% -- WL.

mrpz_from_ioc([SRC|REST], RPZ,UType, IOC) -> %List of the sources, RPZ zone, UType - AXFR/IXFR update type, IOC - list of accumulated IOCs
  CTime=RPZ#rpz.serial, %CTime=ioc2rpz_fun:curr_serial(),
  [[Source]]=ets:match(cfg_table,{[source,SRC],'$2'}),
   case {ets:match(rpz_hotcache_table,{{SRC,UType},'$2','$3'}),UType} of
    {[[Timestamp,IOCZip]],axfr} when CTime=<(Timestamp+?HotCacheTime) ->
      IOC1=binary_to_term(IOCZip),
      ioc2rpz_fun:logMessage("Got source ~p from cache~n",[SRC]); %TODO debug
    {[[_Timestamp,_IOCZip]],axfr} ->
      ioc2rpz_fun:logMessage("Source  ~p was expired in cache~n",[SRC]), %TODO debug
      ets:delete(rpz_hotcache_table,{SRC,UType}),
      IOC1=ioc2rpz_conn:get_ioc(list_to_binary(Source#source.axfr_url),Source#source.regex,Source),

%%%TODO check garbage_collect
%      ioc2rpz_fun:logMessage("Memory total ~p before garbage collector. processes ~p binary ~p ~n",[erlang:memory(total)/1024/1024,erlang:memory(processes)/1024/1024,erlang:memory(binary)/1024/1024]), %TODO debug
%      erlang:garbage_collect(),
%      ioc2rpz_fun:logMessage("Memory total ~p after garbage collector. processes ~p binary ~p ~n",[erlang:memory(total)/1024/1024,erlang:memory(processes),erlang:memory(binary)]), %TODO debug

      ets:insert(rpz_hotcache_table, {{SRC,UType},CTime, term_to_binary(IOC1,[{compressed,?Compression}])});
    {[],axfr} ->
      ioc2rpz_fun:logMessage("Source ~p was not cached~n",[SRC]), %TODO debug
      IOC1=ioc2rpz_conn:get_ioc(list_to_binary(Source#source.axfr_url),Source#source.regex,Source),
      ioc2rpz_fun:logMessage("Memory total ~p before garbage collector. processes ~p binary ~p ~n",[erlang:memory(total)/1024/1024,erlang:memory(processes)/1024/1024,erlang:memory(binary)/1024/1024]), %TODO debug
      erlang:garbage_collect(),
      ioc2rpz_fun:logMessage("Memory total ~p after garbage collector. processes ~p binary ~p ~n",[erlang:memory(total)/1024/1024,erlang:memory(processes)/1024/1024,erlang:memory(binary)/1024/1024]), %TODO debug

      ets:insert(rpz_hotcache_table, {{SRC,UType},CTime, term_to_binary(IOC1,[{compressed,?Compression}])});
    {[[Timestamp,IOCZip]],ixfr} when CTime=<(Timestamp+?HotCacheTimeIXFR) ->
      IOC1=binary_to_term(IOCZip),
      ioc2rpz_fun:logMessage("Got source ~p IXFR from cache ~n",[SRC]); %TODO debug
    {_,ixfr} ->
      %ioc2rpz_fun:logMessage("IXFR request for ~p is not cached by the design~n",[SRC]), %TODO debug
      IOC1=ioc2rpz_conn:get_ioc(list_to_binary(ioc2rpz_fun:constr_ixfr_url(Source#source.ixfr_url,RPZ#rpz.ixfr_update_time,CTime)),Source#source.regex,Source),

      ioc2rpz_fun:logMessage("Memory total ~p before garbage collector. processes ~p binary ~p ~n",[erlang:memory(total)/1024/1024,erlang:memory(processes)/1024/1024,erlang:memory(binary)/1024/1024]), %TODO debug
      erlang:garbage_collect(),
      ioc2rpz_fun:logMessage("Memory total ~p after garbage collector. processes ~p binary ~p ~n",[erlang:memory(total)/1024/1024,erlang:memory(processes)/1024/1024,erlang:memory(binary)/1024/1024]) %TODO debug

  end,
  ets:update_element(cfg_table, [source,SRC], [{2, Source#source{ioc_count=length(IOC1)}}]),
  mrpz_from_ioc(REST,RPZ,UType,IOC1 ++ IOC);

mrpz_from_ioc([],RPZ,_UType,IOC) ->
  IOC.

%Данная ветка спользуется только для sample zone
gen_rpzrule(Domain,RPZ,TTL,<<"true">>,Action, LocData,PktHLen,T_ZIP_L) -> %wildcard = true
  {ok,Cnt1,Pkt1,WPkt1} = gen_rpzrule(Domain,RPZ,TTL,<<"false">>,Action, LocData,PktHLen,T_ZIP_L),
  {ok,Cnt2,Pkt2,WPkt2} = gen_rpzrule(<<"*.",Domain/binary>>,RPZ,TTL,<<"false">>,Action, LocData,PktHLen+byte_size(list_to_binary(Pkt1)),T_ZIP_L),

%TODO switchto wildcards
%	{ok, Cnt1, Pkt1, WPkt1} = gen_rpzrule(Domain,RPZ,?TTL,<<"false">>,Action,[],PktHLen,T_ZIP_L), %LocData=последний[] % Zone#rpz.zone_str
%	{ok,Pkt2,Cnt2}=gen_wildcard(<<"true">>,Pkt1, WPkt1,PktHLen),

  {ok,Cnt1+Cnt2,[Pkt1,Pkt2],[]};


gen_rpzrule(Domain,RPZ,TTL,<<"false">>,<<"nxdomain">>,[],PktHLen,T_ZIP_L) -> %wildcard = false
  case domstr_to_bin_zip(Domain,PktHLen,T_ZIP_L) of
    {error, _} ->
      {ok,0,[],[]};
    {_,BDomain} -> %ok, zip
      {ok,1,[list_to_binary([BDomain,<<?T_CNAME:16,?C_IN:16, TTL:32,1:16,0>>])],[<<?T_CNAME:16,?C_IN:16, TTL:32,1:16,0>>]}
  end;

gen_rpzrule(Domain,RPZ,TTL,<<"false">>,Action,_,PktHLen,T_ZIP_L) when Action==<<"nodata">>;Action==<<"passthru">>;Action==<<"drop">>;Action==<<"tcp-only">> -> %wildcard = false
%  ioc2rpz_fun:logMessage("Domain ~p, Action ~p ~n",[Domain,Action]), %TODO debug
  LocData = case Action of
    <<"nodata">> -> <<"*">>;
    <<"passthru">> -> <<"rpz-passthru">>;
    <<"drop">> -> <<"rpz-drop">>;
    <<"tcp-only">> -> <<"rpz-tcp-only">>
  end,
  case domstr_to_bin_zip(Domain,PktHLen,T_ZIP_L) of
    {error, _} ->
      {ok,0,[],[]};
    {_,BDomain} -> %ok, zip
      {_,LocDataZ} = domstr_to_bin_zip(LocData,0,PktHLen+10+byte_size(BDomain),T_ZIP_L), % what is 10??
%			{_,LocDataZ} = domstr_to_bin(LocData,0),
      ELDS=byte_size(LocDataZ),
      {ok,1,[list_to_binary([BDomain,<<?T_CNAME:16,?C_IN:16, TTL:32,ELDS:16>>,LocDataZ])],[list_to_binary([<<?T_CNAME:16,?C_IN:16, TTL:32,ELDS:16>>,LocDataZ])]}
  end;

gen_rpzrule(Domain,RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"redirect_domain">>;Action==<<"local_cname">> ->
  case domstr_to_bin_zip(Domain,PktHLen,T_ZIP_L) of
    {error, _} ->
      {ok,0,[],[]};
    {_,BDomain} -> %ok, zip
     {_,LocDataZ} = domstr_to_bin_zip(LocData,0,PktHLen+10+byte_size(BDomain),T_ZIP_L),
%     {_,LocDataZ} = domstr_to_bin(LocData,0),
      ELDS=byte_size(LocDataZ),
      {ok,1,[list_to_binary([BDomain,<<?T_CNAME:16,?C_IN:16, TTL:32,ELDS:16>>,LocDataZ])],[list_to_binary([<<?T_CNAME:16,?C_IN:16, TTL:32,ELDS:16>>,LocDataZ])]}
  end;


gen_rpzrule(Domain,RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"redirect_ip">>;Action==<<"local_a">>;Action==<<"local_aaaa">> -> %wildcard = false
  Len = byte_size(LocData),
  RType = if Len == 16 -> ?T_AAAA; true -> ?T_A end,
  %ioc2rpz_fun:logMessage("Domain ~p, Action ~p, LocData ~p ~n",[Domain,Action,LocData]), %TODO debug
  case domstr_to_bin_zip(Domain,PktHLen,T_ZIP_L) of
    {error, _} ->
      {ok,0,[],[]};
    {_,BDomain} -> %ok, zip
      {ok,1,[list_to_binary([BDomain,<<RType:16,?C_IN:16, TTL:32,Len:16>>,LocData])],[list_to_binary([<<RType:16,?C_IN:16, TTL:32,Len:16>>,LocData])]}
  end;

gen_rpzrule(Domain,RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"local_txt">> ->
  case Action of
    <<"local_txt">> -> RType = ?T_TXT
  end,
  Len = byte_size(LocData),
  %ioc2rpz_fun:logMessage("Domain ~p, Action ~p, LocData ~p ~n",[Domain,Action,LocData]), %TODO debug
  case domstr_to_bin_zip(Domain,PktHLen,T_ZIP_L) of
    {error, _} ->
      {ok,0,[],[]};
    {_,BDomain} -> %ok, zip
      {ok,1,[list_to_binary([BDomain,<<RType:16,?C_IN:16, TTL:32,Len:16>>,LocData])],[list_to_binary([<<RType:16,?C_IN:16, TTL:32,Len:16>>,LocData])]}
  end;
%gen_rpzrule(Domain,RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"local_srv">> -> %TODO
%gen_rpzrule(Domain,RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"local_mx">> -> %TODO
%gen_rpzrule(Domain,RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"local_ptr">> -> %TODO
%gen_rpzrule(Domain,RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"local_naptr">> -> %TODO


%Generate multiple rules per action
gen_rpzrule(Domain,RPZ,TTL,<<"false">>,[Action|REST],_,PktHLen,T_ZIP_L) ->
  {ok,Cnt1,Pkt1,WPkt1} = gen_rpzrule(Domain,RPZ,TTL,<<"false">>,Action,[],PktHLen,T_ZIP_L),
  {ok,Cnt2,Pkt2,WPkt2} = gen_rpzrule(Domain,RPZ,TTL,<<"false">>,REST,[],(PktHLen+byte_size(list_to_binary(Pkt1))),T_ZIP_L),
  {ok,Cnt1+Cnt2,[Pkt1|Pkt2],[WPkt1|WPkt2]};


gen_rpzrule(Domain,RPZ,TTL,<<"false">>,[],_,PktHLen,T_ZIP_L) ->
  {ok,0,[],[]};


gen_rpzrule(Domain,RPZ,TTL,<<"false">>,RType,Action,PktHLen,T_ZIP_L) when RType==<<"ip">>;RType==<<"nsdname">>;RType==<<"nsip">> -> %wildcard = false
  BDomain=list_to_binary([Domain,
    case RType of
      <<"ip">> -> ".rpz-ip.";
      <<"nsdname">> -> ".rpz-nsdname.";
      <<"nsip">> -> ".rpz-nsip."
    end
  ,RPZ#rpz.zone_str]),
  %LAction = case Action of
  %  Action when is_binary(Action) -> Action;
  %  _Else -> <<"nxdomain">>
  %end,
  {_, Cnt , Pkt, WPkt} = gen_rpzrule(BDomain,RPZ,TTL,<<"false">>,Action,[],PktHLen,T_ZIP_L),
  {ok,Cnt,[Pkt], [WPkt]};

gen_rpzrule(Domain,_,_,_,Action,_,_,_) ->
  ioc2rpz_fun:logMessage("Error. Unsupported rule. Domain ~p, Action ~p ~n",[Domain,Action]), %TODO debug
  {ok,0,[<<>>],[<<>>]}.


reverse_IP(OrigIP) when OrigIP == <<"::1">>;OrigIP == <<"::1/128">> ->
  <<"128.1:zz">>;

reverse_IP(OrigIP) ->
  [IP|Mask] = ioc2rpz_fun:split_tail(OrigIP, <<"/">>),
  case {ioc2rpz_fun:split_tail(IP, <<".">>),Mask} of
    {[IP1,IP2,IP3,IP4],[]} ->
      list_to_binary([<<"32",".">>,IP4,<<".">>,IP3,<<".">>,IP2,<<".">>,IP1]);
    {[IP1,IP2,IP3,IP4],[IMask]} ->
      list_to_binary([IMask,<<".">>,IP4,<<".">>,IP3,<<".">>,IP2,<<".">>,IP1]);
      _ ->
        reverse_IP6(<<>>,string:split(IP, ":",all),Mask,[])
  end.

reverse_IP6(<<>>,OrigIP,[],_) ->
  IPv6=reverse_IP6(<<>>,OrigIP,no),
  <<"128.", IPv6/binary>>;
reverse_IP6(<<>>,OrigIP,[Mask],_) ->
  IPv6=reverse_IP6(<<>>,OrigIP,no),
  <<Mask/binary, ".", IPv6/binary>>.

reverse_IP6(<<>>,[DIP|TAIL],ZZ) ->
  reverse_IP6(DIP,TAIL,ZZ);
reverse_IP6(RIP,[],_ZZ) ->
  list_to_binary(RIP);
reverse_IP6(RIP,[<<>>|TAIL],ZZ) when ZZ == no ->
  reverse_IP6([<<"zz.">>,RIP],TAIL,yes);
reverse_IP6(RIP,[<<>>|TAIL],ZZ) when ZZ == yes ->
  reverse_IP6(RIP,TAIL,ZZ);
reverse_IP6(RIP,[DIP|TAIL],ZZ) ->
  reverse_IP6([DIP,<<".">>,RIP],TAIL,ZZ).

domstr_to_bin(Str) ->
  domstr_to_bin(ioc2rpz_fun:split_tail(Str, <<".">>),1,<<>>).
domstr_to_bin(Str,Zero) ->
  domstr_to_bin(ioc2rpz_fun:split_tail(Str, <<".">>),Zero,<<>>).
domstr_to_bin([Head|Rest],Zero,Bin) when byte_size(Head) =< 63->
  CLen = byte_size(Head),
  domstr_to_bin(Rest,Zero,[Bin,CLen,Head]);
domstr_to_bin([Head|_Rest],_Zero,_Bin) when byte_size(Head) > 63->
  {error,[]};

domstr_to_bin([],0,Bin) ->
  {ok,list_to_binary([Bin,0])};
domstr_to_bin([],_,Bin) ->
  {ok,list_to_binary([Bin])}.


domstr_to_bin_zip(Str,Pos,T_ZIP_L) when byte_size(Str) > 2->
  domstr_to_bin_zip(Str,1,Pos,T_ZIP_L);
domstr_to_bin_zip(Str,_Pos,_T_ZIP_L) ->
  domstr_to_bin(Str,0).

domstr_to_bin_zip(Str,Zero,Pos,T_ZIP_L) ->
  Labels = ioc2rpz_fun:split_tail(Str, <<".">>),
  case ets:lookup(T_ZIP_L, Labels)  of
    [{_,Offset}] -> BOffset = (16#c000 bor Offset), {zip, <<BOffset:16>>};
    [] when Pos =< 16#3FFF -> ets:insert(T_ZIP_L, {Labels, Pos}), %insert_new
          domstr_to_bin_zip(Labels,Zero,<<>>,Pos,T_ZIP_L);
    [] when Pos > 16#3FFF -> domstr_to_bin_zip(Labels,Zero,<<>>,Pos,T_ZIP_L)
  end.

domstr_to_bin_zip([Head|Rest],Zero,Bin,Pos,T_ZIP_L) when byte_size(Head) =< 63 ->
  CLen = byte_size(Head),
  NewPos=Pos+CLen+1,
  case {ets:lookup(T_ZIP_L, Rest), Rest}  of
    {[{_,Offset}],_} -> BOffset = (16#c000 bor Offset), {zip,list_to_binary([Bin,CLen,Head, <<BOffset:16>>])};
    {[],Rest} when Rest == 0; NewPos > 16#3FFF -> domstr_to_bin_zip(Rest,Zero,[Bin,CLen,Head],NewPos,T_ZIP_L);
    {[],[]} -> domstr_to_bin_zip(Rest,Zero,[Bin,CLen,Head],NewPos,T_ZIP_L);
    {[],Rest} -> ets:insert(T_ZIP_L, {Rest, NewPos}), %insert_new
          domstr_to_bin_zip(Rest,Zero,[Bin,CLen,Head],NewPos,T_ZIP_L)
  end;

domstr_to_bin_zip([Head|_Rest],_Zero,_Bin,_Pos,_T_ZIP_L) when byte_size(Head) > 63->
  {error,[]};

domstr_to_bin_zip([],0,Bin,_Pos,_T_ZIP_L) ->
  {ok,list_to_binary([Bin,0])};
domstr_to_bin_zip([],_Zero,Bin,_Pos,_T_ZIP_L) ->
  {ok,list_to_binary([Bin])}.



dombin_to_str(Dom) ->
  dombin_to_str(<<"">>,Dom).
dombin_to_str(Dom,<<>>) ->
  Dom;
dombin_to_str(Dom,<<Len:8,Rest/binary>>) when (Len > 63) or (byte_size(Rest)<Len) ->
  Dom;
dombin_to_str(<<"">>,<<Len:8,Rest/binary>>) when (Len =< 63) and (byte_size(Rest)>=Len)  ->
  <<Dom1:Len/bytes,Rest1/binary>> = Rest,
  dombin_to_str(binary_to_list(Dom1),Rest1);
dombin_to_str(Dom,<<Len:8,Rest/binary>>) when (Len =< 63) and (byte_size(Rest)>=Len) ->
  <<Dom1:Len/bytes,Rest1/binary>> = Rest,
  dombin_to_str(Dom++"."++binary_to_list(Dom1),Rest1).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B ", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]);
hexstr_to_bin([X|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X,"0"])),
  hexstr_to_bin(T, [V | Acc]).

%%% Convert internal IP representation to a string 
ip_to_str({0,0,0,0,0,65535,IP1,IP2}) ->
  <<IP1B1:8, IP1B2:8>> = <<IP1:16>>,
  <<IP2B1:8, IP2B2:8>> = <<IP2:16>>,
  %?logDebugMSG("~p:~p ~p.~p.~p.~p~n",[IP1,IP2,IP1B1,IP1B2,IP2B1,IP2B2]),
  inet_parse:ntoa({IP1B1,IP1B2,IP2B1,IP2B2});
  
ip_to_str(IP) ->
  %?logDebugMSG("~p~n",[IP]),
  inet_parse:ntoa(IP).


init_T_ZIP_L(Zone) ->
	T_ZIP_L=ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]),
	Labels = ioc2rpz_fun:split_tail(list_to_binary(Zone#rpz.zone_str), <<".">>),
	ets:insert(T_ZIP_L, {Labels, ?ZNameZipN}),
	T_ZIP_L.

