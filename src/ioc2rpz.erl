%Copyright 2017-2025 Vadim Pavlov ioc2rpz[at]gmail[.]com
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

%% @doc IOC2RPZ TCP/TLS Worker Module.
%%
%% This module implements a gen_server-based worker for accepting and processing
%% DNS queries over TCP and TLS (DNS over TLS / DoT) connections. Each worker
%% is spawned by a simple_one_for_one supervisor and handles a single connection
%% lifecycle: accept, receive DNS query, process, respond, and terminate.
%%
%% The module also contains core DNS processing logic including RPZ zone transfers
%% (AXFR/IXFR), TSIG authentication, SOA responses, rate limiting integration,
%% RPZ rule generation, DNS notify, and IOC-to-RPZ conversion.
%% @end

-module(ioc2rpz).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

-include_lib("ioc2rpz.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_ioc2rpz/2,send_notify/1,send_notify/5,send_packets/20,domstr_to_bin/2,send_zone_live/9,mrpz_from_ioc/2,parse_dns_request/3,ip_to_str/1,dombin_to_str/1,reverse_IP/1,mrpz_from_ioc/4]).


%-compile([export_all]).

%% @doc Starts a linked gen_server worker for a given listen socket.
%% @param Socket The listen socket (TCP or TLS) to accept connections on.
%% @param Params A list `[Pid, Proc, TLS]' where Pid is the supervisor PID,
%%        Proc is the process supervisor name, and TLS is `yes' or `no'.
%% @returns `{ok, Pid}' on success.
start_ioc2rpz(Socket,Params) ->
  gen_server:start_link(?MODULE, [Socket,Params], []).

%% @doc Initializes the gen_server worker state and triggers an async accept.
%% Immediately casts an `accept' message to self to begin the accept loop.
%% @param Args A list `[Socket, [Pid, Proc, TLS]]'.
%% @returns `{ok, #state{}}' with the listen socket and connection parameters.
init([Socket,[Pid,Proc,TLS]]) ->
  ?logDebugMSG("ioc2rpz ~p child started ~n", [Proc]),
  gen_server:cast(self(), accept),
  {ok, #state{socket=Socket, tls=TLS, params=[Pid,Proc]}}.

%%%TCP accept
%handle_cast(accept, State = #state{socket=ListenSocket, tls=no, params=[Pid,Proc]}) ->
%  {Respond, AcceptSocket} = case gen_tcp:accept(ListenSocket) of
%    {ok, ASocket} -> {noreply, ASocket};
%    {error, Reason} -> ioc2rpz_fun:logMessage("~p:~p:~p. TCP accept error: ~p ~n",[?MODULE, ?FUNCTION_NAME, ?LINE, Reason]), {stop, ListenSocket}
%  end,
%  ioc2rpz_proc_sup:start_socket(Proc),
%  {Respond, State#state{socket=AcceptSocket, tls=no, params=[Pid,Proc]}};
%%%TLS accept
%handle_cast(accept, State = #state{socket=ListenSocket, tls=yes, params=[Pid,Proc]}) ->
%  case ssl:transport_accept(ListenSocket) of
%    {ok, TLSTransportSocket} ->
%        {Respond, AcceptSocket} = case ssl:handshake(TLSTransportSocket) of
%          {ok, ASocket} -> {noreply, ASocket};
%          {error, Reason} -> ioc2rpz_fun:logMessage("~p:~p:~p. TLS accept error: ~p ~n",[?MODULE, ?FUNCTION_NAME, ?LINE, Reason]), {stop, ListenSocket}
%        end;
%    {error, Reason} -> ioc2rpz_fun:logMessage("~p:~p:~p. TLS accept error: ~p ~n",[?MODULE, ?FUNCTION_NAME, ?LINE, Reason]), {Respond, AcceptSocket} = {stop, ListenSocket}
%  end,
%  %% Boot a new listener to replace this one.
%  ioc2rpz_proc_sup:start_socket(Proc),
%  {Respond, State#state{socket=AcceptSocket, tls=yes, params=[Pid,Proc]}};

%%% Improved accept handling 2025-01-07. In case of a error, why we don't start a new socket?
%%%TCP accept
%% @doc Handles the `accept' cast for TCP (non-TLS) connections.
%% Blocks on `gen_tcp:accept/1' waiting for an incoming TCP connection.
%% On success, spawns a replacement listener via `ioc2rpz_proc_sup:start_socket/1'
%% and transitions to the connected state. On error, logs the reason and stops
%% the worker process.
%% @param State The gen_server state containing the listen socket.
%% @returns `{noreply, State}' on successful accept, `{stop, Reason, State}' on error.
handle_cast(accept, State = #state{socket=ListenSocket, tls=no, params=[Pid,Proc]}) ->
  case gen_tcp:accept(ListenSocket) of
      {ok, AcceptSocket} ->
          ioc2rpz_proc_sup:start_socket(Proc), % Start a new listener immediately
          {noreply, State#state{socket=AcceptSocket, tls=no, params=[Pid,Proc]}};
      {error, closed} ->
          %% Listen socket is dead — do NOT spawn a replacement worker.
          ioc2rpz_fun:logMessage("~p:~p:~p. TCP listen socket closed, worker exiting ~n",
                               [?MODULE, ?FUNCTION_NAME, ?LINE]),
          {stop, {shutdown, listen_socket_closed}, State};
      {error, Reason} ->
          ioc2rpz_fun:logMessage("~p:~p:~p. TCP accept error: ~p ~n",
                               [?MODULE, ?FUNCTION_NAME, ?LINE, Reason]),
          ioc2rpz_proc_sup:start_socket(Proc),
          {stop, normal, State}
  end;

%%%TLS accept
%% @doc Handles the `accept' cast for TLS (DoT) connections.
%% Performs a two-phase accept: first `ssl:transport_accept/1' to accept the
%% raw TCP connection, then `ssl:handshake/2' to complete the TLS negotiation.
%% On success, spawns a replacement listener. On handshake or accept failure,
%% logs the error, closes the transport socket if applicable, spawns a
%% replacement listener, and stops the worker.
%% @param State The gen_server state containing the TLS listen socket.
%% @returns `{noreply, State}' on success, `{stop, Reason, State}' on error.
handle_cast(accept, State = #state{socket=ListenSocket, tls=yes, params=[Pid,Proc]}) ->
  case ssl:transport_accept(ListenSocket) of
      {ok, TLSTransportSocket} ->
          case ssl:handshake(TLSTransportSocket, 5000) of
              {ok, AcceptSocket} ->
                  ioc2rpz_proc_sup:start_socket(Proc),
                  {noreply, State#state{socket=AcceptSocket, tls=yes, params=[Pid,Proc]}};
              {error, HandshakeReason} ->
                  ioc2rpz_fun:logMessage("~p:~p:~p. TLS handshake error: ~p ~n",
                                        [?MODULE, ?FUNCTION_NAME, ?LINE, HandshakeReason]),
                  ssl:close(TLSTransportSocket),
                  ioc2rpz_proc_sup:start_socket(Proc),
                  {stop, normal, State}
          end;
      {error, closed} ->
          %% Listen socket is dead — do NOT spawn a replacement worker.
          %% Let this worker die so the supervisor chain can restart the listener.
          ioc2rpz_fun:logMessage("~p:~p:~p. TLS listen socket closed, worker exiting ~n",
                                [?MODULE, ?FUNCTION_NAME, ?LINE]),
          {stop, {shutdown, listen_socket_closed}, State};
      {error, AcceptReason} ->
          ioc2rpz_fun:logMessage("~p:~p:~p. TLS accept error: ~p ~n",
                                [?MODULE, ?FUNCTION_NAME, ?LINE, AcceptReason]),
          ioc2rpz_proc_sup:start_socket(Proc),
          {stop, normal, State}
  end;

handle_cast(_, State) ->
  {noreply, State}.

%% @doc Handles incoming TCP data from an accepted connection.
%% Strips the 2-byte DNS TCP length prefix, resolves the remote peer address,
%% dispatches the DNS query to `parse_dns_request/3', closes the socket, and
%% stops the worker process (one query per connection).
%% @param Socket The accepted TCP connection socket.
%% @param Pkt The raw TCP data including the 2-byte length prefix.
%% @param State The gen_server state.
%% @returns `{stop, normal, State}' after processing and closing the socket.
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
%  ok = gen_tcp:close(Socket),
  case gen_tcp:close(Socket) of % Improved closing
    ok -> ok;
    {error, Reason} -> ioc2rpz_fun:logMessage("~p:~p:~p. TCP close error: ~p ~n", [?MODULE, ?FUNCTION_NAME, ?LINE, Reason])
  end,
  %  fprof:trace(stop),
  {stop, normal , State}; 

%% @doc Handles incoming TLS data from an accepted DoT connection.
%% Strips the 2-byte DNS TCP length prefix, resolves the remote peer address
%% via `ssl:peername/1', dispatches the DNS query to `parse_dns_request/3',
%% closes the TLS socket, and stops the worker process.
%% @param Socket The accepted TLS connection socket.
%% @param Pkt The raw TLS data including the 2-byte length prefix.
%% @param State The gen_server state.
%% @returns `{stop, normal, State}' after processing and closing the socket.
handle_info({ssl, Socket, <<_:2/binary,Pkt1/binary>>=_Pkt}, State = #state{socket=_ListenSocket, params=_Params}) ->
%  fprof:trace(start),
  {ok,{R_ip,R_port}}=ssl:peername(Socket),
  parse_dns_request(Socket, Pkt1, #proto{proto=tcp, tls=yes, rip=R_ip, rport=R_port}),
%  ok = ssl:close(Socket),
  case ssl:close(Socket) of % Improved closing
    ok -> ok;
    {error, Reason} -> ioc2rpz_fun:logMessage("~p:~p:~p. SSL close error: ~p ~n", [?MODULE, ?FUNCTION_NAME, ?LINE, Reason])
  end,
%  fprof:trace(stop),
  {stop, normal , State};

%% @doc Handles TCP connection closed events. Stops the worker normally.
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
%% @doc Handles TCP error events. Stops the worker normally.
handle_info({tcp_error, _Socket, _}, State) ->
  {stop, normal, State};

%% @doc Handles TLS connection closed events. Stops the worker normally.
handle_info({ssl_closed, _Socket}, State) ->
  {stop, normal, State};
%% @doc Handles TLS error events. Stops the worker normally.
handle_info({ssl_error, _Socket, _}, State) ->
  {stop, normal, State};

%% @doc Catch-all handler for unexpected messages. Logs the message and continues.
handle_info(Msg, State) ->
  ioc2rpz_fun:logMessage("Unexpected message: ~p, State: ~p~n", [Msg, State]),
  {noreply, State}.

%% @doc Handles synchronous calls. Currently a no-op; returns `{noreply, State}'.
handle_call(_E, _From, State) ->
%  io:format("ioc2rpz accept connection~n"),
  {noreply, State}.

%% @doc Terminates the gen_server worker. Performs no cleanup actions.
%% @param Reason The termination reason.
%% @param Tab The gen_server state.
%% @returns `ok'.
terminate(_Reason, _Tab) ->
%  ioc2rpz_db:tab2file([]),
  ok.

%% @doc Handles hot code upgrades. Returns the state unchanged.
code_change(_OldVersion, Tab, _Extra) ->
  {ok, Tab}.

%% @doc Dispatches a DNS packet to the appropriate transport-specific send function.
%% Routes to `send_dns_tcp/3', `send_dns_tls/3', or `send_dns_udp/5' based on
%% the protocol and TLS flag in the Proto record.
%% @param Socket The connection socket.
%% @param Pkt The DNS response packet binary.
%% @param ProtoArgs A list `[Proto, Args]' where Proto is a `#proto{}' record
%%        and Args is `addlen' or `[]'.
%% @returns `ok' on success, `{ok, Pkt}' for DoH, or `{error, Reason}' on failure.
%% Send a message back to the client
send_dns(Socket,Pkt,[Proto,Args]) when Proto#proto.proto == tcp, Proto#proto.tls == no ->
  case send_dns_tcp(Socket,Pkt, Args) of
   {error, Reason} -> ioc2rpz_fun:logMessage("~p:~p:~p. send_dns_tcp. remote IP ~p error: ~p ~n",[?MODULE, ?FUNCTION_NAME, ?LINE, ip_to_str(Proto#proto.rip), Reason]),
                      {error, Reason};
   ok -> ok
  end;

send_dns(Socket,Pkt,[Proto,Args]) when Proto#proto.proto == tcp, Proto#proto.tls == yes ->
  case send_dns_tls(Socket,Pkt, Args) of
   {error, Reason} -> ioc2rpz_fun:logMessage("~p:~p:~p. send_dns_tls. error: ~p ~n",[?MODULE, ?FUNCTION_NAME, ?LINE, Reason]),
                      {error, Reason};
   ok -> ok
  end;

send_dns(_Socket,Pkt,[Proto,_Args]) when Proto#proto.proto == doh ->
	%ioc2rpz_fun:logMessage("Send DoH\n",[]),
	{ok, Pkt};

send_dns(Socket,Pkt,[Proto,Args]) when Proto#proto.proto == udp ->
  send_dns_udp(Socket, Proto#proto.rip, Proto#proto.rport, Pkt, Args).

%% @doc Sends a DNS response over a TCP socket.
%% When Args is `addlen', prepends the 2-byte DNS TCP length prefix to the packet.
%% When Args is `[]', sends the packet as-is (intermediate packet in a multi-packet
%% zone transfer). After a successful send, re-arms the socket with `{active, once}'.
%% @param Socket The TCP connection socket.
%% @param Pkt The DNS response packet binary.
%% @param Args `addlen' for first/only packet, `[]' for intermediate packets.
%% @returns `ok' on success, `{error, Reason}' on send failure.
send_dns_tcp(Socket, Pkt, addlen) -> %used to send the first or an only packet
case gen_tcp:send(Socket, [<<(byte_size(Pkt)):16>>,Pkt]) of
  ok -> inet:setopts(Socket, [{active, once}]); %TODO validate the response, if dropped - pass back to remove cached zone
  {error, Reason} -> {error, Reason} %passing a reason if send was failed
end;

send_dns_tcp(Socket, Pkt, []) -> %used to pass intermediate packets
  case gen_tcp:send(Socket, Pkt) of
    ok -> inet:setopts(Socket, [{active, once}]); %TODO validate the response, if dropped - pass back to remove cached zone
    {error, Reason} -> {error, Reason} %passing a reason if send was failed
end.

%% @doc Sends a DNS response over a TLS socket.
%% When Args is `addlen', prepends the 2-byte DNS TCP length prefix.
%% When Args is `[]', sends the packet as-is for intermediate zone transfer packets.
%% After sending, re-arms the socket with `{active, once}'.
%% Note: Return values from `ssl:send/2' and `ssl:setopts/2' are currently not checked.
%% @param Socket The TLS connection socket.
%% @param Pkt The DNS response packet binary.
%% @param Args `addlen' for first/only packet, `[]' for intermediate packets.
%% @returns The result of `ssl:setopts/2' (typically `ok').
send_dns_tls(Socket, Pkt, addlen) -> %used to send the first or an only packet
  ssl:send(Socket, [<<(byte_size(Pkt)):16>>,Pkt]),
% The connection will not be reused and a child will be terminated
% TODO check compliance with DoT
  ssl:setopts(Socket, [{active, once}]);

send_dns_tls(Socket, Pkt, []) -> %used to pass intermediate packets
  ssl:send(Socket, Pkt),
% The connection will not be reused and a child will be terminated
% TODO check compliance with DoT
  ssl:setopts(Socket, [{active, once}]).

%% @doc Sends a DNS response over UDP.
%% Delegates directly to `gen_udp:send/4'. The return value is currently not checked.
%% @param Socket The UDP socket.
%% @param Dst The destination IP address tuple.
%% @param Port The destination port number.
%% @param Pkt The DNS response packet binary.
%% @param Args Unused arguments (reserved for future use).
%% @returns The result of `gen_udp:send/4'.
send_dns_udp(Socket, Dst, Port, Pkt, _Args) ->
  gen_udp:send(Socket, Dst, Port, Pkt).

%% @doc Parses and validates an incoming DNS request, applying rate limiting.
%% This is the main entry point for DNS query processing. It performs:
%% - Packet size validation (rejects packets of 12 bytes or fewer)
%% - DDoS detection (rejects queries from DNS ports 53/853)
%% - QR bit check (drops responses masquerading as queries)
%% - QDCOUNT validation (rejects multi-question queries)
%% - Per-IP/query rate limiting via `ioc2rpz_fun:check_rate_limit/1'
%% - Delegation to `process_dns_request/3' for valid queries
%%
%% @param Socket The connection socket (TCP, TLS, or UDP).
%% @param Data The raw DNS query binary (without TCP length prefix).
%% @param Proto A `#proto{}' record with protocol type, TLS flag, remote IP/port.
%% @returns Side-effectful; sends DNS responses via `send_dns/3'. No meaningful return.
parse_dns_request(_Socket, Data, Proto) when byte_size(Data) =< 12 ->
%%% Bad DNS packet
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(101),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto)]);

parse_dns_request(_Socket, _Data, Proto) when Proto#proto.rport == 53; Proto#proto.rport == 853 ->
%%% DDoS attempt
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(501),[ip_to_str(Proto#proto.rip),Proto#proto.rport,Proto#proto.proto]);

parse_dns_request(_Socket, <<_DNSId:2/binary, 1:1, _OptB:7, _:1, _OptE:3, _:4, _QDCOUNT:2/big-unsigned-unit:8,_ANCOUNT:2/big-unsigned-unit:8,_NSCOUNT:2/binary,_ARCOUNT:2/binary, Rest/binary>> = _Data, Proto) ->
%%% QR bit set. We've got response instead of query. Drop the message.
%%% replace by extract_label(,<<>>)
%%% 2020-08-22 to remove after QA
  %[QName,<<QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8, _Other_REC/binary>>] = binary:split(Rest,<<0>>),
  {<<QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8, _Other_REC/binary>>,QName} = extract_label(Rest,<<>>),
  QStr=dombin_to_str(QName),
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(109),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass)]);

parse_dns_request(Socket, <<DNSId:2/binary, _:1, OptB:7, _:1, OptE:3, _:4, QDCOUNT:2/big-unsigned-unit:8,ANCOUNT:2/big-unsigned-unit:8,NSCOUNT:2/binary,ARCOUNT:2/binary, Rest/binary>> = _Data, Proto) when QDCOUNT /= 1 -> %_:2/binary, ;ANCOUNT /= 0
%%% Bad DNS request. QDCount != 1
%%% replace by extract_label(,<<>>)
%%% 2020-08-22 to remove after QA
%  [QName,<<QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8, _Other_REC/binary>>] = binary:split(Rest,<<0>>),
  {<<QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8, _Other_REC/binary>>,QName} = extract_label(Rest,<<>>),
  QStr=dombin_to_str(QName),
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(102),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass)]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?SERVFAIL:4>>, <<QDCOUNT:2,ANCOUNT:2,NSCOUNT:2,ARCOUNT:2>>, Rest, [], Proto);

parse_dns_request(Socket, <<PH:4/bytes, _QDCOUNT:2/big-unsigned-unit:8,_ANCOUNT:2/big-unsigned-unit:8,NSCOUNT:2/big-unsigned-unit:8,ARCOUNT:2/big-unsigned-unit:8, Rest/binary>> = Data, Proto = #proto{rip = Rip}) ->
  <<DNSId:2/binary, _:1, OptB:7, _:1, OptE:3, _:4>> = PH,
  {<<QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8, Other_REC/binary>>,QName} = extract_label(Rest,<<>>),
  Question = <<QName/binary,0:8,QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8>>,
  QStr=dombin_to_str(QName),
  case ioc2rpz_fun:check_rate_limit({Rip,QName,QType}) of
      true -> % Rate limit exceeded - send refused
        ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(429),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr,ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass)]), % Log rate limiting event
        send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?REFUSED:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto);
% 2025-01-11 A client (dig) expects a signed response. It may be not needed at all - to check RFC
%        case ARCOUNT of 
%          0 ->
%            send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?REFUSED:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto);
%          _ ->
%            {_RRRes,_DNSRR,TSIG,_SOA,_RAWN} = parse_rr(NSCOUNT, ARCOUNT, Other_REC),
%            Opt = <<1:1,OptB:7, 0:1, OptE:3,?REFUSED:4>>,
%            RH = <<1:16,0:16,0:16,1:16>>, 
%            {ok,TSIGRR,_}=add_TSIG(list_to_binary([DNSId, Opt, RH, Question]),TSIG),
%            send_REQST(Socket, DNSId, Opt, RH, Question, TSIGRR, Proto)
%          end;
      false -> % Rate limit not exceeded, process the request
        %2025-01-10 TODO optimize passing processed data
        process_dns_request(Socket, Data, Proto)
  end.

%% @doc Processes a validated DNS request after rate limiting.
%% Extracts the query name, type, class, and resource records (TSIG, SOA, OPT).
%% Routes the request based on query type and zone:
%% - Management commands (ioc2rpz-status, ioc2rpz-reload-cfg, etc.) via CHAOS TXT
%% - Sample zone transfers (sample-zone.ioc2rpz)
%% - RPZ zone SOA queries and zone transfers (AXFR/IXFR)
%% - TSIG authentication is validated for all protected operations.
%%
%% @param Socket The connection socket.
%% @param Data The raw DNS query binary.
%% @param Proto A `#proto{}' record with connection metadata.
%% @returns Side-effectful; sends DNS responses. No meaningful return.
process_dns_request(Socket, <<PH:4/bytes, QDCOUNT:2/big-unsigned-unit:8,ANCOUNT:2/big-unsigned-unit:8,NSCOUNT:2/big-unsigned-unit:8,ARCOUNT:2/big-unsigned-unit:8, Rest/binary>> = _Data, Proto) when QDCOUNT == 1, ANCOUNT == 0 -> %_DataLen:2/big-unsigned-unit:8,
  STime=erlang:system_time(millisecond), %nanosecond, microsecond, millisecond, second
  <<DNSId:2/binary, _:1, OptB:7, _:1, OptE:3, _:4>> = PH,
  {<<QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8, Other_REC/binary>>,QName} = extract_label(Rest,<<>>),
  Question = <<QName/binary,0:8,QType:2/big-unsigned-unit:8,QClass:2/big-unsigned-unit:8>>,
  QStr=dombin_to_str(QName),

  {RRRes,_DNSRR,TSIG,SOA,RAWN} = parse_rr(NSCOUNT, ARCOUNT, Other_REC),
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(202),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name)]),

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
          ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(301),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),""]),
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
              {_,noauth} ->
                  case send_zone(Zone#rpz.cache,Socket,{Question,DNSId,OptB,OptE,<<QDCOUNT:2,ANCOUNT:2,NSCOUNT:2,ARCOUNT:2>>,Rest,Zone, QType,NSServ,MailAddr,[],SOA}, Proto) of
                   ok -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(201),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),"",(erlang:system_time(millisecond)-STime)]);
                   {error, closed} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(131),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),(erlang:system_time(millisecond)-STime)]);
                   {error, Reason} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(131),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),(erlang:system_time(millisecond)-STime),Reason])
                  end;
              {_,valid} ->
                  case send_zone(Zone#rpz.cache,Socket,{Question,DNSId,OptB,OptE,<<QDCOUNT:2,ANCOUNT:2,NSCOUNT:2,ARCOUNT:2>>,Rest,Zone,QType,NSServ,MailAddr,TSIG1,SOA}, Proto) of
                   ok -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(201),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),(erlang:system_time(millisecond)-STime)]);
                   {error, closed} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(131),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),(erlang:system_time(millisecond)-STime)]);
                   {error, Reason} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(131),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),(erlang:system_time(millisecond)-STime),Reason])
                  %%%%%%%%%% double check TSIG1 was replaced with TSIG
                  end;
              {_,TSIGV} -> send_TSIG_error(TSIGV, Socket, DNSId, OptB, OptE, Question, TSIG1, ["zone transfer failed",[Zone#rpz.zone_str,TSIGV],QStr, QType, QClass], Proto)
            end;
        _ ->
          ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(120),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),""]),
          send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto)
      end;
    {_,_,_,badTSIGposition} ->
    %rfc2845 If a TSIG record is present in any other position, the packet is dropped and a response with RCODE 1 (FORMERR) MUST be returned.
      ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(108),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),<<>>,""]),
      send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?FORMERR:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto);

    {_,_,?C_IN,ok} when (QType == ?T_AXFR orelse (QType == ?T_IXFR)) andalso Proto#proto.proto == udp ->
      ?logDebugMSG("Zone transfer (AXFR and IXFR) via UDP is not supported.~n",[]),
      ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(102),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass)]),
      send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTIMP:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto);

    {QName, QType, QClass,RRRes} ->
%    _  ->
      ?logDebugMSG("Unknow request ~p ~p ~p ~p ~n",[QName, QType, QClass,RRRes]),
      ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(102),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass)]),
      send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTIMP:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto)
  end.


%% @doc Sends a TSIG authentication error response to the client.
%% Handles multiple TSIG error types with appropriate DNS response codes and
%% CEF log messages:
%% - `notsig': Request was not signed; returns REFUSED.
%% - `keynotfound': TSIG key name not found; returns NOTAUTH with TSIG_BADKEY.
%% - `badmac': TSIG MAC validation failed; returns NOTAUTH with TSIG_BADSIG.
%% - `badtimegoodmac': Valid MAC but timestamp out of fudge window; returns
%%   NOTAUTH with TSIG_BADTIME and includes server time for clock sync.
%% - Catch-all: Returns SERVFAIL for any other TSIG error.
%%
%% @param ErrorType The TSIG error atom (`notsig', `keynotfound', `badmac',
%%        `badtimegoodmac', or any other term).
%% @param Socket The connection socket.
%% @param DNSId The 2-byte DNS transaction ID.
%% @param OptB The 7-bit opcode/flags byte from the DNS header.
%% @param OptE The 3-bit extended flags from the DNS header.
%% @param Question The DNS question section binary.
%% @param TSIG The `#dns_TSIG_RR{}' record from the request.
%% @param Info A list `[MSG, TSGV, QStr, QType, QClass]' with error context.
%% @param Proto The `#proto{}' record with connection metadata.
%% @returns The result of `send_REQST/7'.
send_TSIG_error(notsig, Socket, DNSId, OptB, OptE, Question, _TSIG, [MSG,_TSGV,QStr, QType, QClass], Proto) ->
 %%% request not signed
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(103),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),"",MSG]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?REFUSED:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto);

send_TSIG_error(keynotfound, Socket, DNSId, OptB, OptE, Question, TSIG, [MSG,_TSGV,QStr, QType, QClass], Proto) ->
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(104),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),MSG]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>, <<1:16,0:16,0:16,1:16>>,<<Question/binary,(TSIG#dns_TSIG_RR.name)/binary,?RT_TSIG:16/big-unsigned,?C_ANY:16/big-unsigned,0:32,(TSIG#dns_TSIG_RR.rdlength-TSIG#dns_TSIG_RR.mac_len):16/big-unsigned,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time)/binary,(TSIG#dns_TSIG_RR.fudge)/binary,0:16,DNSId/binary,?TSIG_BADKEY:16/big-unsigned,0:16>>, [], Proto);

send_TSIG_error(badmac, Socket, DNSId, OptB, OptE, Question, TSIG, [MSG,_TSGV,QStr, QType, QClass], Proto) ->
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(105),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),MSG]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>, <<1:16,0:16,0:16,1:16>>,<<Question/binary,(TSIG#dns_TSIG_RR.name)/binary,?RT_TSIG:16/big-unsigned,?C_ANY:16/big-unsigned,0:32,(TSIG#dns_TSIG_RR.rdlength-TSIG#dns_TSIG_RR.mac_len):16/big-unsigned,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time)/binary,(TSIG#dns_TSIG_RR.fudge)/binary,0:16,DNSId/binary,?TSIG_BADSIG:16/big-unsigned,0:16>>, [], Proto);

send_TSIG_error(badtimegoodmac, Socket, DNSId, OptB, OptE, Question, TSIG, [MSG,_TSGV,QStr, QType, QClass], Proto) ->
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(106),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),MSG]),
  CTime=erlang:system_time(seconds),
  Pkt = <<DNSId:2/binary,1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4,1:16,0:16,0:16,0:16, Question/binary>>,
  PKT = <<(TSIG#dns_TSIG_RR.mac_len):2/big-unsigned-unit:8,(TSIG#dns_TSIG_RR.mac)/binary,Pkt/binary,(TSIG#dns_TSIG_RR.name)/binary,0:8,255:8,0:32,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,?TSIG_BADTIME:16/big-unsigned,6:16,CTime:48>>,
  MAC = case TSIG#dns_TSIG_RR.alg_str of %TODO вынести в функцию
    "md5" -> crypto:mac(hmac,md5,TSIG#dns_TSIG_RR.key,PKT); %crypto:hmac(md5,TSIG#dns_TSIG_RR.key,PKT);
    "sha256" -> crypto:mac(hmac,sha256,TSIG#dns_TSIG_RR.key,PKT); %crypto:hmac(sha256,TSIG#dns_TSIG_RR.key,PKT);
    "sha512" -> crypto:mac(hmac,sha512,TSIG#dns_TSIG_RR.key,PKT) %crypto:hmac(sha512,TSIG#dns_TSIG_RR.key,PKT)
  end,
  MAC_LEN=byte_size(MAC),
  DATA = <<(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,MAC_LEN:2/big-unsigned-unit:8,MAC/binary,(TSIG#dns_TSIG_RR.oid):16,?TSIG_BADTIME:16/big-unsigned,6:16,CTime:48>>,
  DLEN=byte_size(DATA),
  TSIGR = <<(TSIG#dns_TSIG_RR.name)/binary,0:8,250:8,0:8,255:8,0:32,DLEN:2/big-unsigned-unit:8,DATA/binary>>,
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?NOTAUTH:4>>,<<1:16,0:16,0:16,1:16>>,<<Question/binary,TSIGR/binary>>, [], Proto);


send_TSIG_error(_, Socket, DNSId, OptB, OptE, Question, TSIG, [MSG,MSPG,QStr, QType, QClass], Proto) ->
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(107),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),QStr, ioc2rpz_fun:q_type(QType), ioc2rpz_fun:q_class(QClass),dombin_to_str(TSIG#dns_TSIG_RR.name),MSG, MSPG]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?SERVFAIL:4>>, <<1:16,0:16,0:16,0:16>>, Question, [], Proto).


%% @doc Validates a DNS request's TSIG authentication.
%% Checks whether the request is signed, whether the TSIG key exists in the
%% server's key table and is authorized for the zone, computes the HMAC over
%% the request packet, and validates the MAC and timestamp.
%%
%% @param PH The 4-byte DNS packet header.
%% @param QDCOUNT Question count.
%% @param ANCOUNT Answer count.
%% @param NSCOUNT Authority count.
%% @param ARCOUNT Additional count (minus 1 for the TSIG RR).
%% @param Question The DNS question section binary.
%% @param DNSRR The raw resource records binary (excluding TSIG).
%% @param TSIG The `#dns_TSIG_RR{}' record parsed from the request.
%% @param KEYS A list of authorized TSIG key names for this zone/operation.
%% @returns `{notsig, []}' if unsigned and no keys required,
%%          `{noauth, []}' if unsigned and keys are required,
%%          `{valid, TSIG}' if MAC and timestamp are valid,
%%          `{badmac, []}' if MAC is invalid,
%%          `{badtimegoodmac, TSIG}' if MAC is valid but timestamp is out of range,
%%          `{keynotfound, TSIG}' if the key name is not in the server's key table.
validate_REQ(_PH,_QDCOUNT,_ANCOUNT,_NSCOUNT,_ARCOUNT,_Question,_DNSRR,TSIG, KEYS)  when TSIG#dns_TSIG_RR.name == <<>>, KEYS /= [] ->
  {notsig,[]};

validate_REQ(_PH,_QDCOUNT,_ANCOUNT,_NSCOUNT,_ARCOUNT,_Question,_DNSRR,TSIG, KEYS)  when TSIG#dns_TSIG_RR.name == <<>>, KEYS == [] ->
  {noauth,[]};

validate_REQ(PH,QDCOUNT,ANCOUNT,NSCOUNT,ARCOUNT,Question,DNSRR,TSIG, KEYS) when TSIG#dns_TSIG_RR.name /= <<>> ->
  case {ets:select(cfg_table, [{{[key,TSIG#dns_TSIG_RR.name],'$1','$2','$3'},[],[['$1','$2','$3']]}]), lists:member(TSIG#dns_TSIG_RR.name,KEYS)} of
    {[[_KeyName,Alg, KEY]],true} when Alg == "md5",TSIG#dns_TSIG_RR.alg == <<8:8,"hmac-md5",7:8,"sig-alg",3:8,"reg",3:8,"int",0:8>>; Alg == "sha256",TSIG#dns_TSIG_RR.alg == <<11:8,"hmac-sha256",0:8>>; Alg == "sha512",TSIG#dns_TSIG_RR.alg == <<11:8,"hmac-sha512",0:8>>  ->
        ?logDebugMSG("Found Key ... ",[]),
        LTime=erlang:system_time(seconds), RTimeL = binary:decode_unsigned(TSIG#dns_TSIG_RR.time) - binary:decode_unsigned(TSIG#dns_TSIG_RR.fudge), RTimeH = binary:decode_unsigned(TSIG#dns_TSIG_RR.time) + binary:decode_unsigned(TSIG#dns_TSIG_RR.fudge),
        PKT = <<PH/binary,QDCOUNT:2/big-unsigned-unit:8,ANCOUNT:2/big-unsigned-unit:8,NSCOUNT:2/big-unsigned-unit:8,ARCOUNT:2/big-unsigned-unit:8,Question/binary,DNSRR/binary,(TSIG#dns_TSIG_RR.name)/binary,0,255,0,0,0,0,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,(TSIG#dns_TSIG_RR.error)/binary,(TSIG#dns_TSIG_RR.olen):2/big-unsigned-unit:8,(TSIG#dns_TSIG_RR.odata)/binary>>,
        CH_MAC = case Alg of %TODO вынести в функцию
          "md5" -> crypto:mac(hmac,md5,KEY,PKT); %crypto:hmac(md5,KEY,PKT);
          "sha256" -> crypto:mac(hmac,sha256,KEY,PKT); %crypto:hmac(sha256,KEY,PKT);
          "sha512" -> crypto:mac(hmac,sha512,KEY,PKT) %crypto:hmac(sha512,KEY,PKT)
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

%% @doc Signs a DNS response packet with TSIG.
%% Computes the HMAC over the packet concatenated with TSIG metadata fields,
%% using the algorithm and key from the TSIG record. Supports md5, sha256,
%% and sha512 algorithms. Returns the TSIG resource record binary and an
%% updated TSIG record with the new MAC for subsequent packet signing
%% (time_only mode for multi-packet zone transfers).
%%
%% @param Pkt The DNS response packet binary to sign.
%% @param TSIG The `#dns_TSIG_RR{}' record with key, algorithm, and timing info.
%% @returns `{ok, TSIGBinary, UpdatedTSIG}' where TSIGBinary is the TSIG RR
%%          to append to the DNS packet.
%Sign the packet
add_TSIG(Pkt, TSIG) ->
  if TSIG#dns_TSIG_RR.time_only == true ->
      PKT = <<(TSIG#dns_TSIG_RR.mac_len):2/big-unsigned-unit:8,(TSIG#dns_TSIG_RR.mac)/binary,Pkt/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary>>;
    true ->
      PKT = <<(TSIG#dns_TSIG_RR.mac_len):2/big-unsigned-unit:8,(TSIG#dns_TSIG_RR.mac)/binary,Pkt/binary,(TSIG#dns_TSIG_RR.name)/binary,0:8,255:8,0:32,(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,0:16,0:16>>
  end,
  MAC = case TSIG#dns_TSIG_RR.alg_str of %TODO вынести в функцию
    "md5" -> crypto:mac(hmac,md5,TSIG#dns_TSIG_RR.key,PKT); %crypto:hmac(md5,TSIG#dns_TSIG_RR.key,PKT);
    "sha256" -> crypto:mac(hmac,sha256,TSIG#dns_TSIG_RR.key,PKT); %crypto:hmac(sha256,TSIG#dns_TSIG_RR.key,PKT);
    "sha512" -> crypto:mac(hmac,sha512,TSIG#dns_TSIG_RR.key,PKT) %crypto:hmac(sha512,TSIG#dns_TSIG_RR.key,PKT)
  end,
  LTime = <<(erlang:system_time(seconds)):6/big-unsigned-unit:8>>, % why should we resend client's time?
  %LTime = <<(TSIG#dns_TSIG_RR.time):6/binary>>,
  MAC_LEN=byte_size(MAC),
  DATA = <<(TSIG#dns_TSIG_RR.alg)/binary,(TSIG#dns_TSIG_RR.time):6/binary,(TSIG#dns_TSIG_RR.fudge)/binary,MAC_LEN:2/big-unsigned-unit:8,MAC/binary,(TSIG#dns_TSIG_RR.oid):16,0:16,0:16>>,
  DLEN=byte_size(DATA),
  {ok,<<(TSIG#dns_TSIG_RR.name)/binary,0:8,250:8,0:8,255:8,0:32,DLEN:2/big-unsigned-unit:8,DATA/binary>>,TSIG#dns_TSIG_RR{mac_len=MAC_LEN,mac=MAC,time_only=true,time=LTime}}. %END Sign the packet

%% @doc Parses DNS resource records (authority and additional sections).
%% Extracts TSIG, SOA, OPT, and other RR types from the raw binary.
%% Returns the parsed records, the TSIG record (if present), the SOA record
%% (if present), and the raw RR data (excluding TSIG) for MAC computation.
%%
%% @param NSCOUNT The number of authority section records.
%% @param ARCOUNT The number of additional section records.
%% @param RAW The raw binary of the resource records.
%% @returns `{ok | badTSIGposition, RRList, #dns_TSIG_RR{}, #dns_SOA_RR{}, RawBinary}'.
%%%
%%% Parse resourse records
%%%
parse_rr(0, 0, <<>>) ->
  {ok,[],#dns_TSIG_RR{name = <<>>},#dns_SOA_RR{name = <<>>,serial=0},<<>>};

parse_rr(NSCOUNT, ARCOUNT, RAW) ->
  parse_rr(NSCOUNT, ARCOUNT, RAW, [],#dns_SOA_RR{name = <<>>,serial=0}, <<>>).

parse_rr(0, 0, <<>>, RR, RAWN, SOA) ->
  {ok,RR,#dns_TSIG_RR{name = <<>>},SOA,RAWN};

parse_rr(NSCOUNT, ARCOUNT, <<_Zip:8,_/binary>> = RAW, RR, SOA, RAWN) ->
  %%% 2020-08-21 To remove after QA
  % if Zip >= 192 ->
  %     <<RNAME:2/binary, RType:2/big-unsigned-unit:8,RClass:2/big-unsigned-unit:8,RTTL:4/big-unsigned-unit:8,DLen:2/big-unsigned-unit:8,REST/binary>> = RAW;
  %   true ->
  %     % Labels can be "zipped"
  %     %TODO переделать, так как может зиповаться часть записи/FQDN и в этом случае нужен весь пакет, чтобы получить запись
  %     [RNAME0,<<RType:2/big-unsigned-unit:8,RClass:2/big-unsigned-unit:8,RTTL:4/big-unsigned-unit:8,DLen:2/big-unsigned-unit:8,REST/binary>>] = binary:split(RAW,<<0>>),
  %     RNAME = <<RNAME0/binary,0:8>>
  % end,
   {<<RType:2/big-unsigned-unit:8,RClass:2/big-unsigned-unit:8,RTTL:4/big-unsigned-unit:8,DLen:2/big-unsigned-unit:8,REST/binary>>,RNAME} = extract_label(RAW,<<0>>),

  case {RType, RClass} of
    {?RT_TSIG, ?C_ANY} -> %TSIG Record
      if DLen < byte_size(REST) -> {badTSIGposition,[],[]};
        true ->
          %%% replace by extract_label(,<<>>)
          %%% 2020-08-22 to remove after QA
          %[TSIG_ALG,<<TSIG_TIME:6/bytes,TSIG_FUDGE:2/bytes,MACLEN:2/big-unsigned-unit:8,REST1/binary>>] = binary:split(REST,<<0>>),
          {<<TSIG_TIME:6/bytes,TSIG_FUDGE:2/bytes,MACLEN:2/big-unsigned-unit:8,REST1/binary>>,TSIG_ALG} = extract_label(REST,<<>>),
          <<TSIG_MAC:MACLEN/bytes,TSIG_OID:2/big-unsigned-unit:8,TSIG_ERR:2/bytes,TSIG_OTHER_LEN:2/big-unsigned-unit:8,REST2/binary>> = REST1,
          <<TSIG_OTHER:TSIG_OTHER_LEN/bytes>> = REST2,
          {ok,RR,#dns_TSIG_RR{name=RNAME,type=RType,class=RClass,rdlength=DLen, alg = <<TSIG_ALG/binary,0>>, time=TSIG_TIME, fudge=TSIG_FUDGE, mac_len=MACLEN, mac=TSIG_MAC, oid=TSIG_OID,error=TSIG_ERR,olen=TSIG_OTHER_LEN,odata=TSIG_OTHER,time_only=false},SOA,RAWN}
      end;

    {?T_SOA, ?C_IN} -> %SOA Record
%-record(dns_SOA_RR, {name, type, class, ttl, rdlength, mname, rname, serial, refresh, retry, expire, minimum}).
      <<RDATA:DLen/bytes,RAW2/binary>> = REST,
      {REST2,MName} = extract_label(REST,<<>>),
      {<<Serial:32/big-unsigned, Refresh:32/big-unsigned, Retry:32/big-unsigned, Expire:32/big-unsigned, Minimum:32/big-unsigned,_/binary>>,RName} = extract_label(REST2,<<>>),
      if NSCOUNT > 0 -> NSCOUNT1=NSCOUNT-1, ARCOUNT1=ARCOUNT; true -> ARCOUNT1=ARCOUNT-1,NSCOUNT1=NSCOUNT end,
      %?logDebugMSG("~p ~p SOA Serial ~p ~n",[MName, RName, Serial]),
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



%% @doc Sends a SERVFAIL, REFUSED, NXDOMAIN, or other error/status DNS response.
%% Optionally signs the response with TSIG if a TSIG record is provided.
%% @param Socket The connection socket.
%% @param DNSId The 2-byte DNS transaction ID.
%% @param Opt The DNS header flags binary (QR=1, opcode, rcode).
%% @param RH The record counts binary (QD, AN, NS, AR).
%% @param Question The DNS question section binary.
%% @param TSIG The TSIG record for signing, or `[]' for unsigned responses.
%% @param Proto The `#proto{}' record.
%% @returns The result of `send_dns/3'.
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

%% @doc Sends a SOA (Start of Authority) response for an RPZ zone.
%% Constructs the SOA record from the zone's serial, SOA timers, NS server,
%% and mail address. Optionally signs with TSIG.
%% @param Socket The connection socket.
%% @param Zone The `#rpz{}' record for the queried zone.
%% @param DNSId The 2-byte DNS transaction ID.
%% @param OptB The 7-bit opcode/flags byte.
%% @param OptE The 3-bit extended flags.
%% @param Question The DNS question section binary.
%% @param MailAddr The SOA responsible person (rname) in wire format.
%% @param NSServ The SOA primary nameserver (mname) in wire format.
%% @param TSIG The TSIG record for signing, or `[]'.
%% @param Proto The `#proto{}' record.
%% @returns The result of `send_dns/3'.
%Send SOA
send_SOA(Socket, Zone, DNSId, OptB, OptE, Question, MailAddr, NSServ, TSIG, Proto) ->

  SOA = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,(Zone#rpz.soa_timers)/binary>>,
%%%  SOA = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,7200:32,3600:32,259001:32,7200:32>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOA)):16, SOA/binary>>,
  <<Opcode:4,_:1,TCRD:2>> = <<OptB:7>>,
  if TSIG /= [] ->
    {ok,TSIGRR,_}=add_TSIG(list_to_binary([DNSId, <<1:1, Opcode:4,1:1,TCRD:2, 0:1, OptE:3, ?NOERROR:4, 1:16,1:16,0:16,0:16>>, Question, SOAREC]),TSIG),
    Pkt1 = list_to_binary([DNSId, <<1:1, Opcode:4,1:1,TCRD:2, 0:1, OptE:3, ?NOERROR:4, 1:16,1:16,0:16,1:16>>, Question, SOAREC, TSIGRR]);
    true -> Pkt1 = <<DNSId/binary, 1:1, Opcode:4,1:1,TCRD:2, 1:1, OptE:3, ?NOERROR:4, 1:16,1:16,0:16,0:16, Question/binary, SOAREC/binary>>
  end,
  send_dns(Socket,Pkt1, [Proto,addlen]).
%END Send SOA

%% @doc Sends the built-in sample RPZ zone as an AXFR response.
%% Constructs a complete zone transfer containing example RPZ rules for all
%% supported action types: nxdomain, nodata, passthru, drop, tcp-only,
%% redirect_domain, redirect_ip (IPv4 and IPv6), IP-based triggers,
%% nsdname triggers, and nsip triggers. The zone is bookended by SOA records
%% per the AXFR protocol. Optionally signed with TSIG.
%%
%% @param Socket The connection socket.
%% @param DNSId The 2-byte DNS transaction ID.
%% @param OptB The 7-bit opcode/flags byte.
%% @param OptE The 3-bit extended flags.
%% @param Questions The DNS question section binary.
%% @param MailAddr The SOA rname in wire format.
%% @param NSServ The SOA mname in wire format.
%% @param TSIG The TSIG record for signing, or `[]'.
%% @param Proto The `#proto{}' record.
%% @returns The result of `send_dns/3'.
%% @see gen_rpzrule/5
%% @end
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

%% @doc Sends the server status as a DNS TXT response.
%% Reports ETS table sizes and memory usage for cfg_table, rpz_hotcache_table,
%% rpz_axfr_table, and rpz_ixfr_table.
%% @param Socket The connection socket.
%% @param Args A list `[Question, DNSId, OptB, OptE, TSIG]'.
%% @param Proto The `#proto{}' record.
%% @returns The result of `send_txt_response/4'.
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



%% @doc Sends a DNS TXT response with the given data string.
%% Splits the data into 254-byte chunks (per DNS TXT record rules) and
%% constructs a single TXT resource record. Optionally signs with TSIG.
%% @param Socket The connection socket.
%% @param Args A list `[Questions, DNSId, OptB, OptE, TSIG]'.
%% @param Data The TXT record data as a binary string.
%% @param Proto The `#proto{}' record.
%% @returns The result of `send_dns/3'.
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

%% @doc Generates a single DNS TXT resource record binary from a text binary.
%% Constructs the record with a compressed name pointer (0xC00C), TXT type,
%% IN class, default TTL, and the text data in length-prefixed format
%% (one-octet length prefix followed by the text bytes).
%% @param TXT Binary containing the text data (max 254 bytes).
%% @returns A complete DNS TXT resource record as an iodata binary.
gen_txt_rec(TXT) ->
  Len=byte_size(TXT),
  <<16#c00c:16, ?T_TXT:2/big-unsigned-unit:8, ?C_IN:2/big-unsigned-unit:8, ?TTL:32, (Len+1):16, Len:8, TXT/binary>>.


%% @doc Sends DNS NOTIFY messages to all configured secondary servers for a zone.
%% Spawns a separate process for each notification target. Supports both UDP
%% and TCP notification transports as configured in `Zone#rpz.notifylist'.
%% @param Zone The `#rpz{}' record containing the notify list.
%% @returns A list of spawned process PIDs.
send_notify(Zone) ->
  %TODO wait for the confirmation
  Pkt = <<0:1, ?OP_NOTIFY, 1:1, 0:6, ?NOERROR:4, 1:16,0:16,0:16,0:16,(Zone#rpz.zone)/binary,?T_SOA:16,?C_IN:16>>,
  [ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(221),[inet:ntoa(IP),53,Proto,Zone#rpz.zone_str]) || {Proto,IP} <- Zone#rpz.notifylist ],
  [ spawn(ioc2rpz,send_notify,[IP,Pkt,Proto,0,Zone#rpz.zone_str]) || {Proto,IP} <- Zone#rpz.notifylist ].

%% @doc Sends a DNS NOTIFY to a single destination via UDP.
%% Opens a random high port, sends the notify packet, and closes the socket.
%% Retries up to 3 times on `eaddrinuse' errors.
%% @param Dst The destination IP address tuple.
%% @param Pkt The NOTIFY packet binary (without DNS ID).
%% @param Proto `udp' atom.
%% @param NRuns The current retry count.
%% @param Zone The zone name string (for logging).
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

%% @doc Sends a DNS NOTIFY to a single destination via TCP.
%% Connects to port 53, sends the notify packet with TCP length prefix, and
%% closes the connection. Retries up to 3 times on `eaddrinuse' errors.
send_notify(Dst,Pkt,tcp,NRuns,Zone) ->
  case gen_tcp:connect(Dst, 53, [{active, false}], ?TCPTimeout) of
    {ok, Socket} -> DNSId = crypto:strong_rand_bytes(2), send_dns_tcp(Socket, <<DNSId/binary,Pkt/binary>>, addlen), gen_tcp:close(Socket);
    {error, eaddrinuse} when NRuns < 3 -> send_notify(Dst,Pkt,tcp,NRuns+1,Zone);
  	{error, Reason} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(222),[Dst,53,"tcp",Zone,Reason]), {Reason,[]}
  end.

%% @doc Sends a cached zone transfer by iterating over pre-built packets.
%% Prepends SOA and NS records to the first packet, appends SOA to the last,
%% and signs each packet with TSIG if provided. Used for AXFR/IXFR responses
%% when the zone is already cached in ETS.
%% @param Socket The connection socket.
%% @param NSREC The NS resource record binary.
%% @param SOAREC The SOA resource record binary.
%% @param TSIG The TSIG record for signing, or `[]'.
%% @param PktH The DNS response header binary.
%% @param Questions The DNS question section binary.
%% @param Pkts A list of `{PktN, ANCOUNT, NSCOUNT, ARCOUNT, Pkt}' tuples.
%% @param Proto The `#proto{}' record.
%% @returns `ok' on success, `{error, Reason}' on send failure.
send_cached_zone(Socket,NSREC, SOAREC, TSIG, PktH, Questions, Pkts, Proto) -> %created becasue of concurent zone creation
  send_cached_zone(Socket,NSREC, SOAREC, TSIG, PktH, Questions, Pkts,0, Proto).

send_cached_zone(_Socket,_NSREC, _SOAREC, _TSIG, _PktH, _Questions, [], _PktNum, _Proto) ->
  ok;

send_cached_zone(Socket, NSREC, SOAREC, TSIG, PktH, Questions, [{_PktN,ANCOUNT,NSCOUNT,ARCOUNT,Pkt}|REST], PktNum, Proto) ->
  if PktNum==0 -> PktF=[SOAREC,NSREC,Pkt], Cnt=2; true -> PktF=Pkt, Cnt=0 end,
  if REST==[] -> PktL=[PktF,SOAREC],Cnt1=Cnt+1; true -> PktL=PktF, Cnt1=Cnt end,
  if TSIG /= [] ->
    {ok,TSIGRR,TSIG1}=add_TSIG(list_to_binary([PktH, <<(ANCOUNT+Cnt1):16,NSCOUNT:16,ARCOUNT:16>>, Questions, PktL]),TSIG),
    Pkt1 = list_to_binary([PktH, <<(ANCOUNT+Cnt1):16,NSCOUNT:16,(ARCOUNT+1):16>>, Questions, PktL, TSIGRR]);
    true -> Pkt1 = list_to_binary([PktH,<<(ANCOUNT+Cnt1):16,NSCOUNT:16,ARCOUNT:16>>, Questions, PktL]), TSIG1=TSIG
  end,
  case send_dns(Socket,Pkt1, [Proto,addlen]) of %TODO analyze response, drop cached records and terminate if there were transmission errors
    ok -> send_cached_zone(Socket, NSREC, SOAREC, TSIG1, PktH, Questions, REST,PktNum+1, Proto);
  	{error, Reason} -> {error, Reason}
  end.

%% @doc Routes a zone transfer request to the appropriate handler.
%% Dispatches based on cache status, zone readiness, query type (AXFR/IXFR),
%% and zone serial numbers. Handles:
%% - Cached AXFR: sends pre-built packets from ETS
%% - Cached IXFR with same/newer serial: sends SOA-only response
%% - Cached IXFR with full transfer needed: sends full zone
%% - Cached IXFR with incremental data: sends add/delete diff
%% - Not-ready zones: returns SERVFAIL
%% - Non-cacheable zones: generates and sends live, with hot cache support
%%
%% @param Cache `<<"true">>' or other value indicating cache mode.
%% @param Socket The connection socket.
%% @param ZoneParams A tuple with all zone transfer parameters.
%% @param Proto The `#proto{}' record.
%% @returns `ok' on success, or the result of `send_dns/3'.
%Return cached zone
send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,_RH,_Rest,Zone,?T_AXFR,NSServ,MailAddr,TSIG,_SOA}, Proto) when Zone#rpz.status == ready;Zone#rpz.status == updating,Zone#rpz.serial /= 0 -> %AXFR
%  ioc2rpz_fun:logMessage("Zone ~p is cached ~p ~p ~n", [Zone#rpz.zone_str, Zone#rpz.serial,Zone#rpz.status ]),

  SOA = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOA)):16, SOA/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  Pkt=ioc2rpz_db:read_db_pkt(Zone),
  send_cached_zone(Socket, NSRec, SOAREC, TSIG, <<DNSId:2/binary ,1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>, Questions, Pkt, Proto);

send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,_RH,_Rest,Zone,?T_IXFR,NSServ,MailAddr,TSIG,SOA}, Proto) when Zone#rpz.serial=<SOA#dns_SOA_RR.serial ->
%If an IXFR query with the same or newer version number than that of the server is received, it is replied to with a single SOA record of the server's current version, just as in AXFR.
  SOAR = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOAR)):16, SOAR/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  PktH = [DNSId, <<1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>],
  if TSIG /= [] ->
    {ok,TSIGRR,_}=add_TSIG(list_to_binary([PktH, <<1:16,0:16,0:16>>, Questions, SOAREC]),TSIG),
    Pkt1 = list_to_binary([PktH, <<1:16,0:16,1:16>>, Questions, SOAREC, TSIGRR]);
    true -> Pkt1 = list_to_binary([PktH,<<1:16,0:16,0:16>>, Questions, SOAREC])
  end,
  send_dns(Socket,Pkt1, [Proto,addlen]); %TODO analyze response, drop cached records and terminate if there were transmission errors


send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,_RH,_Rest,Zone,?T_IXFR,NSServ,MailAddr,TSIG,SOA}, Proto) when Zone#rpz.serial==Zone#rpz.serial_ixfr,Zone#rpz.status == ready;Zone#rpz.serial==Zone#rpz.serial_ixfr,Zone#rpz.status == updating;SOA#dns_SOA_RR.serial<Zone#rpz.serial_ixfr,Zone#rpz.status == ready;SOA#dns_SOA_RR.serial<Zone#rpz.serial_ixfr,Zone#rpz.status == updating ->
%Serial_IXFR = Serial => do full zone transfer. SOA#dns_SOA_RR.serial less than Serial and we do not have the changes log

  ioc2rpz_fun:logMessage("IXFR zone ~p serial ~p request serial ~p ~n",[Zone#rpz.zone_str, Zone#rpz.serial_ixfr, SOA#dns_SOA_RR.serial]),
  SOAR = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOAR)):16, SOAR/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  Pkt=ioc2rpz_db:read_db_pkt(Zone),
  send_cached_zone(Socket, NSRec, SOAREC, TSIG, <<DNSId:2/binary ,1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>, Questions, Pkt, Proto);

send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,_RH,_Rest,Zone,?T_IXFR,NSServ,MailAddr,TSIG,SOA}, Proto) when Zone#rpz.status == ready;Zone#rpz.status == updating -> %IXFR
  SOAR = <<NSServ/binary,MailAddr/binary,(Zone#rpz.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOARCL = <<NSServ/binary,MailAddr/binary,(SOA#dns_SOA_RR.serial):32,(Zone#rpz.soa_timers)/binary>>,
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOAR)):16, SOAR/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  SOARECCL = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOARCL)):16, SOARCL/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  IOCexp=[ {X,Exp,Z} || [X,_,Exp,Z] <- ioc2rpz_db:read_db_record(Zone,SOA#dns_SOA_RR.serial,expired) ],
  IOCnew=[ {X,Exp,Z} || [X,_,Exp,Z] <- ioc2rpz_db:read_db_record(Zone,SOA#dns_SOA_RR.serial,new)],
%  ioc2rpz_fun:logMessage("Serial ~p /= Serial IXFR ~p, IXFR=~p Zone ~p Expired IOC ~p, New IOC ~p ~n",[Zone#rpz.serial,Zone#rpz.serial_ixfr,SOA#dns_SOA_RR.serial,Zone#rpz.zone_str,IOCexp,IOCnew]),

% {ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})$"), %
	{ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}(\\/[0-9]{1,3})?)$|(:)"),

  PktHLen = 12+byte_size(Questions),
%  T_ZIP_L=ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]), % нужны ли {read_concurrency, true}, {write_concurrency, true} ???
	T_ZIP_L=init_T_ZIP_L(Zone),
  %В момент переключения на добавления - SOARECCL обнуляем, таким образом отслеживаем, что мы добавили новую SOA
  {ok, NRules, NIOCs}=send_packets(Socket,IOCexp ++ IOCnew, [], 0, 0, true, [DNSId, <<1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>], Questions, SOAREC,SOARECCL,Zone,MP,PktHLen,T_ZIP_L,TSIG,0,ixfr,0,false,Proto),
  ioc2rpz_fun:logMessage("Zone ~p, ~p rules, ~p IOCs ~n", [Zone#rpz.zone_str, NRules, NIOCs]),
  ets:delete(T_ZIP_L),
  ok;

%Zone was not cached, but should be
send_zone(<<"true">>,Socket,{Questions,DNSId,OptB,OptE,_RH,_Rest,Zone,QType,_NSServ,_MailAddr,TSIG,_SOA}, Proto) when Zone#rpz.status == notready;Zone#rpz.status == updating ->
 % ioc2rpz_fun:logMessage("Zone ~p is not ready ~n", [Zone#rpz.zone_str]),
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(121),[ip_to_str(Proto#proto.rip),Proto#proto.rport,?iif(Proto#proto.tls == yes,tls,Proto#proto.proto),Zone#rpz.zone_str, ioc2rpz_fun:q_type(QType), "IN",dombin_to_str(TSIG#dns_TSIG_RR.name),""]),
  send_REQST(Socket, DNSId, <<1:1,OptB:7, 0:1, OptE:3,?SERVFAIL:4>>, <<1:16,0:16,0:16,0:16>>, Questions, TSIG, Proto);

%Non cachable zones
send_zone(_,Socket,{Questions,DNSId,OptB,OptE,_RH,_Rest,Zone,_QType,NSServ,MailAddr,TSIG,_SOA}, Proto) ->
  SOAR = <<NSServ/binary,MailAddr/binary,(ioc2rpz_fun:curr_serial()):32,(Zone#rpz.soa_timers)/binary>>, %TODO get new serial and old serial use old serial if MD5 is the same
  SOAREC = <<?ZNameZip, ?T_SOA:16, ?C_IN:16, 604800:32, (byte_size(SOAR)):16, SOAR/binary>>, % 16#c00c:16 - Zone name/request is always at this location (10 bytes from DNSID)
  NSRec = <<?ZNameZip, ?T_NS:16, ?C_IN:16, 604800:32, (byte_size(NSServ)):16, NSServ/binary>>,
  CTime=ioc2rpz_fun:curr_serial_60(),
  case ets:match(rpz_hotcache_table,{{pkthotcache,Zone#rpz.zone,'_'},'$2','$3'}) of
    [[Timestamp,Pkt1]|REST] when CTime=<(Timestamp+?HotCacheTime) ->
      ioc2rpz_fun:logMessage("Found the zone in the hot cache~n",[]), %TODO remove debug
      Pkt = [binary_to_term(Pkt1) | [binary_to_term(X) || [_,X] <- REST]],
      send_cached_zone(Socket, NSRec, SOAREC, TSIG, <<DNSId:2/binary ,1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>, Questions, Pkt, Proto);
    _Else ->
      send_zone_live(Socket,sendNhotcache,Zone#rpz{serial=CTime},[DNSId, <<1:1, OptB:7, 1:1, OptE:3, ?NOERROR:4, 1:16>>],Questions, SOAREC,NSRec,TSIG,Proto)
  end,
  ok.

%% @doc Generates and sends a zone transfer live (not from cache).
%% Fetches IOCs from all configured sources, computes an MD5 hash for change
%% detection, writes records to the database, and streams the zone transfer
%% packets to the client. Also manages hot cache storage for non-cacheable zones.
%%
%% @param Socket The connection socket.
%% @param Op The operation type (`cache', `send', `sendNcache', `sendNhotcache').
%% @param Zone The `#rpz{}' record for the zone.
%% @param PktH The DNS response header binary.
%% @param Questions The DNS question section binary.
%% @param SOAREC The SOA resource record binary.
%% @param NSRec The NS resource record binary.
%% @param TSIG The TSIG record for signing, or `[]'.
%% @param Proto The `#proto{}' record.
%% @returns `{ok, MD5, NRules, NIOCs}' on success, or
%%          `{updateSOA, MD5, RuleCount, MaxIOC}' if zone data unchanged.
send_zone_live(Socket,Op,Zone,PktH,Questions, SOAREC,NSRec,TSIG,Proto) ->
  IOC = mrpz_from_ioc(Zone,axfr),
  MD5=crypto:hash(md5,term_to_binary(IOC)),
  case {Op, Zone#rpz.ioc_md5} of
    {cache, MD5} -> {updateSOA, MD5, Zone#rpz.rule_count, Zone#rpz.max_ioc}; %TODO looks like something was not finished
    _Else ->
%      {ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})$"), %
			{ok,MP} = re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}(\\/[0-9]{1,3})?)$|(:)"),
      PktHLen = 12+byte_size(Questions),
      ioc2rpz_db:write_db_record(Zone,IOC,axfr),
      ioc2rpz_db:delete_old_db_record(Zone),
			T_ZIP_L=init_T_ZIP_L(Zone),
      {ok, NRules, NIOCs}=send_packets(Socket,IOC, [], 0, 0, true, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,0,Op,0,true,Proto),
		  ioc2rpz_fun:logMessage("Live zone ~p, ~p rules, ~p IOCs ~n", [Zone#rpz.zone_str, NRules, NIOCs]),
      ets:delete(T_ZIP_L),
      {ok,MD5, NRules, NIOCs}
  end.

%% @doc Waits for a spawned zone packet generation process to complete.
%% Used in concurrent zone caching where IOCs are split across processes.
%% @param PID The PID of the spawned process to wait for.
%% @param Zone The zone name (unused, for debugging).
%% @returns `{ok, NRules, NIOCs}' from the spawned process.
w_send_packets(PID, _Zone) ->
  receive
    { ok, PID, {ok, NRules, NIOCs} } ->
      %ioc2rpz_fun:logMessage("Zone ~p. Got message from ~p # of rules ~p # of IOCs ~p~n",[Zone, PID, NRules, NIOCs]),
			{ok, NRules, NIOCs}
  end.


%% @doc Builds and sends DNS zone transfer packets from a list of IOC records.
%% This is the core packet assembly engine for AXFR/IXFR zone transfers.
%% It accumulates RPZ rules into packets up to `?DNSPktMax' size, then sends
%% or caches each packet. Handles:
%% - Empty zones (sends SOA+NS+SOA)
%% - First packet initialization with SOA and NS records
%% - Packet overflow and splitting at `?DNSPktMax' boundary
%% - IXFR delete/add SOA transitions
%% - Concurrent caching via process spawning
%% - TSIG signing of each packet
%% - Hot cache and ETS packet storage
%%
%% Has 20 parameters due to the accumulated state carried through recursion.
%%
%% @param Socket The connection socket (or `<<>>' for cache-only mode).
%% @param IOC The list of `{IOCBinary, Expiry, Type}' tuples to process.
%% @param Pkt The accumulated packet binary for the current DNS message.
%% @param ACount The answer record count for the current packet.
%% @param PSize The current packet payload size.
%% @param Zip Whether label compression is enabled.
%% @param PktH The DNS response header binary.
%% @param Questions The DNS question section binary.
%% @param SOAREC The current SOA record binary.
%% @param NSRec The NS record binary (or client SOA for IXFR).
%% @param Zone The `#rpz{}' record (rule_count and ioc_count are accumulated).
%% @param MP A compiled regex for IP address detection.
%% @param PktHLen The header + question length for offset calculations.
%% @param T_ZIP_L The label compression ETS table ID.
%% @param TSIG The TSIG record for signing, or `[]'.
%% @param PktN The current packet sequence number.
%% @param DBOp The database operation (`send', `cache', `sendNcache',
%%        `sendNhotcache', `ixfr').
%% @param SOANSSize The size of SOA+NS records (for first packet offset).
%% @param IXFRNewR Whether the IXFR "new records" SOA has been emitted.
%% @param Proto The `#proto{}' record.
%% @returns `{ok, NRules, NIOCs}' with the total rule and IOC counts.

% Empty zone
send_packets(Socket,[], [], 0, _ACount, _Zip, PktH, Questions, SOAREC,NSRec,Zone,_MP,_PktHLen,_T_ZIP_L,TSIG,PktN,DBOp,_SOANSSize,_IXFRNewR,Proto) -> %NSRec = Client SOA for IXFR
%%%TODO save # of rules set to 0 for AXRF
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
    ioc2rpz_fun:logMessage("Empty zone. CNT ~p ~n", [Cnt]),
    send_dns(Socket,Pkt1, [Proto,addlen]);
    true -> ok
  end,
  %if IXFR -> пустой зоны должно не быть, но на всякий случай можно предусмотреть передачу только SOA
  if (DBOp == cache) or (DBOp == sendNcache) ->
    ioc2rpz_db:write_db_pkt(Zone, {0,0,0,0, []}); % was 0,3,0,0 <- 3 was wrong here because we didn't add SOA/NS/SOA to the empty zone
    true -> ok
  end,
  if DBOp == sendNhotcache ->
    CTime=ioc2rpz_fun:curr_serial_60(),%erlang:system_time(seconds),
    ets:insert(rpz_hotcache_table, {{pkthotcache,Zone#rpz.zone,PktN},CTime, term_to_binary({0,0,0,0, []},[{compressed,?Compression}])}); %looks like the same issue as above was 0,3,0,0
    true -> ok
  end,
	{ok, 0, 0};


send_packets(Socket,IOC, [], _ACount, _PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,0,IXFRNewR,Proto) when T_ZIP_L /= 0 -> % первый пакет
  SOANSSize = if PktN == 0 ->
    byte_size(<<SOAREC/binary,NSRec/binary>>);
    true -> 0
  end,
  %TODO split IOC by # cores and spawn for DBOp == cache
  %sequential
  %send_packets(Socket,IOC, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto);
	%	{ok, Zone#rpz.rule_count, Zone#rpz.ioc_count};
  %concurrent
  if DBOp == cache ->
      [IOC1,IOC2]=ioc2rpz_fun:split(IOC,?IOCperProc),
      ParentPID = self(),
%      spawn_opt(ioc2rpz,send_packets,[Socket,IOC1, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC, NSRec, Zone, MP, PktHLen, 0, TSIG, PktN, DBOp, SOANSSize, IXFRNewR,Proto],[{fullsweep_after,0}]),
%  ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]) ---> init_T_ZIP_L(Zone)
      PID=spawn_opt(fun() ->
        ParentPID ! {ok, self(), ioc2rpz:send_packets(Socket,IOC1, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC, NSRec, Zone#rpz{rule_count=0, ioc_count=0}, MP, PktHLen, init_T_ZIP_L(Zone), TSIG, PktN, DBOp, SOANSSize, IXFRNewR, Proto) }
        end
        ,[{fullsweep_after,0}]),
      %ioc2rpz_fun:logMessage("Zone ~p started ~p ~n",[Zone#rpz.zone_str, PID]),
      if IOC2 /= [] ->
        {ok, NRules1, NIOCs1}=ioc2rpz:send_packets(<<>>,IOC2, [], 0, 0, true, <<>>, Questions, SOAREC,NSRec,Zone#rpz{rule_count=0, ioc_count=0},MP,PktHLen,T_ZIP_L,[],PktN+100,cache,0,false,Proto);
        true -> NRules1=0, NIOCs1=0
      end,
      {ok, NRules, NIOCs}=w_send_packets(PID, Zone#rpz.zone_str);
    true ->
      {ok, NRules, NIOCs}=send_packets(Socket,IOC, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC,NSRec,Zone#rpz{rule_count=0, ioc_count=0},MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto),
			NRules1=0, NIOCs1=0
  end,
	{ok, NRules+NRules1, NIOCs+NIOCs1};

%send_packets(Socket,IOC, [], _ACount, _PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,0,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) ->
%  %ioc2rpz_fun:logMessage("Zone ~p zip ~n",[Zone#rpz.zone_str]),
%  T_ZIP_L = ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]),
%  send_packets(Socket,IOC, <<>> , 0, SOANSSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto);

% последний пакет, нужно отсылать
send_packets(Socket,[], Pkt, ACount, _PSize, _Zip, PktH, Questions, SOAREC,NSREC,Zone,_,_,_,TSIG,PktN,DBOp,_SOANSSize,IXFRNewR,Proto) ->
%%%TODO save # of rules for AXRF
  case {PktN, IXFRNewR} of
    {0, false} -> PktF=[SOAREC,NSREC,Pkt,SOAREC], Cnt=4;
    {0, true} -> PktF=[SOAREC,NSREC,Pkt], Cnt=3;
    _Else -> PktF=Pkt, Cnt=1
  end,
  %ioc2rpz_fun:logMessage("Zone ~p, Last packet ACOUNT ~p, packets ~p, Cnt ~p ~n",[Zone#rpz.zone_str,ACount,(PktN+1),Cnt]), %TODO Debug
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
%    ioc2rpz_fun:logMessage("Evoke write, PID ~p  ~n",[self()]),
    ioc2rpz_db:write_db_pkt(Zone, {PktN,ACount,0,0, Pkt});
    true -> ok
  end,
  if DBOp == sendNhotcache ->
    CTime=ioc2rpz_fun:curr_serial_60(),
    ets:insert(rpz_hotcache_table, {{pkthotcache,Zone#rpz.zone,PktN},CTime, term_to_binary({PktN,ACount,0,0, Pkt},[{compressed,?Compression}])}); %2019-06-13 BUG in live zones # of records ACount+Cnt
    true -> ok
  end,
	{ok, Zone#rpz.rule_count, Zone#rpz.ioc_count};

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
    SendStatus = send_dns(Socket,Pkt2, [Proto,[]]); %TODO analyze response, drop cached records and terminate if there were transmission errors
    true -> SendStatus = ok
  end,
  if (DBOp == cache) or (DBOp == sendNcache) ->
%    ioc2rpz_fun:logMessage("Evoke write, PID ~p  ~n",[self()]),
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
  if (SendStatus == ok) ->
    send_packets(Socket,Tail, <<>> , 0, 0, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG1,PktN+1,DBOp,SOANSSize0,IXFRNewR,Proto);
    true ->
    % remove sources???
        ioc2rpz_fun:logMessage("Communication error. Removing partily cached zone and stopping operations ~n",[]),
        ets:match_delete(rpz_hotcache_table,{{pkthotcache,Zone#rpz.zone,'_'},'_','_'}),
        ioc2rpz_db:delete_db_pkt(Zone),
        erlang:exit(self(), normal)
        %{ok, 0, 0}
  end;

send_packets(Socket,[{IOC,_IOCExp,_IoCType}|Tail], Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) when ((byte_size(IOC)+byte_size(Zone#rpz.zone))>=253) ->
  send_packets(Socket,Tail, Pkt , ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto);

send_packets(Socket,[{IOC,IOCExp,_IoCType}|Tail], Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) when Zone#rpz.ioc_type  == <<"ip">> ->
  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> SOASize=byte_size(SOAREC); true -> SOASize=0 end,
  if (IOCExp>Zone#rpz.serial) or (IOCExp==0) or (DBOp == ixfr) ->
      {ok, Cnt, Rules,_} = gen_rpzrule(reverse_IP(IOC),Zone,?TTL,<<"false">>,<<"ip">>,Zone#rpz.action,PktHLen+PSize+SOASize,T_ZIP_L); % Zone#rpz.zone_str - need t
      true -> Cnt=0, Rules=[]
  end,
  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> Rules1 = [SOAREC | Rules], Cnt1=Cnt+1, IXFRNewR1 = true; true -> Rules1=Rules, Cnt1=Cnt, IXFRNewR1 = IXFRNewR end,
  Pkt1 = list_to_binary([Pkt, Rules1]),
  PSize1 = byte_size(Pkt1)+SOANSSize,
  send_packets(Socket,Tail, Pkt1 , ACount+Cnt1, PSize1, Zip, PktH, Questions, SOAREC,NSRec,Zone#rpz{rule_count=Zone#rpz.rule_count+Cnt, ioc_count=Zone#rpz.ioc_count+1},MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR1,Proto);

send_packets(Socket,[{IOC,IOCExp,_IoCType}|Tail], Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) when Zone#rpz.ioc_type == <<"fqdn">> -> % докидываем записи и пересчитываем размеры
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
  send_packets(Socket,Tail,Pkt1,ACount+Cnt1,PSize1, Zip, PktH, Questions, SOAREC,NSRec,Zone#rpz{rule_count=Zone#rpz.rule_count+Cnt, ioc_count=Zone#rpz.ioc_count+1},MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR1,Proto);

send_packets(Socket,[{IOC,IOCExp,IoCType}|Tail], Pkt, ACount, PSize, Zip, PktH, Questions, SOAREC,NSRec,Zone,MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR,Proto) -> %mixed докидываем записи и пересчитываем размеры
  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> SOASize=byte_size(SOAREC); true -> SOASize=0 end,
  %ioc2rpz_fun:logMessage("Check ~p ~p ~p ~p ~p ~n",[Zone#rpz.zone_str, IOC,IOCExp,Zone#rpz.serial,DBOp]),
  if (IOCExp>Zone#rpz.serial) or (IOCExp==0) or (DBOp == ixfr)  ->
      case IoCType of
        "ip" -> {ok, Cnt, Rules, WRules} = gen_rpzrule(reverse_IP(IOC),Zone,?TTL,<<"false">>,<<"ip">>,Zone#rpz.action,PktHLen+PSize+SOASize,T_ZIP_L), Rules2=[];% Cnt=1;
        "fqdn" -> {ok, _, Rules, WRules} = gen_rpzrule(list_to_binary([IOC,".",Zone#rpz.zone_str,"."]),Zone,?TTL,<<"false">>,Zone#rpz.action,[],PktHLen+PSize+SOASize,T_ZIP_L), {ok,Rules2,Cnt}=gen_wildcard(Zone#rpz.wildcards,Rules, WRules,PSize+PktHLen+SOASize); % Zone#rpz.zone_str SOASize-не было 20191124???
        _ ->
%          ioc2rpz_fun:logMessage("Rex ~p ~p~n",[IOC,IoCType]),
          case re:run(IOC,MP,[global,notempty,{capture,[1],binary}]) of
            {match,_} -> {ok, Cnt, Rules, WRules} = gen_rpzrule(reverse_IP(IOC),Zone,?TTL,<<"false">>,<<"ip">>,Zone#rpz.action,PktHLen+PSize+SOASize,T_ZIP_L), Rules2=[];% Cnt=1;
            _ -> {ok, _, Rules, WRules} = gen_rpzrule(list_to_binary([IOC,".",Zone#rpz.zone_str,"."]),Zone,?TTL,<<"false">>,Zone#rpz.action,[],PktHLen+PSize+SOASize,T_ZIP_L), {ok,Rules2,Cnt}=gen_wildcard(Zone#rpz.wildcards,Rules, WRules,PSize+PktHLen+SOASize) % Zone#rpz.zone_str SOASize-не было 20191124???
          end
      end;
      true -> Cnt=0, Rules=[], Rules2=[]
  end,

  if ((IOCExp>Zone#rpz.serial) or (IOCExp==0)) and (DBOp == ixfr) and (IXFRNewR /= true) -> Rules1 = [SOAREC | Rules], Cnt1=Cnt+1, IXFRNewR1 = true; true -> Rules1=Rules, Cnt1=Cnt, IXFRNewR1 = IXFRNewR end,
  Pkt1 = list_to_binary([Pkt, Rules1, Rules2]),
  PSize1 = byte_size(Pkt1)+SOANSSize,
  send_packets(Socket,Tail, Pkt1 , ACount+Cnt1, PSize1, Zip, PktH, Questions, SOAREC,NSRec,Zone#rpz{rule_count=Zone#rpz.rule_count+Cnt, ioc_count=Zone#rpz.ioc_count+1},MP,PktHLen,T_ZIP_L,TSIG,PktN,DBOp,SOANSSize,IXFRNewR1,Proto).

%% @doc Generates wildcard RPZ rules when wildcards are enabled.
%% When `<<"true">>', creates a `*.' prefixed rule using label compression
%% if the packet offset allows it (below 16#3FFF), otherwise uses uncompressed labels.
%% When `<<"false">>', returns no additional rules (count=1 for the base rule only).
%% @param WCards `<<"true">>' or `<<"false">>'.
%% @param Rules The base rule binaries.
%% @param WRules The wildcard-compatible rule binaries (without domain prefix).
%% @param PSize The current packet size for compression offset calculation.
%% @returns `{ok, [WildcardRuleBinaries], Count}'.
gen_wildcard(WCards, [Rules|RESTR], [WRules|RESTWR], PSize) ->
  {ok,Rul1,Cnt1}=gen_wildcard(WCards, Rules, WRules, PSize),
  {ok,Rul2,Cnt2}=gen_wildcard(WCards, RESTR, RESTWR, PSize),
  {ok,[Rul1|Rul2],Cnt1+Cnt2};

gen_wildcard(_WCards, [], [], _PSize) ->
  {ok,[],0};

gen_wildcard(<<"true">>, _Rules, WRules, PSize) when PSize < 16#3FFF ->
  R1Loc=(16#c000 bor PSize), % looks like this is a bug
  Rules2=[<<1,"*",R1Loc:16>>, WRules],
  {ok,Rules2,2};

gen_wildcard(<<"true">>, Rules, _WRules, PSize) when PSize >= 16#3FFF ->
  {ok,[<<1,"*">>,Rules],2};

gen_wildcard(<<"false">>, _Rules, _WRules, _PSize) ->
  {ok,[],1}.

%% @doc Removes whitelisted indicators from the IOC list and deduplicates.
%% When the whitelist is empty, only deduplicates using ordsets.
%% When non-empty, builds a gb_set from whitelist entries and filters them out.
%% @param IOC The list of `{IOCBinary, Expiry, Type}' tuples.
%% @param WL The whitelist as a list of `{IOCBinary, Expiry, Type}' tuples, or `[]'.
%% @returns A deduplicated, whitelist-filtered, sorted list of IOC tuples.
remove_WL(IOC,WL) when WL == [] ->
%the function removes whitelisted indicators from the list of IOC
%remove duplicates
  ordsets:to_list(ordsets:from_list(IOC));

remove_WL(IOC,WL) ->
%TODO do not check expiration date, but save the largest expiration
%  ordsets:to_list(ordsets:subtract(ordsets:from_list(IOC), ordsets:from_list(WL))).
%медлеенее в 2 раза

  %WLSet = gb_sets:from_list(WL), %bug #20
  %%%TODO add ioc_type {E,Exp,IOC_Type}
	WLSet = gb_sets:from_list([E || {E,_Exp,_IoCType} <- WL]),

  %%%TODO add ioc_type {E,Exp,IOC_Type}
  [X || {E,_Exp,_IoCType} = X <- ordsets:to_list(ordsets:from_list(IOC)), not gb_sets:is_element(E, WLSet)]. % TODO duplicates gb_sets vs ordsets

%% @doc Checks if a source is currently being updated by another process.
%% If no other process holds the source (pid is `[]'), claims it by updating
%% the ETS config table with the current process PID. If another process is
%% active, waits and retries. If the other process is dead, reclaims the source.
%% @param Source The `#source{}' record.
%% @param SRC The source name string.
%% @param Pid The PID of the process currently updating the source, or `[]'.
check_source_updating(Source, SRC,[]) ->
  ets:update_element(cfg_table, [source,SRC], [{2, Source#source{pid=self()}}]);

check_source_updating(Source, SRC, Pid) ->
  check_source_updating(Source, SRC, Pid, is_process_alive(Pid)).

check_source_updating(Source, SRC, Pid, true) -> % if the process is alive - wait 0.5 sec and validate after that
  ioc2rpz_fun:logMessage("~p updates ~p source. ~p is waiting...~n",[Pid, SRC, self()]),
  timer:sleep(500+rand:uniform(100)),
  check_source_updating(Source, SRC, Pid, is_process_alive(Pid));

check_source_updating(_Source, SRC, Pid, false) -> % if the proces is not alive - check config if someother process grab it and restart validation
  timer:sleep(500+rand:uniform(100)),
  [[Source]]=ets:match(cfg_table,{[source,SRC],'$2'}),
  if Pid == Source#source.pid ->
    ioc2rpz_fun:logMessage("~p is dead. no other process is pulling it. ~p will download it.~n",[Pid, self()]),
    ets:update_element(cfg_table, [source,SRC], [{2, Source#source{pid=self()}}]);
    true ->
      ioc2rpz_fun:logMessage("~p is dead. Got ~p in the config. ~p is waiting...~n",[Pid, Source#source.pid, self()]),
      check_source_updating(Source, SRC, Source#source.pid)
  end.

%% @doc Fetches IOCs from all sources for a zone and removes whitelisted entries.
%% This is the top-level entry point that collects indicators from all configured
%% sources (via `mrpz_from_ioc/4') and subtracts whitelisted indicators.
%% @param Zone The `#rpz{}' record containing source and whitelist references.
%% @param UType The update type: `axfr' for full zone, `ixfr' for incremental.
%% @returns A deduplicated, whitelist-filtered list of `{IOC, Expiry, Type}' tuples.
mrpz_from_ioc(Zone,UType) -> %Zone - RPZ zone
  remove_WL(mrpz_from_ioc(Zone#rpz.sources,Zone,UType,[]),mrpz_from_ioc(Zone#rpz.whitelist,Zone,axfr,[])).% -- WL.

%% @doc Recursively fetches IOCs from a list of source names.
%% For each source, checks the hot cache first; if cached and not expired,
%% uses the cached data. Otherwise downloads fresh data via `ioc2rpz_conn:get_ioc/2',
%% caches it, and accumulates the results. Handles both AXFR (full) and IXFR
%% (incremental) source URLs. Updates the source's `ioc_count' in `cfg_table'.
%%
%% @param Sources A list of source name strings.
%% @param RPZ The `#rpz{}' record for the zone being updated.
%% @param UType The update type: `axfr' or `ixfr'.
%% @param IOC The accumulated list of IOC tuples from previous sources.
%% @returns A list of `{IOC, Expiry, Type}' tuples from all sources.
mrpz_from_ioc([SRC|REST], RPZ,UType, IOC) -> %List of the sources, RPZ zone, UType - AXFR/IXFR update type, IOC - list of accumulated IOCs
  CTime=RPZ#rpz.serial, %CTime=ioc2rpz_fun:curr_serial(),
  [[Source]]=ets:match(cfg_table,{[source,SRC],'$2'}),
  check_source_updating(Source, SRC,Source#source.pid),
   case {ets:match(rpz_hotcache_table,{{SRC,UType},'$2','$3'}),UType} of
    {[[Timestamp,IOCZip]],axfr} when CTime=<(Timestamp+Source#source.hotcache_time) ->
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
    {[[Timestamp,IOCZip]],ixfr} when CTime=<(Timestamp+Source#source.hotcacheixfr_time) ->
      IOC1=binary_to_term(IOCZip),
      ioc2rpz_fun:logMessage("Got source ~p IXFR from cache ~n",[SRC]); %TODO debug
    {_,ixfr} ->
      %ioc2rpz_fun:logMessage("IXFR request for ~p is not cached by the design~n",[SRC]), %TODO debug
      IOC1=ioc2rpz_conn:get_ioc(list_to_binary(ioc2rpz_fun:constr_ixfr_url(Source#source.ixfr_url,RPZ#rpz.ixfr_nz_update_time,CTime)),Source#source.regex,Source),

      ioc2rpz_fun:logMessage("Memory total ~p before garbage collector. processes ~p binary ~p ~n",[erlang:memory(total)/1024/1024,erlang:memory(processes)/1024/1024,erlang:memory(binary)/1024/1024]), %TODO debug
      erlang:garbage_collect(),
      ioc2rpz_fun:logMessage("Memory total ~p after garbage collector. processes ~p binary ~p ~n",[erlang:memory(total)/1024/1024,erlang:memory(processes)/1024/1024,erlang:memory(binary)/1024/1024]) %TODO debug

  end,
  ets:update_element(cfg_table, [source,SRC], [{2, Source#source{ioc_count=length(IOC1), pid=[]}}]),
  mrpz_from_ioc(REST,RPZ,UType,IOC1 ++ IOC);

mrpz_from_ioc([],_RPZ,_UType,IOC) ->
  IOC.

%% @doc Generates DNS RPZ rule resource records for a given domain and action.
%% This is the core RPZ rule generator that converts an IOC domain name and
%% RPZ action into DNS wire-format resource records. Supports label compression
%% via the T_ZIP_L ETS table.
%%
%% When Wildcard is `<<"true">>', generates both the base rule and a wildcard
%% (`*.' prefix) rule. When `<<"false">>', generates a single rule.
%%
%% Supported actions:
%% - `<<"nxdomain">>' - CNAME to root (`.') causing NXDOMAIN
%% - `<<"nodata">>' - CNAME to `*' causing empty answer
%% - `<<"passthru">>' - CNAME to `rpz-passthru' (allow through)
%% - `<<"drop">>' - CNAME to `rpz-drop' (silently drop)
%% - `<<"tcp-only">>' - CNAME to `rpz-tcp-only' (force TCP retry)
%% - `{<<"redirect_domain">>, Domain}' - CNAME to specified domain
%% - `{<<"redirect_ip">>, IP}' - A/AAAA record with specified IP
%% - `{<<"local_txt">>, Text}' - TXT record with specified text
%% - `<<"ip">>', `<<"nsdname">>', `<<"nsip">>' - Special RPZ trigger types
%%   that append the appropriate RPZ trigger prefix to the domain
%% - List of actions - generates multiple rules per IOC
%%
%% @param Domain The fully qualified domain name binary for the RPZ rule.
%% @param RPZ The `#rpz{}' record for the zone.
%% @param TTL The TTL value for the generated records.
%% @param Wildcard `<<"true">>' or `<<"false">>' for wildcard generation.
%% @param Action The RPZ action (see above).
%% @param LocData Additional location data (action-specific, or `[]').
%% @param PktHLen The current packet offset for label compression.
%% @param T_ZIP_L The label compression ETS table ID.
%% @returns `{ok, Count, [RuleBinaries], [WildcardRuleBinaries]}'.

%% This branch is used only for the sample zone (wildcard=true)
gen_rpzrule(Domain,RPZ,TTL,<<"true">>,Action, LocData,PktHLen,T_ZIP_L) -> %wildcard = true
  {ok,Cnt1,Pkt1,_WPkt1} = gen_rpzrule(Domain,RPZ,TTL,<<"false">>,Action, LocData,PktHLen,T_ZIP_L),
  {ok,Cnt2,Pkt2,_WPkt2} = gen_rpzrule(<<"*.",Domain/binary>>,RPZ,TTL,<<"false">>,Action, LocData,PktHLen+byte_size(list_to_binary(Pkt1)),T_ZIP_L),

%TODO switchto wildcards
%	{ok, Cnt1, Pkt1, WPkt1} = gen_rpzrule(Domain,RPZ,?TTL,<<"false">>,Action,[],PktHLen,T_ZIP_L), %LocData=последний[] % Zone#rpz.zone_str
%	{ok,Pkt2,Cnt2}=gen_wildcard(<<"true">>,Pkt1, WPkt1,PktHLen),

  {ok,Cnt1+Cnt2,[Pkt1,Pkt2],[]};


gen_rpzrule(Domain,RPZ,TTL,<<"false">>,<<"nxdomain">>,[],PktHLen,T_ZIP_L) -> %wildcard = false
  case domstr_to_bin_zip(Domain,PktHLen,T_ZIP_L) of
    {error, _} ->
			?logDebugMSG("Zone ~p bad IOC ~p ~n",[RPZ#rpz.zone_str,Domain]),
      {ok,0,[],[]};
    {_,BDomain} -> %ok, zip
      {ok,1,[list_to_binary([BDomain,<<?T_CNAME:16,?C_IN:16, TTL:32,1:16,0>>])],[<<?T_CNAME:16,?C_IN:16, TTL:32,1:16,0>>]}
  end;

gen_rpzrule(Domain,_RPZ,TTL,<<"false">>,Action,_,PktHLen,T_ZIP_L) when Action==<<"nodata">>;Action==<<"passthru">>;Action==<<"drop">>;Action==<<"tcp-only">> -> %wildcard = false
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
      {_,LocDataZ} = domstr_to_bin_zip({ok,[LocData]},0,PktHLen+10+byte_size(BDomain),T_ZIP_L), % what is 10??
%			{_,LocDataZ} = domstr_to_bin(LocData,0),
      ELDS=byte_size(LocDataZ),
      {ok,1,[list_to_binary([BDomain,<<?T_CNAME:16,?C_IN:16, TTL:32,ELDS:16>>,LocDataZ])],[list_to_binary([<<?T_CNAME:16,?C_IN:16, TTL:32,ELDS:16>>,LocDataZ])]}
  end;

gen_rpzrule(Domain,_RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"redirect_domain">>;Action==<<"local_cname">> ->
  case domstr_to_bin_zip(Domain,PktHLen,T_ZIP_L) of
    {error, _} ->
      {ok,0,[],[]};
    {_,BDomain} -> %ok, zip
     {_,LocDataZ} = domstr_to_bin_zip({ok,LocData},0,PktHLen+10+byte_size(BDomain),T_ZIP_L),
%       ioc2rpz_fun:logMessage("Domain ~p DomainZ ~p ~n",[LocData,LocDataZ]), %TODO debug

%     {_,LocDataZ} = domstr_to_bin(LocData,0),
      ELDS=byte_size(LocDataZ),
      {ok,1,[list_to_binary([BDomain,<<?T_CNAME:16,?C_IN:16, TTL:32,ELDS:16>>,LocDataZ])],[list_to_binary([<<?T_CNAME:16,?C_IN:16, TTL:32,ELDS:16>>,LocDataZ])]}
  end;


gen_rpzrule(Domain,_RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"redirect_ip">>;Action==<<"local_a">>;Action==<<"local_aaaa">> -> %wildcard = false
  Len = byte_size(LocData),
  RType = if Len == 16 -> ?T_AAAA; true -> ?T_A end,
  %ioc2rpz_fun:logMessage("Domain ~p, Action ~p, LocData ~p ~n",[Domain,Action,LocData]), %TODO debug
  case domstr_to_bin_zip(Domain,PktHLen,T_ZIP_L) of
    {error, _} ->
      {ok,0,[],[]};
    {_,BDomain} -> %ok, zip
      {ok,1,[list_to_binary([BDomain,<<RType:16,?C_IN:16, TTL:32,Len:16>>,LocData])],[list_to_binary([<<RType:16,?C_IN:16, TTL:32,Len:16>>,LocData])]}
  end;

gen_rpzrule(Domain,_RPZ,TTL,<<"false">>,{Action,LocData},_,PktHLen,T_ZIP_L) when Action==<<"local_txt">> ->
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


gen_rpzrule(_Domain,_RPZ,_TTL,<<"false">>,[],_,PktHLen,_T_ZIP_L) ->
  {ok,0,[],[]};


gen_rpzrule(Domain,RPZ,TTL,<<"false">>,RType,Action,PktHLen,T_ZIP_L) when RType==<<"ip">>;RType==<<"nsdname">>;RType==<<"nsip">> -> %wildcard = false
  BDomain=list_to_binary([Domain,
    case RType of
      <<"ip">> -> ".rpz-ip.";
      <<"nsdname">> -> ".rpz-nsdname.";
      <<"nsip">> -> ".rpz-nsip."
    end
  ,RPZ#rpz.zone_str,"."]),
  %LAction = case Action of
  %  Action when is_binary(Action) -> Action;
  %  _Else -> <<"nxdomain">>
  %end,
  {_, Cnt , Pkt, WPkt} = gen_rpzrule(BDomain,RPZ,TTL,<<"false">>,Action,[],PktHLen,T_ZIP_L),
  {ok,Cnt,[Pkt], [WPkt]};

gen_rpzrule(Domain,_,_,_,Action,_,_,_) ->
  ioc2rpz_fun:logMessage("Error. Unsupported rule. Domain ~p, Action ~p ~n",[Domain,Action]), %TODO debug
  {ok,0,[<<>>],[<<>>]}.


%reverse_IP(OrigIP) when OrigIP == <<"::1">>;OrigIP == <<"::1/128">>;OrigIP == <<"::01">>;OrigIP == <<"::01/128">> ->
%  <<"128.1.zz">>;

%% @doc Reverses an IP address into RPZ wire format for IP-based triggers.
%% Converts IPv4 addresses like "10.20.30.40" to "32.40.30.20.10" (with /32 prefix).
%% Handles CIDR notation (e.g., "10.0.0.0/8" becomes "8.0.0.0.10").
%% Delegates IPv6 addresses to `reverse_IP6/4'.
%% @param OrigIP The IP address binary string, optionally with CIDR mask.
%% @returns The reversed IP binary string in RPZ format.
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

reverse_IP6(<<>>,[<<>>|TAIL],ZZ) when ZZ == no ->
  reverse_IP6([<<"zz">>],TAIL,yes);
reverse_IP6(<<>>,[DIP|TAIL],ZZ) ->
  reverse_IP6(DIP,TAIL,ZZ);
%reverse_IP6(RIP,[<<>>,<<>>|TAIL],ZZ) when ZZ == no ->
%  reverse_IP6([<<"zz.">>,RIP],TAIL,yes);
reverse_IP6(RIP,[],_ZZ) ->
  list_to_binary(RIP);
reverse_IP6(RIP,[<<>>|TAIL],ZZ) when ZZ == no ->
  reverse_IP6([<<"zz.">>,RIP],TAIL,yes);
reverse_IP6(RIP,[<<>>|TAIL],ZZ) when ZZ == yes ->
  reverse_IP6(RIP,TAIL,ZZ);
reverse_IP6(RIP,[DIP|TAIL],ZZ) ->
  reverse_IP6([DIP,<<".">>,RIP],TAIL,ZZ).

%% @doc Converts a domain name string to DNS wire format binary.
%% Splits the domain by "." and encodes each label with a length prefix byte.
%% Labels longer than 63 bytes are rejected per DNS specification.
%% @param Str The domain name as a binary string.
%% @returns `{ok, Binary}' on success, `{error, []}' if any label exceeds 63 bytes.
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


%% @doc Converts a domain name string to DNS wire format with label compression.
%% Uses the T_ZIP_L ETS table to track previously seen label suffixes and
%% replace them with 2-byte compression pointers (offset at or above 0xC000).
%% Falls back to `domstr_to_bin/2' for very short domains.
%% @param Str The domain name binary string.
%% @param Pos The current byte offset in the DNS packet for compression.
%% @param T_ZIP_L The label compression ETS table ID.
%% @returns `{ok, Binary}' or `{zip, Binary}' (with compression pointers),
%%          or `{error, []}' on invalid input.
domstr_to_bin_zip(Str,Pos,T_ZIP_L) when byte_size(Str) > 2->
%  ioc2rpz_fun:logMessage("Domain ~p ~n",[Str]), %TODO debug
  domstr_to_bin_zip(clean_labels(Str),1,Pos,T_ZIP_L);

domstr_to_bin_zip(Str,_Pos,_T_ZIP_L) ->
%  ioc2rpz_fun:logMessage("Weird domain. Domain ~p ~n",[Str]), %TODO debug
  domstr_to_bin(Str,0).


domstr_to_bin_zip({error,_Labels},_Zero,_Pos,_T_ZIP_L) ->
  %?logDebugMSG("Bad IOC ~p ~n",[Labels]),
  {error,[]};

domstr_to_bin_zip({ok,Labels},Zero,Pos,T_ZIP_L) ->
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

%% @doc Validates and cleans domain name labels.
%% Splits the domain by "." and checks each label contains only valid DNS
%% characters (alphanumeric, hyphen, underscore). Removes empty labels caused
%% by consecutive dots. Returns `{ok, Labels}' or `{error, Str}'.
%% @param Str The domain name binary string.
%% @returns `{ok, [LabelBinaries]}' or `{error, OriginalStr}'.
clean_labels(Str) ->
  Labels = ioc2rpz_fun:split_tail(Str, <<".">>),
	%?logDebugMSG("Labels ~p ~n",[Labels]),
  case clean_labels([], Labels) of
		{ok, LabelsFix} -> {ok, LabelsFix};
		{error, _} -> {error, Str}
	end.

clean_labels(GoodL, [Head|Tail]) when Head == <<>>, Tail =/= [] ->
	%?logDebugMSG("GoodL ~p ~p ~n",[GoodL, Tail]),
	%{error, []};
	clean_labels(GoodL, Tail);

clean_labels(GoodL, [Head|Tail]) ->
	%48 - "0", 57 - "9", 45 - "-", 95 - "_",  65 - "A", 90 - "Z", 97 - "a", 122 - "z"
  case ([X || <<X>> <= Head, not((X >= 48) and (X =< 57)) and not((X >= 65) and (X =< 90)) and not((X >= 97) and (X =< 122)) and (X /= 45) and (X /= 95)]) of
		[] -> clean_labels(GoodL ++ [Head], Tail);
		_ -> % ?logDebugMSG("Bad label - ~p ~n",[Head]),
				{error,[]}
	end;


clean_labels(GoodL, []) when GoodL ==[] ->
	{error, []};

clean_labels(GoodL, []) when GoodL /=[] ->
	{ok, GoodL}.

%% @doc Converts a DNS wire-format binary domain name to a human-readable string.
%% Reads length-prefixed labels and joins them with dots. Stops at a zero-length
%% label, a compression pointer (length 192 or above), or when the remaining binary
%% is shorter than the declared label length.
%% @param Dom The DNS wire-format binary.
%% @returns A string representation of the domain name.
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

%% @doc Converts a binary to a hexadecimal string representation.
%% Each byte is formatted as a 2-digit uppercase hex value separated by spaces.
%% @param Bin The binary to convert.
%% @returns A flat string of hex values.
bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B ", [X]) ||
    X <- binary_to_list(Bin)]).

%% @doc Converts a hexadecimal string to a binary.
%% Parses pairs of hex characters into bytes. Handles odd-length strings by
%% padding the last character with "0".
%% @param S The hex string to convert.
%% @returns A binary.
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

%% @doc Converts an internal Erlang IP address tuple to a human-readable string.
%% Handles IPv4-mapped IPv6 addresses (::ffff:x.x.x.x) by extracting the
%% IPv4 portion. Delegates to `inet_parse:ntoa/1' for formatting.
%% @param IP An IP address tuple ({A,B,C,D} for IPv4 or 8-tuple for IPv6).
%% @returns A string representation of the IP address.
%%% Convert internal IP representation to a string
ip_to_str({0,0,0,0,0,65535,IP1,IP2}) ->
  <<IP1B1:8, IP1B2:8>> = <<IP1:16>>,
  <<IP2B1:8, IP2B2:8>> = <<IP2:16>>,
  %?logDebugMSG("~p:~p ~p.~p.~p.~p~n",[IP1,IP2,IP1B1,IP1B2,IP2B1,IP2B2]),
  inet_parse:ntoa({IP1B1,IP1B2,IP2B1,IP2B2});

ip_to_str(IP) ->
  %?logDebugMSG("~p~n",[IP]),
  inet_parse:ntoa(IP).


%% @doc Initializes the label compression ETS table for a zone.
%% Creates a private ETS set table and pre-populates it with the zone name
%% labels at the standard zone name offset (`?ZNameZipN'), enabling compression
%% of the zone name suffix in all subsequent RPZ rule records.
%% @param Zone The `#rpz{}' record containing the zone_str field.
%% @returns The ETS table ID for the label compression table.
init_T_ZIP_L(Zone) ->
	T_ZIP_L=ets:new(label_zip_table, [{read_concurrency, true}, {write_concurrency, true}, set, private]),
	Labels = ioc2rpz_fun:split_tail(list_to_binary(Zone#rpz.zone_str), <<".">>),
	ets:insert(T_ZIP_L, {Labels, ?ZNameZipN}),
	T_ZIP_L.


%% @doc Extracts a DNS label (domain name) from a binary packet.
%% Handles both regular labels (length-prefixed) and compressed labels
%% (2-byte pointer with high bits 11). Recursively reads labels until
%% a zero-length terminator or compression pointer is encountered.
%% The AddZero parameter controls whether a zero byte is appended at the end.
%% @param Packet The binary packet to extract from.
%% @param AddZero `<<0>>' to append a zero terminator, or `<<>>' for none.
%% @returns `{RemainingBinary, LabelBinary}'.
%Extract zipped or regular label
extract_label(Packet,AddZero) ->
 extract_label(Packet,<<>>,AddZero).

extract_label(<<Zip:8,_/binary>>=Packet, Labels,_AddZero) when Zip >= 192 ->
  <<Label:2/binary,REST/binary>>=Packet,
  {REST, <<Labels/binary,Label/binary>>};

extract_label(<<Zip:8,REST/binary>>=_Packet, Labels,AddZero) when Zip == 0 ->
  {REST, <<Labels/binary,AddZero/binary>>};

extract_label(<<Len:8,_/binary>>=Packet, Labels,AddZero) ->
  <<_:8,Label:Len/bytes,REST/binary>>=Packet,
  extract_label(REST,<<Labels/binary,Len:8,Label/binary>>,AddZero).

%%%%
%%%% EUnit tests
%%%%
extract_label_test() -> [
  ?assert(extract_label(<<16#c00c:16,7,"example",3,"com">>,<<>>) =:= {<<7,"example",3,"com">>,<<16#c00c:16>>}),
  ?assert(extract_label(<<7,"example",16#c00c:16>>,<<>>) =:= {<<>>,<<7,"example",16#c00c:16>>}),
  ?assert(extract_label(<<7,"example",16#c00c:16,0>>,<<>>) =:= {<<0>>,<<7,"example",16#c00c:16>>}),
  ?assert(extract_label(<<7,"example",3,"com",0>>,<<>>) =:= {<<>>,<<7,"example",3,"com">>}),
  ?assert(extract_label(<<7,"example",3,"com",0,7,"example",3,"com">>,<<0>>) =:= {<<7,"example",3,"com">>,<<7,"example",3,"com",0>>})

].

reverse_IP_test() ->[
	?assert(reverse_IP(<<"10.20.30.40">>) =:= <<"32.40.30.20.10">>),
	?assert(reverse_IP(<<"10.20.30.40/24">>) =:= <<"24.40.30.20.10">>),
	?assert(reverse_IP(<<"fc00:01::01">>) =:= <<"128.01.zz.01.fc00">>),
	?assert(reverse_IP(<<"fc00::01/64">>) =:= <<"64.01.zz.fc00">>),
	?assert(reverse_IP(<<"fd00::/8">>) =:= <<"8.zz.fd00">>),
	?assert(reverse_IP(<<"::1">>) =:= <<"128.1.zz">>),
	?assert(reverse_IP(<<"::01/128">>) =:= <<"128.01.zz">>)
].

remove_WL_test() -> [
	?assert(remove_WL([{<<"yellowcabnc.com">>,0,"fqdn"},{<<"google1.com">>,0,"fqdn"},{<<"example1.com">>,0,"fqdn"},{<<"exa1.com">>,0,"fqdn"}],[{<<"yellowcabnc.com">>,0,"fqdn"},{<<"google.com">>,0,"fqdn"},{<<"example.com">>,0,"fqdn"}]) =:= [{<<"exa1.com">>,0,"fqdn"}, {<<"example1.com">>,0,"fqdn"},{<<"google1.com">>,0,"fqdn"}]),
	?assert(remove_WL([{<<"yellowcabnc.com">>,10,"fqdn"},{<<"google.com">>,0,"fqdn"},{<<"ioc2rpz.ru">>,10,"fqdn"}],[{<<"yellowcabnc.com">>,0,"fqdn"},{<<"google.com">>,0,"fqdn"},{<<"isc.com">>,0,"fqdn"},{<<"example.com">>,0,"fqdn"}]) =:= [{<<"ioc2rpz.ru">>,10,"fqdn"}])
].
