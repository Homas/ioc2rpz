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

%IOC2RPZ Functions

-module(ioc2rpz_fun).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ioc2rpz.hrl").
-export([logMessage/2,logMessageCEF/2,strs_to_binary/1,curr_serial/0,curr_serial_60/0,constr_ixfr_url/3,ip_to_bin/1,read_local_actions/1,split_bin_bytes/2,split_tail/2,rsplit_tail/2,
         bin_to_lowcase/1,ip_in_list/2,intersection/2,bin_to_hexstr/1,conv_to_Mb/1,q_class/1,q_type/1,split/2,msg_CEF/1,base64url_decode/1,get_cipher_suites/1]).

logMessage(Message, Vars) ->
  logMessage(group_leader(), Message, Vars).

logMessage(Dest, Message, Vars) ->
 ?addTS(Dest),
 io:fwrite(Dest,Message,Vars).


logMessageCEF(Message, Vars) -> % "Device Event Class ID|Name|Severity|[Extension]" must be passed
  logMessageCEF(group_leader(), Message, Vars).

logMessageCEF(Dest, Message, Vars) ->
 ?addTS(Dest),
 io:fwrite(Dest,"CEF:0|ioc2rpz|ioc2rpz|~s"++Message,[?ioc2rpz_ver|Vars]).

%CEF:Version|Device Vendor|Device Product|Device Version|Device Event Class ID|Name|Severity|[Extension]
% Severity is a string or integer and reflects the importance of the event. The valid string values are Unknown, Low, Medium, High, and Very-High. The valid integer values are 0-3=Low, 4-6=Medium, 7- 8=High, and 9-10=Very-High.

msg_CEF(101)    -> "|000101|Bad DNS packet|3|src=~s spt=~p proto=~p~n";
msg_CEF(102)    -> "|000102|Bad DNS request|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p~n";
msg_CEF(103)    -> "|000103|Refused|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(104)    -> "|000104|TSIG key not found|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(105)    -> "|000105|TSIG Bad MAC|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(106)    -> "|000106|TSIG Bad time|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(107)    -> "|000107|Other TSIG error|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p msg2=~p~n";
msg_CEF(108)    -> "|000108|Wrong TSIG position|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";

msg_CEF(120)    -> "|000120|RPZ not found|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(121)    -> "|000121|RPZ not ready|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";

msg_CEF(130)    -> "|000130|REST API Basic authentication failed|7|src=~s spt=~p username=~p path=~p msg=~p~n";
msg_CEF(131)    -> "|000131|REST API Authentication failed|7|src=~s spt=~p path=~p msg=~p~n";
msg_CEF(135)    -> "|000135|REST MGMT request denied|7|src=~s spt=~p path=~p msg=~p~n";
msg_CEF(136)    -> "|000136|MGMT request failed|7|src=~s spt=~p path=~p msg=~p~n";
msg_CEF(137)    -> "|000137|Unsupported request|7|src=~s spt=~p path=~p msg=~p~n";
msg_CEF(138)    -> "|000138|Zone not found|7|src=~s spt=~p path=~p msg=~p~n";

msg_CEF(201)    -> "|000201|RPZ transfer success|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p  tsigkey=~p transfer_time=~p~n";
msg_CEF(202)    -> "|000202|DNS Query|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p~n";

msg_CEF(230)    -> "|000230|MGMT request|7|src=~s spt=~p path=~p msg=~p~n";

msg_CEF(221)    -> "|000221|DNS Notify|3|dst=~s dpt=~s proto=~p zone=~p~n";
msg_CEF(222)    -> "|000222|DNS Notify error|5|dst=~s dpt=~s proto=~s zone=~p msg=~p~n";

msg_CEF(301)    -> "|000301|MGMT request denied|7|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";

msg_CEF(501)    -> "|000501|Possible DDoS CVE-2004-0789|3|src=~s spt=~p proto=~p~n";

msg_CEF(_)    -> "Not defined~n".

strs_to_binary(Strs) ->
  strs_to_binary(Strs,[]).

strs_to_binary([Head|Tail],Result) ->
  Bin = list_to_binary(Head),
  strs_to_binary(Tail,[Result,Bin]);
strs_to_binary([],Result) ->
  lists:flatten(Result).


curr_serial() ->
  erlang:system_time(seconds).

curr_serial_60() -> %Current serial has a minute resolution to cache IXFR
  CTime=erlang:system_time(seconds),
  CTime - CTime rem 60.



constr_ixfr_url(IUrl,FromTime,ToTime) ->
  constr_ixfr_url(IUrl,FromTime,ToTime,[]).

constr_ixfr_url(["[:FTimestamp:]"|IUrl],FromTime,ToTime,Url) ->
  constr_ixfr_url(IUrl,FromTime,ToTime,Url++integer_to_list(FromTime));

constr_ixfr_url(["[:ToTimestamp:]"|IUrl],FromTime,ToTime,Url) ->
  constr_ixfr_url(IUrl,FromTime,ToTime,Url++integer_to_list(ToTime));

constr_ixfr_url([A|IUrl],FromTime,ToTime,Url) ->
  constr_ixfr_url(IUrl,FromTime,ToTime,Url++A);

constr_ixfr_url([],_FromTime,_ToTime,Url) ->
  Url.

%The function split a binary by a pattern. Is not faster then binary:split. Consider to remove or do predifined pattern.
z_split(Bin,Pattern) when is_binary(Bin),is_binary(Pattern) ->
  z_split(Bin, 0, Pattern).
z_split(Bin,N, Pattern) ->
  case Bin of
    <<_:N/binary,Pattern:1/binary,_/binary>> ->
      <<B1:N/binary,Pattern:1/binary,B2/binary>> = Bin,
      [B1,B2];
    <<_:N/binary>> ->
      [Bin];
    _ ->
      z_split(Bin, N+1,Pattern)
  end.

ip_to_bin(IP) when is_list(IP)->
  ip_to_bin(inet:parse_address(IP));

ip_to_bin({ok,{IP1,IP2,IP3,IP4}}) ->
  <<IP1,IP2,IP3,IP4>>;

ip_to_bin({ok,{IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8}}) ->
  <<IP1:16,IP2:16,IP3:16,IP4:16,IP5:16,IP6:16,IP7:16,IP8:16>>.


read_local_actions(Actions) ->
 read_local_actions(Actions,[]).

read_local_actions([{Act,LData}|REST],Acc) when Act=="local_a";Act=="local_aaaa" ->
 read_local_actions(REST,[{list_to_binary(Act),ioc2rpz_fun:ip_to_bin(LData)}|Acc]);

read_local_actions([{Act,LData}|REST],Acc) when Act=="local_cname" ->
 read_local_actions(REST,[{list_to_binary(Act),list_to_binary(LData)}|Acc]);

read_local_actions([{Act,LData}|REST],Acc) when Act=="local_txt" ->
 LocD=list_to_binary(LData),
 read_local_actions(REST,[{list_to_binary(Act),<<(byte_size(LocD)),LocD/binary>>}|Acc]);


read_local_actions([],Acc) ->
 Acc.


split_bin_bytes(Bin, Size) when byte_size(Bin) >= Size, Size>0 ->
    {Chunk, Rest} = split_binary(Bin, Size),
    [Chunk|split_bin_bytes(Rest, Size)];
split_bin_bytes(<<>>,_Size) ->
    [];
split_bin_bytes(Bin,_Size)  ->
    [Bin].


split_tail(String, Pattern) ->
%  ioc2rpz_fun:logMessage("z_split ~p ~p ~n",[String, Pattern]),
	case binary:split(String, Pattern) of %binary:split
		[First, Second] -> [First | split_tail(Second, Pattern)];
		[First] -> [First];
		[] -> []
	end.

rsplit_tail(String, Pattern) ->
%  ioc2rpz_fun:logMessage("z_split ~p ~p ~n",[String, Pattern]),
	case binary:split(String, Pattern) of %binary:split
		[First, Second] -> rsplit_tail(Second, Pattern) ++ [First];
		[First] -> [First];
		[] -> []
	end.

bin_to_lowcase(A) ->
 << << (b_to_lowcase(C)) >> || << C >> <= A >>.
% << << C >> || << C >> <= A >>.

b_to_lowcase(A) when A>=65,A=<90 ->
 A+32;
b_to_lowcase(A) ->
 A.
 
ip_in_list(IP,LST) -> %TODO check CIDR as well
 lists:member(IP,LST).
 
intersection(L1,L2) -> lists:filter(fun(X) -> lists:member(X,L1) end, L2).

bin_to_hexstr(<<Bin:128/big-unsigned-integer>>) ->
 lists:flatten(io_lib:format("~32.16.0b", [Bin])).


%conv_to_Mb(M) ->
%  list_to_binary(case M of
%    M when M > 1024*1024*1024 -> [integer_to_list(M div 1024*1024*1024), "/Gb"];
%    M when M > 1024*1024 -> [integer_to_list(M div (1024*1024)),"/Mb"];
%    M when M > 1024 -> [integer_to_list(M div 1024),"/Kb"];
%    M -> [integer_to_list(M),"/bytes"]
%  end).
  

conv_to_Mb(Size) when Size >= 1024 -> conv_to_Mb(Size, ["B","KB","MB","GB","TB","PB"]);

conv_to_Mb(Size) ->
 list_to_binary([integer_to_list(Size),"/bytes"]).

conv_to_Mb(S, [_|[_|_] = L]) when S >= 1024 -> conv_to_Mb(S/1024, L);
conv_to_Mb(S, [M|_]) ->
    list_to_binary(io_lib:format("~.2f/~s", [float(S), M])).
    
    
q_class(?C_IN)    -> "IN";
q_class(?C_CHAOS) -> "CHAOS";
q_class(?C_ANY)   -> "ANY";
q_class(QClass)   -> integer_to_list(QClass).

q_type(?T_A)      -> "A";
q_type(?T_NS)     -> "NS";
q_type(?T_CNAME)  -> "CNAME";
q_type(?T_SOA)    -> "SOA";
q_type(?T_TXT)    -> "TXT";
q_type(?T_AAAA)   -> "AAAA";
q_type(?T_OPT)    -> "OPT";
q_type(?T_IXFR)   -> "IXFR";
q_type(?T_AXFR)   -> "AXFR";
q_type(?T_ANY)    -> "ANY";
q_type(?RT_TSIG)  -> "TSIG";
q_type(QType)     -> integer_to_list(QType).


% 1.17
% Split a list into two parts; the length of the first part is given.
% usage: p99:split(List,Length)
% example:
% p99:split([a,b,c],2). =>  [[a,b],[c]]
% p99:split([a,b,c],1). =>  [[a],[b,c]]

split([],_)->
    [];
split([H|T],Index) when Index>0,T==[] ->
    [[H],T];
split([H|T],1)->
    [[H],T];
split([H|T],Index)->
    [RH,RT]=split(T,Index-1),
    [[H|RH],RT].



base64url_decode(Str) ->
	StrURL=binary:replace(binary:replace(Str,<<"-">>,<<"+">>,[global]),<<"_">>,<<"/">>,[global]),
	Pad = case byte_size(StrURL) rem 4 of
		0 -> <<>>;
		1 -> <<>>;
		3 -> <<"=">>;
		2 -> <<"==">>
	end,
	try {ok, base64:decode(<<StrURL/binary, Pad/binary>>)}
	catch
			throw:Term -> {error,<<>>};
			exit:Reason -> {error,<<>>};
			error:Reason:Stk -> {error,<<>>}
	end.


get_cipher_suites('tlsv1.2-1.3') ->
  TLS12=ssl:cipher_suites(default, 'tlsv1.2'),
  TLS13=ssl:cipher_suites(exclusive, 'tlsv1.3'),
  ssl:append_cipher_suites(TLS12,TLS13);

get_cipher_suites(TLSVersion) ->
  ssl:cipher_suites(default, TLSVersion).
  

%%%%
%%%% EUnit tests
%%%%
q_class_test() -> [
	?assert(q_class(?C_IN) =:= "IN"),
	?assert(q_class(42) =:= "42")
].
	
q_type_test() -> [
	?assert(q_type(?T_CNAME) =:= "CNAME"),
	?assert(q_type(42) =:= "42")
].
	
conv_to_Mb_test() -> [
	?assert(conv_to_Mb(42) =:= <<"42/bytes">>),
	?assert(conv_to_Mb(1536) =:= <<"1.50/KB">>),
	?assert(conv_to_Mb(3221225472) =:= <<"3.00/GB">>)
].
	
msg_CEF_test() -> [
	?assert(msg_CEF(138) =:= "|000138|Zone not found|7|src=~s spt=~p path=~p msg=~p~n"),
	?assert(msg_CEF(424242) =:= "Not defined~n")	
].


ip_to_bin_test() ->[
	?assert(ip_to_bin("10.10.10.10") =:= <<10,10,10,10>>),
	?assert(ip_to_bin("fc00::01") =:= <<16#fc00:16,0:16,0:16,0:16,0:16,0:16,0:16,1:16>>)
].

base64url_decode_test() -> [
 ?assert(base64url_decode(<<"AAABAAABAAAAAAAAB2V4YW1wbGUDY29tAAABAAE">>) =:= {ok,<<0,0,1,0,0,1,0,0,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1>>}),
 ?assert(base64url_decode(<<"AAABAAABAAAAAAAAB2V4YW1wbGUDY29tAAABAAE==">>) =:= {error,<<>>})
].

bin_to_lowcase_test() ->[
	?assert(bin_to_lowcase(<<"fC00::01">>) =:= <<"fc00::01">>),
	?assert(bin_to_lowcase(<<"Aaaaaa">>) =:= <<"aaaaaa">>),
	?assert(bin_to_lowcase(<<"bBbBbB">>) =:= <<"bbbbbb">>),
	?assert(bin_to_lowcase(<<"ccC">>) =:= <<"ccc">>),
	?assert(bin_to_lowcase(<<"D">>) =:= <<"d">>),
	?assert(bin_to_lowcase(<<"f">>) =:= <<"f">>),
	?assert(bin_to_lowcase(<<"eeeeeeeeeeeeeeeeeeeeeee">>) =:= <<"eeeeeeeeeeeeeeeeeeeeeee">>)
].