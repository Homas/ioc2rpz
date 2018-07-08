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

%IOC2RPZ Functions

-module(ioc2rpz_fun).
-include_lib("ioc2rpz.hrl").
-export([logMessage/2,strs_to_binary/1,curr_serial/0,curr_serial_60/0,constr_ixfr_url/3,ip_to_bin/1,read_local_actions/1,split_bin_bytes/2,split_tail/2,
         bin_to_lowcase/1,ip_in_list/2,intersection/2,bin_to_hexstr/1,conv_to_Mb/1]).

logMessage(Message, Vars) ->
  logMessage(group_leader(), Message, Vars).

logMessage(Dest, Message, Vars) ->
 {{Y,M,D},{HH,MM,SS}}=calendar:local_time(),
 io:fwrite(Dest,"~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w "++Message,[Y,M,D,HH,MM,SS|Vars]).


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