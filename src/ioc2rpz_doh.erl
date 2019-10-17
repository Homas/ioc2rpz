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

-module(ioc2rpz_doh).
-include_lib("eunit/include/eunit.hrl").

-include_lib("ioc2rpz.hrl").

-export([init/2, allowed_methods/2, content_types_accepted/2, content_types_provided/2,parse_dns/2]).

init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.
	
allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>],
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/dns-message">>, parse_dns}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/dns-message">>, parse_dns}
     ], Req, State}.
		 
parse_dns(#{method := Method} = Req, State) when Method == <<"GET">> ->
	DNSMessage = case cowboy_req:match_qs([{dns, [], <<>>}], Req) of
	  #{dns := <<>>} -> {error,<<>>};
		#{dns := DNSQ} -> ioc2rpz_fun:base64url_decode(DNSQ)
	end,
	parse_dns(Req, State, DNSMessage);

parse_dns(#{method := Method} = Req, State) when Method == <<"POST">> ->
	DNSMessage = case cowboy_req:has_body(Req) of
		true -> {ok,DNSM,Req0} = read_body(Req,<<>>),{ok,DNSM};
		_ -> Req0=Req, {error, <<>>}
	end,
	parse_dns(Req0, State, DNSMessage).

parse_dns(Req, State, {error, <<>>}) ->
	#{peer := {IP, Port}} = Req,
	ioc2rpz_fun:logMessage("Bad request from ~p:~p. URI: ~p\n",[ioc2rpz:ip_to_str(IP), Port, cowboy_req:uri(Req)]),
	{normal,cowboy_req:reply(400,#{}, "Bad request\n", Req),State};

parse_dns(Req, State, {ok, DNSMessage}) ->
	#{peer := {IP, Port}} = Req,
	ioc2rpz_fun:logMessage("DNS request from ~p:~p. ~p Host: ~p, Path: ~p. Message ~p\n",[ioc2rpz:ip_to_str(IP), Port, cowboy_req:method(Req), cowboy_req:host(Req), cowboy_req:path(Req), DNSMessage]),
	Respond = case ioc2rpz:parse_dns_request(<<>>, DNSMessage, #proto{proto=doh, rip=IP, rport=Port}) of
		{ok, Data} -> {Data, Req, State};
		_ -> Req0=cowboy_req:reply(400,#{}, "Bad request\n", Req), {normal, Req0, State}
	end,
	Respond.
		
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.
