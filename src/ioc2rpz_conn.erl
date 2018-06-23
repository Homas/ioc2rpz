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

%IOC2RPZ Connectors

-module(ioc2rpz_conn).
-include_lib("ioc2rpz.hrl").
-export([get_ioc/2]).

%get_ioc reads IOCs from a local file
get_ioc(<<"file:",Filename/binary>> = _URL,REGEX) ->
  %TODO
  %Check if file > 2Gb, read by chunks
  %TODO
% TODO remove below after checks 20180622
%  {ok, Bin} = file:read_file(Filename),
%  BinLow=ioc2rpz_fun:bin_to_lowcase(Bin),
%  clean_feed(ioc2rpz_fun:split_tail(BinLow,<<"\n">>),REGEX);
  case file:read_file(Filename) of
    {ok, Bin} ->
      BinLow=ioc2rpz_fun:bin_to_lowcase(Bin),
      clean_feed(ioc2rpz_fun:split_tail(BinLow,<<"\n">>),REGEX);
    {error, Reason} ->
      ioc2rpz_fun:logMessage("Error reading file ~p reason ~p ~n",[Filename, Reason]), %TODO debug
      []
  end;

%get_ioc download IOCs from http/https/ftp
get_ioc(<<Proto:5/bytes,_/binary>> = URL,REGEX) when Proto == <<"http:">>;Proto == <<"https">>;Proto == <<"ftp:/">> ->
%inets, ssl must be started and stopped in supervisor: inets:start(), ssl:start(), ssl:stop(), inets:stop()

% TODO remove below after checks 20180622
%  {ok,{{_,200,_},_,Response}} = httpc:request(get,{binary_to_list(URL),[]},[],[{body_format,binary},{sync,true}]),
%  BinLow=ioc2rpz_fun:bin_to_lowcase(Response),
%  clean_feed(ioc2rpz_fun:split_tail(BinLow,<<"\n">>),REGEX).

  case httpc:request(get,{binary_to_list(URL),[]},[],[{body_format,binary},{sync,true}]) of
  {ok,{{_,200,_},_,Response}} ->
    BinLow=ioc2rpz_fun:bin_to_lowcase(Response),
    clean_feed(ioc2rpz_fun:split_tail(BinLow,<<"\n">>),REGEX);
  {error,Reason} ->
    ioc2rpz_fun:logMessage("Error downloading feed ~p reason ~p ~n",[URL, Reason]), %TODO debug
    []
  end.

%get_ioc(<<"rpz:",RRPZ/binary>>,REGEX) when is_binary(URL) ->
%rpz:alg:keyname:key:rpzfeedname:(IP)
%  ok.
%get_ioc(<<"sql:",RRPZ/binary>>,REGEX) when is_binary(URL) ->
%sql:mysql:name:pwd:connection:(sql)
%  ok.
%STIX/TAXII
%OpenDXL

%No clean REGEX
%Read IOCs. One IOC per a line. Do not perform any modifications. Expiration date is not supported. During a next full zone update (AXFR update). All IOCs are refreshed;
clean_feed(IOC,none) ->
  [ {X,0} || X <- IOC, X /= <<>>];

%Default REFEX
%Extract IOCs,remove unsupported chars using standard REGEX. Expiration date is not supported;
clean_feed(IOC,[]) ->
  {ok,MP} = re:compile("^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$"),
  [ X || X <- clean_feed(IOC,[],MP), X /= <<>>];


%Extract IOCs,remove unsupported chars using user's defined REGEX. Expiration date is supported. First value - IOC, second - Exp. Date;
clean_feed(IOC,REX) -> %REX - user's regular expression
  {ok,MP} = re:compile(REX),
  [ X || X <- clean_feed(IOC,[],MP), X /= <<>>].

clean_feed([Head|Tail],CleanIOC,REX) ->
  IOC2 = case re:run(Head,REX,[global,notempty,{capture,[1,2],binary}]) of
    {match,[[IOC,<<>>]]} -> {IOC,0}; %TODO check string:casefold performance impact on huge feeds
    {match,[[IOC,EXP]]} -> {IOC,conv_t2i(EXP)}; %TODO check string:casefold performance impact on huge feeds
    _Else -> <<>>
  end,
  clean_feed(Tail, [IOC2|CleanIOC],REX);

clean_feed([],CleanIOC,_REX) ->
  CleanIOC.

conv_t2i(<<Y:4/bytes,"-",M:2/bytes,"-",D:2/bytes,Sep:1/bytes,HH:2/bytes,":",MM:2/bytes,":",SS:2/bytes,_Rest/binary>>) when Sep==<<"T">>;Sep==<<" ">>->
  calendar:datetime_to_gregorian_seconds({{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}, {binary_to_integer(HH), binary_to_integer(MM), binary_to_integer(SS)}})-62167219200;
conv_t2i(_EXP) ->
  0.
