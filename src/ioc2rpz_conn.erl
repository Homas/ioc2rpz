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

%IOC2RPZ Connectors

-module(ioc2rpz_conn).
-include_lib("ioc2rpz.hrl").
-export([get_ioc/3,clean_feed_bin/2,clean_feed/2]).

get_ioc(URL,REGEX,Source) ->
  case get_ioc(URL,?Src_Retry) of
    {ok, Bin} ->
      ioc2rpz_fun:logMessage("Source: ~p, size: ~s (~p), MD5: ~p ~n",[Source#source.name, ioc2rpz_fun:conv_to_Mb(byte_size(Bin)),byte_size(Bin), ioc2rpz_fun:bin_to_hexstr(crypto:hash(md5,Bin))]), %TODO debug

      %TODO spawn cleanup
      CTime=ioc2rpz_fun:curr_serial_60(),
      %L=[ {ioc2rpz_fun:bin_to_lowcase(X),Y} || {X,Y} <- clean_feed(ioc2rpz_fun:split_tail(Bin,<<"\n">>),REGEX) ],
      L=p_clean_feed(ioc2rpz_fun:split_tail(Bin,<<"\n">>),REGEX,Source#source.max_ioc), 
      
      ioc2rpz_fun:logMessage("Source: ~p, got ~p indicators, clean time ~p ~n",[Source#source.name, length(L), (ioc2rpz_fun:curr_serial_60()-CTime)]), %TODO debug
      L;
    _ ->
      []
  end.

w_clean_feed(PID) ->
  receive 
    { ok, PID, IOC } -> [ {ioc2rpz_fun:bin_to_lowcase(X),Y} || {X,Y} <- IOC ]
  end.

p_clean_feed(IOC,REGEX,Max) when Max == undefined; Max == 0 ->
  p_clean_feed(IOC,REGEX,Max,0);

p_clean_feed(IOC,REGEX,Max) when Max /= undefined ->
  lists:sublist(p_clean_feed(IOC,REGEX,Max,0),Max).
  
p_clean_feed(IOC,REGEX,Max,Count)  ->
  ParentPID = self(),
  [IOC1,IOC2]=ioc2rpz_fun:split(IOC,?IOCperProc),  
  PID=spawn_opt(fun() ->
      ParentPID ! {ok, self(), ioc2rpz_conn:clean_feed(IOC1,REGEX)  }
      end
      ,[{fullsweep_after,0}]),
  L = if IOC2 /= [] , Count+?IOCperProc < Max ; IOC2 /= [],Max == undefined; IOC2 /= [],Max == 0 ->
    p_clean_feed(IOC2,REGEX,Max,Count+?IOCperProc);
    true -> []
  end,
  w_clean_feed(PID) ++ L .
  
  

%reads IOCs from a local file
get_ioc(<<"file:",Filename/binary>> = URL, Retry) ->
  case file:read_file(Filename) of
    {ok, Bin} ->
      {ok, Bin};
	  {error,Reason} when Retry > 0 ->
	    ioc2rpz_fun:logMessage("Error downloading feed ~p reason ~p. Try ~p ~n",[URL, Reason, (?Src_Retry-Retry)]), %TODO timeout and add retry
			timer:sleep(?Src_Retry_TimeOut*1000),
			get_ioc(URL, Retry-1);
    {error, Reason}  when Retry == 0->
      ioc2rpz_fun:logMessage("Error reading file ~p reason ~p ~n",[Filename, Reason]), 
      {error, Reason}
  end;

%IOCs are provided by a local script
get_ioc(<<"shell:",CMD/binary>> = _URL, Retry) ->
  {ok, list_to_binary(os:cmd(binary_to_list(CMD)))};

%download IOCs from http/https/ftp
get_ioc(<<Proto:5/bytes,_/binary>> = URL, Retry) when Proto == <<"http:">>;Proto == <<"https">>;Proto == <<"ftp:/">> ->
	httpc:set_options([{cookies,enabled}]),
  case httpc:request(get,{binary_to_list(URL),[{"User-Agent", "Mozilla"}]},[],[{body_format,binary},{sync,true},{timeout, SourcePullTimeout}]) of %,{socket_opts,[{cookies,enabled}]}
  {ok,{{_,200,_},_,Response}} ->
    {ok,Response};
  {ok,{{_,Code,_},Headers,Response}} ->
    ioc2rpz_fun:logMessage("Unexpected response code ~p, headers ~p ~n",[Code, Headers]), 
		{ok,<<>>};
  {error,Reason} when Retry > 0 ->
    ioc2rpz_fun:logMessage("Error downloading feed ~p reason ~p. Try ~p ~n",[URL, Reason, (?Src_Retry-Retry)]), %TODO timeout and add retry
		timer:sleep(?Src_Retry_TimeOut*1000),
		get_ioc(URL, Retry-1);
  {error,Reason} when Retry == 0 ->
    ioc2rpz_fun:logMessage("Error downloading feed ~p reason ~p ~n",[URL, Reason]), %TODO timeout and add retry
    {error,Reason}
  end. 

%get_ioc reads IOCs from a local file
%%%get_ioc(<<"file:",Filename/binary>> = _URL,REGEX,Source,stype) ->
%%%  %TODO
%%%  %Check if file > 2Gb, read by chunks
%%%  %TODO
%%%  case file:read_file(Filename) of
%%%    {ok, Bin} ->
%%%      ioc2rpz_fun:logMessage("Source: ~p, size: ~s (~p), MD5: ~p ~n",[Source#source.name, ioc2rpz_fun:conv_to_Mb(byte_size(Bin)),byte_size(Bin), ioc2rpz_fun:bin_to_hexstr(crypto:hash(md5,Bin))]), %TODO debug
%%%      BinLow=ioc2rpz_fun:bin_to_lowcase(Bin),
%%%      %L=[ {ioc2rpz_fun:bin_to_lowcase(X),Y} || {X,Y} <- clean_feed(ioc2rpz_fun:split_tail(Bin,<<"\n">>),REGEX) ],
%%%      L=clean_feed(ioc2rpz_fun:split_tail(BinLow,<<"\n">>),REGEX),
%%%      ioc2rpz_fun:logMessage("Source: ~p, got ~p indicators~n",[Source#source.name, length(L)]), %TODO debug
%%%      L;
%%%    {error, Reason} ->
%%%      ioc2rpz_fun:logMessage("Error reading file ~p reason ~p ~n",[Filename, Reason]), %TODO debug
%%%      []
%%%  end;
%%%
%%%get_ioc(<<"shell:",CMD/binary>> = _URL,REGEX,Source,stype) ->
%%%  Bin=list_to_binary(os:cmd(binary_to_list(CMD))), %, #{ max_size => ?ShellMaxRespSize }
%%%  ioc2rpz_fun:logMessage("Source: ~p, size: ~s (~p), MD5: ~p ~n",[Source#source.name, ioc2rpz_fun:conv_to_Mb(byte_size(Bin)),byte_size(Bin), ioc2rpz_fun:bin_to_hexstr(crypto:hash(md5,Bin))]), %TODO debug
%%%  BinLow=ioc2rpz_fun:bin_to_lowcase(Bin),
%%%  %L=[ {ioc2rpz_fun:bin_to_lowcase(X),Y} || {X,Y} <- clean_feed(ioc2rpz_fun:split_tail(Bin,<<"\n">>),REGEX) ],
%%%  L=clean_feed(ioc2rpz_fun:split_tail(BinLow,<<"\n">>),REGEX),
%%%  ioc2rpz_fun:logMessage("Source: ~p, got ~p indicators~n",[Source#source.name, length(L)]), %TODO debug
%%%  L;
%%%
%%%%get_ioc download IOCs from http/https/ftp
%%%get_ioc(<<Proto:5/bytes,_/binary>> = URL,REGEX,Source,stype) when Proto == <<"http:">>;Proto == <<"https">>;Proto == <<"ftp:/">> ->
%%%%inets, ssl must be started and stopped in supervisor: inets:start(), ssl:start(), ssl:stop(), inets:stop()
%%%
%%%  case httpc:request(get,{binary_to_list(URL),[]},[],[{body_format,binary},{sync,true}]) of
%%%  {ok,{{_,200,_},_,Response}} ->
%%%    ioc2rpz_fun:logMessage("Source: ~p, size: ~s (~p), MD5: ~p ~n",[Source#source.name, ioc2rpz_fun:conv_to_Mb(byte_size(Response)), byte_size(Response), ioc2rpz_fun:bin_to_hexstr(crypto:hash(md5,Response))]), %TODO debug
%%%    BinLow=ioc2rpz_fun:bin_to_lowcase(Response),
%%%    %L=[ {ioc2rpz_fun:bin_to_lowcase(X),Y} || {X,Y} <- clean_feed(ioc2rpz_fun:split_tail(Response,<<"\n">>),REGEX) ],
%%%    L=clean_feed(ioc2rpz_fun:split_tail(BinLow,<<"\n">>),REGEX),
%%%    ioc2rpz_fun:logMessage("Source: ~p, got ~p indicators~n",[Source#source.name, length(L)]), %TODO debug
%%%    L;
%%%  {error,Reason} ->
%%%    ioc2rpz_fun:logMessage("Error downloading feed ~p reason ~p ~n",[URL, Reason]), %TODO debug
%%%    []
%%%  end.
%%%
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
  {ok,MP} = re:compile("^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$",[{newline, any}]),
  [ X || X <- clean_feed(IOC,[],MP), X /= <<>>];


%Extract IOCs,remove unsupported chars using user's defined REGEX. Expiration date is supported. First value - IOC, second - Exp. Date;
clean_feed(IOC,REX) -> %REX - user's regular expression
  {ok,MP} = re:compile(REX,[{newline, any}]),
  [ X || X <- clean_feed(IOC,[],MP), X /= <<>>].

clean_feed([Head|Tail],CleanIOC,REX) ->
  IOC2 = case re:run(Head,REX,[global,notempty,{capture,[1,2],binary}]) of
    {match,[[IOC,<<>>]]} -> {IOC,0};
    {match,[[IOC,EXP]]} -> {IOC,conv_t2i(EXP)};
    _Else -> <<>>
  end,
  clean_feed(Tail, [IOC2 | CleanIOC], REX);

clean_feed([],CleanIOC,_REX) ->
  CleanIOC.


%%%Check memory consumtion
clean_feed_bin(IOC,none) ->
  [ {X,0} || X <- IOC, X /= <<>>];

clean_feed_bin(IOC,[]) ->
  {ok,MP} = re:compile("^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$",[{newline, any}]), 
  [ X || X <- clean_feed_bin(IOC,<<>>,MP), X /= <<>>];

clean_feed_bin(IOC,REX) -> %REX - user's regular expression
  {ok,MP} = re:compile(REX,[{newline, any}]), 
  [ X || X <- clean_feed_bin(IOC,<<>>,MP), X /= <<>>].

clean_feed_bin([Head|Tail],CleanIOC,REX) ->
  IOC2 = case re:run(Head,REX,[global,notempty,{capture,[1,2],binary}]) of
    {match,[[IOC,<<>>]]} -> <<(ioc2rpz_fun:bin_to_lowcase(IOC))/binary,",",0,";">>;
    {match,[[IOC,EXP]]} -> <<(ioc2rpz_fun:bin_to_lowcase(IOC))/binary,",",(conv_t2i(EXP))/binary,";">>;
    _Else -> <<>>
  end,
  clean_feed_bin(Tail, <<CleanIOC/binary,IOC2/binary>>, REX);
clean_feed_bin([],CleanIOC,_REX) ->
  [ {A,B} || [A,B] <- [ ioc2rpz_fun:split_tail(X,<<",">>) || X <- ioc2rpz_fun:split_tail(CleanIOC,<<";">>), X /= <<>> ]].
%%%Check memory consumtion


conv_t2i(<<Y:4/bytes,"-",M:2/bytes,"-",D:2/bytes,Sep:1/bytes,HH:2/bytes,":",MM:2/bytes,":",SS:2/bytes,_Rest/binary>>) when Sep==<<"T">>;Sep==<<"t">>;Sep==<<" ">>->
  calendar:datetime_to_gregorian_seconds({{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}, {binary_to_integer(HH), binary_to_integer(MM), binary_to_integer(SS)}})-62167219200;
conv_t2i(_EXP) ->
  0.
