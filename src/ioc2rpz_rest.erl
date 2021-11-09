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

-module(ioc2rpz_rest).
-include_lib("eunit/include/eunit.hrl").

-include_lib("ioc2rpz.hrl").

-export([init/2, allowed_methods/2, content_types_provided/2, to_json/2, to_txt/2, is_authorized/2]).

%-record(state, {op,user}). % can we redefine record???

init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json},
      {<<"text/plain">>, to_txt}
     ], Req, State}.

is_authorized(Req, State) ->
	#{peer := {IP, Port}} = Req,
	[[MKeysT,ACL,Srv]] = ets:match(cfg_table,{srv,'_','_','$4','$5','_','$7'}),
	MKeys=lists:flatten([ MKeysT,[ ets:match(cfg_table,{[key_group,X,'_'],'$3'}) || X <- Srv#srv.key_groups ] ]),

	MGMTIP=ioc2rpz_fun:ip_in_list(ioc2rpz:ip_to_str(IP),ACL),

	case {cowboy_req:parse_header(<<"authorization">>, Req),MGMTIP} of
		{{basic, User, Password}, true} ->
			{UserB, TKey}= case ets:select(cfg_table,[{{[key,'$1'],'$2','_','$4'},[{'==','$2',User}],[['$1','$4']]}]) of
				[[X,Y]] -> {X,base64:encode(Y)};
				[]	-> {false, false}
			end,
			case {lists:member(UserB,MKeys), TKey == Password} of
				{true, true} -> {true, Req, State#state{user=User}};
				_ ->
					Body = io_lib:format("{status: \"error\", msg: \"Authentication failed\"}\n",[]),
                    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(130),[ioc2rpz:ip_to_str(IP), Port, User, cowboy_req:path(Req), ""]),
					Req0=cowboy_req:set_resp_body(Body,Req),
					{{false, <<"Basic">>}, Req0, State}
			end;
		{{bearer, Token}, true} ->
            ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(131),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
			{false, Req, State#state{user=Token}};

		{_, false} ->
            ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(135),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
			Body = io_lib:format("{status: \"error\", msg: \"Authentication failed\"}\n",[]),
			Req0=cowboy_req:set_resp_body(Body,Req),
			{{false, <<"Basic">>}, Req0, State};
		_ ->
            ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(131),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
			Body = io_lib:format("{status: \"error\", msg: \"Authentication failed\"}\n",[]),
			Req0=cowboy_req:set_resp_body(Body,Req),
			{{false, <<"Basic">>}, Req0, State}
	end.


to_json(Req, State) ->
	srv_mgmt(Req, State, json).

to_txt(Req, State) ->
	srv_mgmt(Req, State, txt).

%	ioc2rpz_fun:logMessage("Req:\n~p\n\nState:\n~p\n\n",[Req,State]),

srv_mgmt(Req, State, Format) when State#state.op == reload_cfg -> %Reload server configuration
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
	{Body,Req0} = case {ioc2rpz_sup:reload_config3(reload), Format} of
		{ok, json} -> {"{\"status\":\"ok\",\"msg\":\"Configuration reloaded\"}\n",Req};
		{ok, txt} -> {"status: ok\nmsg: Configuration reloaded\n",Req};
		{_, json} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(136),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {"{\"status\":\"error\",\"msg\":\"Configuration reload error\"}\n",cowboy_req:reply(520, Req)};
		{_, txt} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(136),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {"status: error\nmsg: Configuration reload error\n",cowboy_req:reply(520, Req)}
	end,
	{Body, Req0, State};


srv_mgmt(Req, State, Format) when State#state.op == update_tkeys -> %Reload TSIG keys from the configuration (other records are not updated)
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
	{Body,Req0} = case {ioc2rpz_sup:reload_config3(updTkeys), Format} of
		{ok, json} -> {"{\"status\":\"ok\",\"msg\":\"TSIG keys were updated\"}\n",Req};
		{ok, txt} -> {"status: ok\nmsg: TSIG keys were updated\n",Req};
		{_, json} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(136),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {"{\"status\":\"error\",\"msg\":\"TSIG keys update error\"}\n",cowboy_req:reply(520, Req)};
		{_, txt} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(136),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {"status: error\nmsg: TSIG keys update error\n",cowboy_req:reply(520, Req)}
	end,
	{Body, Req0, State};

srv_mgmt(Req, State, Format) when State#state.op == cache_sources_clear_all -> % clear all sources from the hot cache
	#{peer := {IP, Port}} = Req,
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
  %SW=ets:match(cfg_table, {[source,'_'],'$2'}),
  %[ ioc2rpz_fun:logMessage("deleting ~p source~n",[X#source.name]) || [X] <- SW ],
  %[ ets:delete(rpz_hotcache_table, {X#source.name,Y}) || [X] <- SW, Y <-[axfr,ixfr] ],
  [ ioc2rpz_fun:logMessage("deleting ~p ~p source from the hotcache~n",[X,Y]) || [X,Y] <- ets:match(rpz_hotcache_table,{{'$1','$2'},'_','_'})],
  [ ets:delete(rpz_hotcache_table, {X,Y}) || [X,Y] <- ets:match(rpz_hotcache_table,{{'$1','$2'},'_','_'})],
	Body = case Format of
		json -> "{\"status\":\"ok\",\"msg\":\"All sources were removed from the hotcache\"}\n";
		txt -> "status: ok\nmsg: All sources were removed from the hotcache\n"
	end,
	{Body, Req, State};

srv_mgmt(Req, State, Format) when State#state.op == cache_sources_clear_one-> % clear a source from the hot cache
	#{peer := {IP, Port}} = Req,
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
  Source = binary_to_list(cowboy_req:binding(source, Req)),
  %[ ioc2rpz_fun:logMessage("~p ~p source in cache~n",[X,Y]) || [X,Y] <- ets:match(rpz_hotcache_table,{{'$1','$2'},'_','_'})],
  ioc2rpz_fun:logMessage("deleting ~p source from the hot cache~n",[Source]),
  ets:delete(rpz_hotcache_table, {Source,axfr}), ets:delete(rpz_hotcache_table, {Source,ixfr}),
  %[ ioc2rpz_fun:logMessage("~p ~p source in cache~n",[X,Y]) || [X,Y] <- ets:match(rpz_hotcache_table,{{'$1','$2'},'_','_'})],
	Body = case Format of
		json -> io_lib:format("{\"status\":\"ok\",\"msg\":\"~s source was removed from the hot cache\"}\n",[Source]);
		txt -> io_lib:format("status: ok\nmsg: ~s source was removed from the hot cache\n",[Source])
	end,
	{Body, Req, State};

srv_mgmt(Req, State, Format) when State#state.op == cache_sources_load_all -> % load all sources to the hotcache
	#{peer := {IP, Port}} = Req,
  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
  SW=[X#source.name || [X] <- ets:match(cfg_table, {[source,'_'],'$2'})],
  %ioc2rpz:mrpz_from_ioc(SW,#rpz{serial=ioc2rpz_fun:curr_serial()},axfr,[]),
  spawn_opt(ioc2rpz,mrpz_from_ioc,[SW,#rpz{serial=ioc2rpz_fun:curr_serial()},axfr,[]],[{fullsweep_after,0}]),
	Body = case Format of
		json -> "{\"status\":\"ok\",\"msg\":\"All sources will loaded to the hot cache\"}\n";
		txt -> "status: ok\nmsg: All sources will loaded to the hot cache\n"
	end,
	{Body, Req, State};

srv_mgmt(Req, State, Format) when State#state.op == update_all_rpz -> % Force update all RPZ zones
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
	spawn_opt(ioc2rpz_sup,update_all_zones,[true],[{fullsweep_after,0}]),
	Body = case Format of
		json -> "{\"status\":\"ok\",\"msg\":\"All RPZ zones will be updated\"}\n";
		txt -> "status: ok\nmsg: All RPZ zones will be updated\n"
	end,
	{Body, Req, State};

srv_mgmt(Req, State, Format) when State#state.op == update_rpz -> %Update an RPZ zone
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
	RPZ = binary_to_list(cowboy_req:binding(rpz, Req)),
	Zones = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
	ZoneS = case [ X || [X] <- Zones, X#rpz.zone_str == RPZ ] of
		[X] -> spawn_opt(ioc2rpz_sup,update_zone_full,[X],[{fullsweep_after,0}]), true;
		[] -> false
	end,
	{Body,Req0} = case {ZoneS, Format} of
		{true,json} -> {io_lib:format("{\"status\":\"ok\",\"msg\":\"RPZ ~s will be updated\"}\n",[RPZ]),Req};
		{true,txt} -> {io_lib:format("status: ok\nmsg: RPZ ~s will be updated\n",[RPZ]),Req};
		{false,json} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(136),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {io_lib:format("{\"status\":\"error\",\"msg\":\"RPZ ~s not found\"}\n",[RPZ]),cowboy_req:reply(520, Req)};
		{false,txt} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(136),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {io_lib:format("status: error\nmsg: RPZ ~s not found\n",[RPZ]),cowboy_req:reply(520, Req)}
	end,
	{Body, Req0, State};

srv_mgmt(Req, State, Format) when State#state.op == terminate -> %Shutdown server
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
	Body = case Format of
		json -> "{\"status\":\"ok\",\"msg\":\"Terminating\"}\n";
		txt -> "status: ok\nmsg: Terminating\n"
	end,
	ioc2rpz_sup:stop_ioc2rpz_sup(),
	{Body, Req, State};

srv_mgmt(Req, State, Format) when State#state.op == stats_serv -> % Statistics -- TODO
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
		% io_lib:format("[~s]",[list_tuples_to_json([],Array)])
		Body=case Format of
			txt     ->  io_lib:format("Srv:\n ~s\nRPZ:\n ~p\nSources:\n ~p\n",[gen_srv_stats(txt),gen_rpz_stats(),gen_source_stats()]);
			json    ->  io_lib:format("{\"srv\":~s,\"rpz\":~s,\"sources\":~s}\n",[gen_srv_stats(json),list_tuples_to_json(gen_rpz_stats()),list_tuples_to_json(gen_source_stats())])
			end,
		{Body, Req, State};


srv_mgmt(Req, State, Format) when State#state.op == stats_rpz -> % Statistics -- TODO
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
		Body=case Format of
			txt     ->  io_lib:format("RPZ:\n ~p\n",[gen_rpz_stats()]);
			json    ->  io_lib:format("{\"rpz\":~s}\n",[list_tuples_to_json(gen_rpz_stats())])
			end,
		{Body, Req, State};

srv_mgmt(Req, State, Format) when State#state.op == stats_source -> % Statistics -- TODO
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
		Body=case Format of
			txt     ->  io_lib:format("Sources:\n ~p\n",[gen_source_stats()]);
			json    ->  io_lib:format("{\"sources\":~s}\n",[list_tuples_to_json(gen_source_stats())])
			end,
		{Body, Req, State};



srv_mgmt(Req, State, Format) when State#state.op == get_rpz -> % Get RPZ
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
	RPZ = binary_to_list(cowboy_req:binding(rpz, Req)),
	Zones = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
    Data = case [ X || [X] <- Zones, X#rpz.zone_str == RPZ ] of
		[] -> [];
        [Zone] -> ioc2rpz_db:read_db_record(Zone,0,active)
	end,
	{Body,Req0} = case {Data, Format} of
		{[],json} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(138),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {io_lib:format("{\"status\":\"error\",\"msg\":\"RPZ ~s not found\"}\n",[RPZ]),cowboy_req:reply(520, Req)};
		{[],txt} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(138),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {io_lib:format("status: error\nmsg: RPZ ~s not found\n",[RPZ]),cowboy_req:reply(520, Req)};
		{_,json} -> {io_lib:format("{\"status\":\"ok\",\"rpz\":\"~s\",\"iocs\":[~s]}\n",[RPZ,ioc2jsonarr(Data)]),Req};
		{_,txt} -> {lists:flatten([ io_lib:format("~s\n",[binary_to_list(X)]) || [X,_Y,_Z] <- Data]),Req}
	end,
	{Body, Req0, State};


%srv_mgmt(Req, State, Format) when State#state.op == get_ioc -> % Check in which feeds ioc is included
%	#{peer := {IP, Port}} = Req,
%  ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
%	IOC = binary_to_list(cowboy_req:binding(rpz, Req)),
%
%% need to validate TSIG on the access to the feeds
%
% ioc2rpz_db:lookup_db_record(<<"baddomain1.com">>,no).
% ioc2rpz_db:lookup_db_record(<<"99.98.61.5">>,no).
% DB
%11> ets:select(rpz_ixfr_table,[{{{ioc,'$0',<<"99.98.61.5">>},'$2','$3'},[],[{{'$0','$2','$3'}}]}]).
%[{<<5,108,111,99,97,108,7,105,111,99,50,114,112,122,0>>,
%  1572419220,0},
% {<<8,108,111,99,97,108,45,105,112,7,105,111,99,50,114,112,
%    122,0>>,
%  1572419220,0}]
%12> ets:select(rpz_ixfr_table,[{{{ioc,'$0',<<"baddomain.com">>},'$2','$3'},[],[{{'$0','$2','$3'}}]}]).
%[]
%13> ets:select(rpz_ixfr_table,[{{{ioc,'$0',<<"baddomain1.com">>},'$2','$3'},[],[{{'$0','$2','$3'}}]}]).
%[{<<5,108,111,99,97,108,7,105,111,99,50,114,112,122,0>>,
%  1572419220,0}]
%
%	{Body, Req0, State};


srv_mgmt(Req, State, Format) when State#state.op == get_ioc -> % check IoC
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
		IOC = ioc2rpz_fun:bin_to_lowcase(cowboy_req:binding(ioc, Req)),
		TKEY = try
				maps:get(tkey,cowboy_req:match_qs([tkey],Req)) %%%%% parse_qs
			catch _:_ ->
				<<"">>
		end,
		{Recur, Zones} = get_tkey_zones(TKEY),
    %ioc2rpz_fun:logMessage("Recursion: ~p\nZones: ~p\n\n",[Recur,Zones]),
		Body=format_ioc(ioc2rpz_db:lookup_db_record(IOC,Recur),{IOC,TKEY, Zones},Format),
		{Body, Req, State};


srv_mgmt(Req, State, Format) when State#state.op == catch_all -> % Catch all unsupported requests from authenticated users
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(137),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
    Body = case Format of
		json -> "{\"status\":\"error\",\"msg\":\"Unsupported request\"}\n";
		txt ->  "status: error\nmsg: Unsupported request\n"
    end,
	{Body, Req, State}.
%    Req0 = case Format of
%		json -> cowboy_req:reply(501,#{<<"content-type">> => <<"application/json">>}, ["{\"status\":\"error\",\"msg\":\"Unsupported request\"}\n"],Req);
%		txt ->  cowboy_req:reply(501,#{<<"content-type">> => <<"text/html">>}, ["status: error\nmsg: Unsupported request\n"],Req)
%    end,
%	{false, Req0, State}.

rest_terminate(_Req, _State) ->
	ok.

format_ioc({ok,Results},Req,Format) ->
	format_ioc(Results,Req,Format,"");

format_ioc({error,_Results},{IOC,_TKEY,_Zones},json) ->
	io_lib:format("{\"status\":\"error\", \"ioc\": ~p}",[IOC]);

format_ioc({error,_Results},{IOC,_TKEY,_Zones},txt) ->
	io_lib:format("status: error\nIOC: ~p\n",[IOC]).

format_ioc([],{IOC,TKEY,_Zones},json, Result) ->
 io_lib:format("{\"ioc\":\"~s\", \"tkey\":\"~s\", \"data\":[~s]}\n\n",[IOC,TKEY,Result]);

format_ioc([{El,Feeds}|Results],Req,json,"") ->
	Ind=io_lib:format("{\"ioc\": \"~s\", \"feeds\": ~s}",[El, parse_feeds(Feeds,Req,"",json)]),
	format_ioc(Results,Req,json, Ind);

format_ioc([{El,Feeds}|Results],Req,_Format,Result) ->
	Ind=io_lib:format("{\"ioc\": \"~s\", \"feeds\": ~s}",[El, parse_feeds(Feeds,Req,"",json)]),
	format_ioc(Results,Req,json, Result ++","++ Ind).


parse_feeds([],_Req,Result,json) ->
	"["++Result++"]";

parse_feeds([{Feed, Serial, Exp}|REST],{_IOC,_TKEY,Zones}=Req,"",json) ->
	Memb=maps:is_key(Feed,Zones),
	Feed_Str=if (Memb) -> {FN,TY,WC}=maps:get(Feed,Zones), io_lib:format("{\"feed\":~p, \"wildcard\":~s, \"type\":~p, \"rpz_serial\": ~p, \"ioc_expiration\": ~p}",[FN, WC, binary_to_list(TY), Serial, Exp]); true -> "" end,
	parse_feeds(REST,Req,Feed_Str,json);

parse_feeds([{Feed, Serial, Exp}|REST],{_IOC,_TKEY,Zones}=Req,Result,json) ->
	Memb=maps:is_key(Feed,Zones),
	Feed_Str=if (Memb) -> {FN,TY,WC}=maps:get(Feed,Zones), ","++io_lib:format("{\"feed\":~p, \"wildcard\":~s, \"type\":~p, \"rpz_serial\": ~p, \"ioc_expiration\": ~p}",[FN, WC, binary_to_list(TY), Serial, Exp]); true -> "" end,
	parse_feeds(REST,Req,Result++Feed_Str,json).

%%%
%%% Get zones availble for TKey
%%%
get_tkey_zones(TKey) ->
	{ok, TKeyBin} = ioc2rpz:domstr_to_bin(TKey,0),
	Groups = [ X || [X,_Y] <- ets:match(cfg_table,{[key_group,'$1',TKeyBin],'$3'}) ],
	get_tkey_zones(TKeyBin, Groups, [ X || [X] <- ets:match(cfg_table,{[rpz,'_'],'_','$4'}) ], []). %{X#rpz.zone, X#rpz.zone_str, X#rpz.wildcards, X#rpz.akeys, X#rpz.ioc_type, X#rpz.key_groups}

get_tkey_zones(_TKeyBin, _Groups,[], Zones) ->
	Recur = [ X || {_,{_,_,X}} <- Zones, X == <<"true">> ] /= [],
%	ZNames = [ X || {X,{_,_,_}} <- Zones ],
	{Recur, maps:from_list(lists:flatten(Zones))};

get_tkey_zones(TKeyBin, Groups, [RPZ|Rest], Zones) ->
	KZ = lists:member(TKeyBin, RPZ#rpz.akeys),
	GZ = [X || X <- Groups, lists:member(X,RPZ#rpz.key_groups)],
	AZ=case {KZ,GZ,TKeyBin} of
		{true,_,_} -> [{RPZ#rpz.zone, {RPZ#rpz.zone_str, RPZ#rpz.ioc_type, RPZ#rpz.wildcards}}];
		{_,Gr,_} when Gr /= [] -> [{RPZ#rpz.zone, {RPZ#rpz.zone_str, RPZ#rpz.ioc_type, RPZ#rpz.wildcards}}];
		{_,_,<<0,0>>} -> [{RPZ#rpz.zone,{RPZ#rpz.zone_str, RPZ#rpz.ioc_type, RPZ#rpz.wildcards}}];
		_Else -> []
	end,
	get_tkey_zones(TKeyBin, Groups, Rest, Zones ++ AZ).

gen_rpz_stats() ->
	[ [{"name",X#rpz.zone_str},{"rule_count",X#rpz.rule_count},{"ioc_count",X#rpz.ioc_count},{"serial",X#rpz.serial},{"serial_ixfr",X#rpz.serial_ixfr},{"update_time",X#rpz.update_time},{"ixfr_update_time",X#rpz.ixfr_update_time},{"ixfr_nz_update_time",X#rpz.ixfr_nz_update_time}] || [X]  <- ets:match(cfg_table,{[rpz,'_'],'_','$2'}), X#rpz.rule_count /= undefined].

gen_source_stats() ->
	[ [{"name",X#source.name},{"ioc_count",X#source.ioc_count}] || [X]  <- ets:match(cfg_table,{[source,'_'],'$2'}), X#source.ioc_count /= undefined].

gen_srv_stats(Format) ->
	Srv_rules = lists:sum(([ X#rpz.rule_count || [X]  <- ets:match(cfg_table,{[rpz,'_'],'_','$2'}), X#rpz.rule_count /= undefined])),
	Node=node(),
  WS = erlang:system_info(wordsize),
  MemHC = binary_to_list(ioc2rpz_fun:conv_to_Mb(ioc2rpz_db:db_table_info(rpz_hotcache_table,memory) * WS)),
  MemAXFR = binary_to_list(ioc2rpz_fun:conv_to_Mb(ioc2rpz_db:db_table_info(rpz_axfr_table,memory) * WS)),
  MemIXFR = binary_to_list(ioc2rpz_fun:conv_to_Mb(ioc2rpz_db:db_table_info(rpz_ixfr_table,memory) * WS)),
	gen_srv_stats(Format,[Node,Srv_rules,MemHC,MemAXFR,MemIXFR]).

gen_srv_stats(txt, [Node,Srv_rules,MemHC,MemAXFR,MemIXFR]) ->
  io_lib:format("node_name ~p\n srv_total_rules ~b\n hot_cache_mem ~s\n axfr_table_mem ~s\n ixfr_table_mem ~s\n",[Node,Srv_rules,MemHC,MemAXFR,MemIXFR]);
gen_srv_stats(json, [Node,Srv_rules,MemHC,MemAXFR,MemIXFR]) ->
  io_lib:format("{\"node_name\":\"~p\",\"srv_total_rules\":~b,\"hot_cache_mem\":\"~s\",\"axfr_table_mem\":\"~s\",\"ixfr_table_mem\":\"~s\"}",[Node,Srv_rules,MemHC,MemAXFR,MemIXFR]).

list_tuples_to_json(Array) ->
    io_lib:format("[~s]",[list_tuples_to_json([],Array)]).

list_tuples_to_json([],[E|Rest]) ->
    list_tuples_to_json(tuple_to_json(E),Rest);

list_tuples_to_json(Resp,[E|Rest]) ->
    list_tuples_to_json(tuple_to_json(E)++","++Resp,Rest);

list_tuples_to_json(Resp,[]) ->
    Resp.

tuple_to_json({Name,Value}) when is_integer(Value)->
    io_lib:format("{\"~s\":~b}",[Name,Value]);

tuple_to_json({Name,Value}) ->
    io_lib:format("{\"~s\":\"~s\"}",[Name,Value]);

tuple_to_json(REST) ->
	Res=mtuple_to_json([],REST),
  io_lib:format("{~s}",[Res]).

mtuple_to_json([],[{Name,Value}|REST]) when is_integer(Value)->
    mtuple_to_json(io_lib:format("\"~s\":~b",[Name,Value]),REST);

mtuple_to_json([],[{Name,Value}|REST]) ->
    mtuple_to_json(io_lib:format("\"~s\":\"~s\"",[Name,Value]),REST);

mtuple_to_json(Val,[{Name,Value}|REST]) when is_integer(Value)->
    mtuple_to_json(Val++io_lib:format(",\"~s\":~b",[Name,Value]),REST);

mtuple_to_json(Val,[{Name,Value}|REST]) ->
    mtuple_to_json(Val++io_lib:format(",\"~s\":\"~s\"",[Name,Value]),REST);

mtuple_to_json(Val,[]) ->
    Val.

ioc2jsonarr(IOCs) ->
    %ioc2rpz_fun:logMessage("~p\n\n",[IOCs]),
    ioc2jsonarr([],IOCs).

ioc2jsonarr([],[[IOC|_]|REST]) ->
    ioc2jsonarr(io_lib:format("\"~s\"",[binary_to_list(IOC)]),REST);

ioc2jsonarr(Resp,[[IOC|_]|REST]) ->
    ioc2jsonarr(io_lib:format("\"~s\",",[binary_to_list(IOC)])++Resp,REST);

ioc2jsonarr(Resp,[]) ->
    Resp.


%%%%
%%%% EUnit tests
%%%%
