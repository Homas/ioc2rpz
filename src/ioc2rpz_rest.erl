-module(ioc2rpz_rest).

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
		Srv_rules = lists:sum(([ X#rpz.rule_count || [X]  <- ets:match(cfg_table,{[rpz,'_'],'_','$2'}), X#rpz.rule_count /= undefined])),
		RPZ_stat = [ {X#rpz.zone_str,X#rpz.rule_count} || [X]  <- ets:match(cfg_table,{[rpz,'_'],'_','$2'}), X#rpz.rule_count /= undefined],
		Sources_stat = [ {X#source.name,X#source.ioc_count} || [X]  <- ets:match(cfg_table,{[source,'_'],'$2'}), X#source.ioc_count /= undefined],
		Body=case Format of
			txt     ->  io_lib:format("Srv total RPZ rules: ~p\nRPZ:\n ~p\nSources:\n ~p\n",[Srv_rules,RPZ_stat,Sources_stat]);
					json    ->  io_lib:format("{\"srv_total_rules\":~p,\"rpz\":~s,\"sources\":~s}\n",[Srv_rules,list_tuples_to_json(RPZ_stat),list_tuples_to_json(Sources_stat)])
			end,
		{Body, Req, State};

srv_mgmt(Req, State, Format) when State#state.op == stats_rpz -> % Statistics -- TODO
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
		RPZ_stat = [ {X#rpz.zone_str,X#rpz.rule_count} || [X]  <- ets:match(cfg_table,{[rpz,'_'],'_','$2'}), X#rpz.rule_count /= undefined],
		Body=case Format of
			txt     ->  io_lib:format("RPZ:\n ~p\n",[RPZ_stat]);
					json    ->  io_lib:format("{\"rpz\":~s}\n",[list_tuples_to_json(RPZ_stat)])
			end,
		{Body, Req, State};

srv_mgmt(Req, State, Format) when State#state.op == stats_source -> % Statistics -- TODO
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
		Sources_stat = [ {X#source.name,X#source.ioc_count} || [X]  <- ets:match(cfg_table,{[source,'_'],'$2'}), X#source.ioc_count /= undefined],
		Body=case Format of
			txt     ->  io_lib:format("Sources:\n ~p\n",[Sources_stat]);
					json    ->  io_lib:format("{\"sources\":~s}\n",[list_tuples_to_json(Sources_stat)])
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
	
rest_terminate(Req, State) ->
	ok.

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
    io_lib:format("{\"~s\":\"~b\"}",[Name,Value]).
    
ioc2jsonarr(IOCs) ->
    %ioc2rpz_fun:logMessage("~p\n\n",[IOCs]),
    ioc2jsonarr([],IOCs).

ioc2jsonarr([],[[IOC|_]|REST]) ->
    ioc2jsonarr(io_lib:format("\"~s\"",[binary_to_list(IOC)]),REST);

ioc2jsonarr(Resp,[[IOC|_]|REST]) ->
    ioc2jsonarr(io_lib:format("\"~s\",",[binary_to_list(IOC)])++Resp,REST);

ioc2jsonarr(Resp,[]) ->
    Resp.
