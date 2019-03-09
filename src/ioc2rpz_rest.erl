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
	[[MKeys,ACL]] = ets:match(cfg_table,{srv,'_','_','$4','$5','_'}),
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
					Req0=cowboy_req:set_resp_body(Body,Req),
					{{false, <<"Basic">>}, Req0, State}
			end;
		{{bearer, Token}, true} ->
			User = <<"get tsig name">>,
			{true, Req, State#state{user=User}};		
		_ ->
			Body = io_lib:format("{status: \"error\", msg: \"Authentication failed\"}\n",[]),
			Req0=cowboy_req:set_resp_body(Body,Req),
			{{false, <<"Basic">>}, Req0, State}
	end.
	

to_json(Req, State) ->
	srv_mgmt(Req, State, json).

to_txt(Req, State) ->
	srv_mgmt(Req, State, txt).

%	ioc2rpz_fun:logMessage("Req:\n~p\n\nState:\n~p\n\n",[Req,State]),

srv_mgmt(Req, State, Format) when State#state.op == reload_cfg ->
	#{peer := {IP, _Port}} = Req,
	{Body,Req0} = case {ioc2rpz_sup:reload_config3(reload), Format} of
		{ok, json} -> {"{\"status\":\"ok\",\"msg\":\"Configuration reloaded\"}\n",Req};
		{ok, txt} -> {"status: ok\nmsg: Configuration reloaded\n",Req};
		{_, json} -> {"{\"status\":\"error\",\"msg\":\"Configuration reload error\"}\n",cowboy_req:reply(520, Req)};
		{_, json} -> {"status: error\nmsg: Configuration reload error\n",cowboy_req:reply(520, Req)}
	end,
	{Body, Req0, State};


srv_mgmt(Req, State, Format) when State#state.op == update_tkeys ->
	#{peer := {IP, _Port}} = Req,
	{Body,Req0} = case {ioc2rpz_sup:reload_config3(updTkeys), Format} of
		{ok, json} -> {"{\"status\":\"ok\",\"msg\":\"TSIG keys were updated\"}\n",Req};
		{ok, txt} -> {"status: ok\nmsg: TSIG keys were updated\n",Req};
		{_, json} -> {"{\"status\":\"error\",\"msg\":\"TSIG keys update error\"}\n",cowboy_req:reply(520, Req)};
		{_, json} -> {"status: error\nmsg: TSIG keys update error\n",cowboy_req:reply(520, Req)}
	end,
	{Body, Req0, State};


srv_mgmt(Req, State, Format) when State#state.op == update_all_rpz ->
	#{peer := {IP, _Port}} = Req,
	spawn_opt(ioc2rpz_sup,update_all_zones,[true],[{fullsweep_after,0}]),
	Body = case Format of
		json -> "{\"status\":\"ok\",\"msg\":\"All RPZ zones will be updated\"}\n";
		txt -> "status: ok\nmsg: All RPZ zones will be updated\n"
	end,
	{Body, Req, State};

srv_mgmt(Req, State, Format) when State#state.op == update_rpz ->
	#{peer := {IP, _Port}} = Req,
	RPZ = binary_to_list(cowboy_req:binding(rpz, Req)),
	Zones = ets:match(cfg_table,{[rpz,'_'],'_','$4'}),
	ZoneS = case [ X || [X] <- Zones, X#rpz.zone_str == RPZ ] of
		[X] -> spawn_opt(ioc2rpz_sup,update_zone_full,[X],[{fullsweep_after,0}]), true;
		[] -> false
	end,
	{Body,Req0} = case {ZoneS, Format} of
		{true,json} -> {io_lib:format("{\"status\":\"ok\",\"msg\":\"RPZ ~s will be updated\"}\n",[RPZ]),Req};
		{true,txt} -> {io_lib:format("status: ok\nmsg: RPZ ~s will be updated\n",[RPZ]),Req};
		{false,json} -> {io_lib:format("{\"status\":\"error\",\"msg\":\"RPZ ~s not found\"}\n",[RPZ]),cowboy_req:reply(520, Req)};
		{false,txt} -> {io_lib:format("status: error\nmsg: RPZ ~s not found\n",[RPZ]),cowboy_req:reply(520, Req)}
	end,
	{Body, Req0, State};

srv_mgmt(Req, State, Format) when State#state.op == terminate ->
	#{peer := {IP, _Port}} = Req,
	Body = case Format of
		json -> "{\"status\":\"ok\",\"msg\":\"Terminating\"}\n";
		txt -> "status: ok\nmsg: Terminating\n"
	end,
	ioc2rpz_sup:stop_ioc2rpz_sup(),
	{Body, Req, State};
		
srv_mgmt(Req, State, Format) -> % TODO Statistics
	#{peer := {IP, Port}} = Req,
	Srv_IOCs = lists:sum(([ element(25,X) || [X]  <- ets:match(cfg_table,{[rpz,'_'],'_','$2'}), element(25,X) /= undefined])),
	RPZ_stat = [ {element(4,X),element(25,X)} || [X]  <- ets:match(cfg_table,{[rpz,'_'],'_','$2'}), element(25,X) /= undefined],
	Sources_stat = [ {element(2,X),element(6,X)} || [X]  <- ets:match(cfg_table,{[source,'_'],'$2'}),element(6,X) /= undefined],
	Body=io_lib:format("Peer: ~s:~p\nSrv IOCs: ~p\nRPZ:\n ~p\nSources:\n ~p\n",[ioc2rpz:ip_to_str(IP),Port,Srv_IOCs,RPZ_stat,Sources_stat]),
	%Body0=Body++io_lib:format("\n\nReq:\n~p\n\nState:\n~p\n\n",[Req,State]),
	{Body, Req, State}.
	
rest_terminate(Req, State) ->
	ok.
