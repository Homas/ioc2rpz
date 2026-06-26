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

%% @doc REST API module for ioc2rpz server management.
%%
%% Implements a `cowboy_rest' handler providing JSON and plain-text
%% endpoints for configuration reload, TSIG key updates, hot-cache
%% management, RPZ zone operations, IOC lookups, and server/RPZ/source
%% statistics.  All endpoints require HTTP Basic authentication against
%% TSIG keys stored in `cfg_table' and an IP-based ACL check.
%% @end
-module(ioc2rpz_rest).
-include_lib("eunit/include/eunit.hrl").

-include_lib("ioc2rpz.hrl").

-export([init/2, allowed_methods/2, content_types_provided/2, to_json/2, to_txt/2, is_authorized/2]).

%-record(state, {op,user}). % can we redefine record???

%% @doc Cowboy REST callback — initialise the handler.
%%
%% Extracts the operation atom (e.g. `reload_cfg', `stats_serv',
%% `catch_all') from the route options and stores it in `#state.op'
%% so that `srv_mgmt/3' can dispatch to the correct handler clause.
%%
%% @param Req   Cowboy request object.
%% @param Opts  Route options list; the head element is the operation atom.
%% @returns `{cowboy_rest, Req, State}'.
%% @end
init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.

%% @doc Cowboy REST callback — declare supported HTTP methods.
%% @returns `{[<<"GET">>, <<"POST">>], Req, State}'.
%% @end
allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>],
    {Methods, Req, State}.

%% @doc Cowboy REST callback — declare provided content types.
%%
%% Maps `application/json' to {@link to_json/2} and `text/plain' to
%% {@link to_txt/2}.  Cowboy selects the handler via content negotiation.
%%
%% @returns `{[{MediaType, Handler}], Req, State}'.
%% @end
content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json},
      {<<"text/plain">>, to_txt}
     ], Req, State}.

%% @doc Cowboy REST callback — authenticate and authorise the request.
%%
%% Performs two checks:
%% <ol>
%%   <li>IP-based ACL — the peer IP must appear in the server's
%%       management ACL (`cfg_table' `srv' record).</li>
%%   <li>HTTP Basic auth — the username must match a TSIG key name
%%       (or key-group member) and the password must equal the
%%       Base64-encoded key secret.</li>
%% </ol>
%% Bearer tokens are logged but currently rejected.  On failure a
%% CEF log message is emitted and a `401' challenge is returned.
%%
%% @param Req   Cowboy request object.
%% @param State Handler state.
%% @returns `{true, Req, State}' on success, or
%%          `{{false, Challenge}, Req, State}' on failure.
%% @end
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
			case {lists:member(UserB,MKeys), ioc2rpz_fun:constant_time_compare(TKey, Password)} of
				{true, true} -> {true, Req, State#state{user=User}};
				_ ->
					Body = io_lib:format("{status: \"error\", msg: \"Authentication failed\"}\n",[]),
                    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(140),[ioc2rpz:ip_to_str(IP), Port, User, cowboy_req:path(Req), ""]),
					Req0=cowboy_req:set_resp_body(Body,Req),
					{{false, <<"Basic">>}, Req0, State}
			end;
		{{bearer, Token}, true} ->
            ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(141),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
			{{false, <<"Token">>}, Req, State#state{user=Token}};

		{_, false} ->
            ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(145),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
			Body = io_lib:format("{status: \"error\", msg: \"Authentication failed\"}\n",[]),
			Req0=cowboy_req:set_resp_body(Body,Req),
			{{false, <<"Basic">>}, Req0, State};
		_ ->
            ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(141),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
			Body = io_lib:format("{status: \"error\", msg: \"Authentication failed\"}\n",[]),
			Req0=cowboy_req:set_resp_body(Body,Req),
			{{false, <<"Basic">>}, Req0, State}
	end.


%% @doc Content handler for `application/json' — delegates to {@link srv_mgmt/3}.
%% @end
to_json(Req, State) ->
%  ioc2rpz_fun:logMessage("Req:\n~p\n\nState:\n~p\n\n",[Req,State]),
	srv_mgmt(Req, State, json).

%% @doc Content handler for `text/plain' — delegates to {@link srv_mgmt/3}.
%% @end
to_txt(Req, State) ->
%  ioc2rpz_fun:logMessage("Req:\n~p\n\nState:\n~p\n\n",[Req,State]),
	srv_mgmt(Req, State, txt).

%	ioc2rpz_fun:logMessage("Req:\n~p\n\nState:\n~p\n\n",[Req,State]),

%% @doc Dispatch an authenticated REST request to the appropriate handler.
%%
%% The function is multi-clause, guarded on `State#state.op'.  Each clause
%% handles one REST endpoint, logs a CEF audit event, performs the
%% operation, and returns a response body in the requested `Format'
%% (`json' or `txt').
%%
%% Supported operations (one clause per endpoint):
%% <ul>
%%   <li>`reload_cfg'           — reload the full server configuration.</li>
%%   <li>`update_tkeys'         — reload TSIG keys only.</li>
%%   <li>`cache_sources_clear_all' — purge every source from the hot cache.</li>
%%   <li>`cache_sources_clear_one' — purge a single named source from the hot cache.</li>
%%   <li>`cache_sources_load_all'  — pre-load all sources into the hot cache.</li>
%%   <li>`update_all_rpz'       — force-update every RPZ zone.</li>
%%   <li>`update_rpz'           — force-update a single RPZ zone by name.</li>
%%   <li>`terminate'            — gracefully shut down the server.</li>
%%   <li>`stats_serv'           — return combined server, RPZ, and source statistics.</li>
%%   <li>`stats_rpz'            — return RPZ zone statistics only.</li>
%%   <li>`stats_source'         — return IOC source statistics only.</li>
%%   <li>`get_rpz'              — export all IOC records for a given RPZ zone.</li>
%%   <li>`get_ioc'              — look up which RPZ feeds contain a given IOC.</li>
%%   <li>`catch_all'            — fallback for unrecognised endpoints (returns error).</li>
%% </ul>
%%
%% @param Req    Cowboy request object.
%% @param State  Handler state containing `#state.op' and `#state.user'.
%% @param Format `json' | `txt'.
%% @returns `{Body, Req, State}'.
%% @end
srv_mgmt(Req, State, Format) when State#state.op == reload_cfg -> %Reload server configuration
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
	{Body,Req0} = case {ioc2rpz_sup:reload_config3(reload), Format} of
		{ok, json} -> {"{\"status\":\"ok\",\"msg\":\"Configuration reloaded\"}\n",Req};
		{ok, txt} -> {"status: ok\nmsg: Configuration reloaded\n",Req};
		{_, json} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(146),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {"{\"status\":\"error\",\"msg\":\"Configuration reload error\"}\n",cowboy_req:reply(520, Req)};
		{_, txt} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(146),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {"status: error\nmsg: Configuration reload error\n",cowboy_req:reply(520, Req)}
	end,
	{Body, Req0, State};


srv_mgmt(Req, State, Format) when State#state.op == update_tkeys -> %Reload TSIG keys from the configuration (other records are not updated)
	#{peer := {IP, Port}} = Req,
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(230),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
	{Body,Req0} = case {ioc2rpz_sup:reload_config3(updTkeys), Format} of
		{ok, json} -> {"{\"status\":\"ok\",\"msg\":\"TSIG keys were updated\"}\n",Req};
		{ok, txt} -> {"status: ok\nmsg: TSIG keys were updated\n",Req};
		{_, json} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(146),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {"{\"status\":\"error\",\"msg\":\"TSIG keys update error\"}\n",cowboy_req:reply(520, Req)};
		{_, txt} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(146),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {"status: error\nmsg: TSIG keys update error\n",cowboy_req:reply(520, Req)}
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
		[X] ->
      %X#rpz.sources
      %ioc2rpz_fun:logMessage("deleting ~p source from the hot cache~n",[Source]),
      %ets:delete(rpz_hotcache_table, {Source,axfr}), ets:delete(rpz_hotcache_table, {Source,ixfr}),

      ioc2rpz_fun:logMessage("debugging ~p ~n",[X#rpz.sources]),

      [ ioc2rpz_fun:logMessage("deleting ~p source~n",[Z]) || Z <- X#rpz.sources ],
      [ ets:delete(rpz_hotcache_table, {Z,Y}) || Z <- X#rpz.sources, Y <-[axfr,ixfr] ],

      spawn_opt(ioc2rpz_sup,update_zone_full,[X],[{fullsweep_after,0}]), true;
		[] -> false
	end,
	{Body,Req0} = case {ZoneS, Format} of
		{true,json} -> {io_lib:format("{\"status\":\"ok\",\"msg\":\"RPZ ~s will be updated\"}\n",[ioc2rpz_fun:json_escape(RPZ)]),Req};
		{true,txt} -> {io_lib:format("status: ok\nmsg: RPZ ~s will be updated\n",[RPZ]),Req};
		{false,json} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(146),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {io_lib:format("{\"status\":\"error\",\"msg\":\"RPZ ~s not found\"}\n",[ioc2rpz_fun:json_escape(RPZ)]),cowboy_req:reply(520, Req)};
		{false,txt} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(146),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {io_lib:format("status: error\nmsg: RPZ ~s not found\n",[RPZ]),cowboy_req:reply(520, Req)}
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
  #{type := Type} = cowboy_req:match_qs([{type, [], <<"both">>}], Req),
%  erlang:display(Type),
	{Body,Req0} = case {Data, Format, Type} of
		{[],json,_} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(148),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {io_lib:format("{\"status\":\"error\",\"msg\":\"RPZ ~s not found\"}\n",[ioc2rpz_fun:json_escape(RPZ)]),cowboy_req:reply(520, Req)};
		{[],txt,_} -> ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(148),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]), {io_lib:format("status: error\nmsg: RPZ ~s not found\n",[RPZ]),cowboy_req:reply(520, Req)};
		{_,json,_} -> {io_lib:format("{\"status\":\"ok\",\"rpz\":\"~s\",\"iocs\":[~s]}\n",[ioc2rpz_fun:json_escape(RPZ),ioc2jsonarr(Data,binary_to_list(Type))]),Req};
%		{_,txt} -> {lists:flatten([ io_lib:format("~s,~s\n",[binary_to_list(X),Type]) || [X,_Ser,_Exp,Type] <- Data]),Req}
    {_,txt,<<"fqdn">>} -> {lists:flatten([ io_lib:format("~s\n",[binary_to_list(X)]) || [X,_Ser,_Exp,"fqdn"] <- Data]),Req};
    {_,txt,<<"ip">>} -> {lists:flatten([ io_lib:format("~s\n",[binary_to_list(X)]) || [X,_Ser,_Exp,"ip"] <- Data]),Req};
    {_,txt,_} -> {lists:flatten([ io_lib:format("~s\n",[binary_to_list(X)]) || [X,_Ser,_Exp,_Type] <- Data]),Req}
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
    ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(147),[ioc2rpz:ip_to_str(IP), Port, cowboy_req:path(Req), ""]),
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

%% @doc Collect per-RPZ-zone statistics from `cfg_table'.
%%
%% Returns a list of property-lists, one per RPZ zone whose
%% `rule_count' is defined.  Each entry contains the zone name,
%% rule/IOC counts, serial numbers, and update timestamps.
%%
%% @returns `[[{Key, Value}]]' suitable for JSON serialisation via
%%          {@link list_tuples_to_json/1}.
%% @end
gen_rpz_stats() ->
	[ [{"name",X#rpz.zone_str},{"status",atom_to_list(X#rpz.status)},{"rule_count",X#rpz.rule_count},{"ioc_count",X#rpz.ioc_count},{"serial",X#rpz.serial},{"serial_ixfr",X#rpz.serial_ixfr},{"update_time",X#rpz.update_time},{"ixfr_update_time",X#rpz.ixfr_update_time},{"ixfr_nz_update_time",X#rpz.ixfr_nz_update_time}] || [X]  <- ets:match(cfg_table,{[rpz,'_'],'_','$2'}), X#rpz.rule_count /= undefined].

%% @doc Collect per-source statistics from `cfg_table'.
%%
%% Returns a list of property-lists, one per IOC source whose
%% `ioc_count' is defined, containing the source name and indicator count.
%%
%% @returns `[[{Key, Value}]]'.
%% @end
gen_source_stats() ->
	[ [{"name",X#source.name},{"ioc_count",X#source.ioc_count}] || [X]  <- ets:match(cfg_table,{[source,'_'],'$2'}), X#source.ioc_count /= undefined].

%% @doc Collect server-wide statistics.
%%
%% Gathers the Erlang node name, total rule count across all RPZ zones,
%% and memory consumption of the hot-cache, AXFR, and IXFR ETS tables.
%% Delegates formatting to `gen_srv_stats/2'.
%%
%% @param Format `json' | `txt'.
%% @returns Formatted string (iolist).
%% @end
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
    io_lib:format("{\"~s\":\"~s\"}",[Name,ioc2rpz_fun:json_escape(Value)]);

tuple_to_json(REST) ->
	Res=mtuple_to_json([],REST),
  io_lib:format("{~s}",[Res]).

mtuple_to_json([],[{Name,Value}|REST]) when is_integer(Value)->
    mtuple_to_json(io_lib:format("\"~s\":~b",[Name,Value]),REST);

mtuple_to_json([],[{Name,Value}|REST]) ->
    mtuple_to_json(io_lib:format("\"~s\":\"~s\"",[Name,ioc2rpz_fun:json_escape(Value)]),REST);

mtuple_to_json(Val,[{Name,Value}|REST]) when is_integer(Value)->
    mtuple_to_json(Val++io_lib:format(",\"~s\":~b",[Name,Value]),REST);

mtuple_to_json(Val,[{Name,Value}|REST]) ->
    mtuple_to_json(Val++io_lib:format(",\"~s\":\"~s\"",[Name,ioc2rpz_fun:json_escape(Value)]),REST);

mtuple_to_json(Val,[]) ->
    Val.

ioc2jsonarr(IOCs,Type) ->
%    ioc2rpz_fun:logMessage("~p\n\n",[IOCs]),
%    erlang:display(IOCs),
    ioc2jsonarr([],IOCs,Type).

ioc2jsonarr([],[[IOC,_,_,IType]|REST],Type) ->
  if (IType == Type) or (Type == "both") ->
    ioc2jsonarr(io_lib:format("\"~s\"",[ioc2rpz_fun:json_escape(IOC)]),REST,Type);
    true -> ioc2jsonarr([],REST,Type)
  end;

ioc2jsonarr(Resp,[[IOC,_,_,IType]|REST],Type) ->
  if (IType == Type) or (Type == "both") ->
    ioc2jsonarr(io_lib:format("\"~s\",",[ioc2rpz_fun:json_escape(IOC)])++Resp,REST,Type);
    true -> ioc2jsonarr(Resp,REST,Type)
  end;

ioc2jsonarr(Resp,[],_) ->
    Resp.


%%%%
%%%% EUnit tests
%%%%
