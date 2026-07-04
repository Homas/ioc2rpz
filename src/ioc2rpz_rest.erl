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

%% Task 13.2 (R1/R4): TXT success terminator. Mirrors the JSON top-level shape
%% (`ioc'/`tkey' plus the accumulated per-match body) using the module's
%% `key: value' txt convention (cf. the txt error clause above). The JSON
%% clauses/output are left byte-for-byte unchanged.
format_ioc([],{IOC,TKEY,_Zones},txt, Result) ->
	io_lib:format("ioc: ~s\ntkey: ~s\n~s",[IOC,TKEY,Result]);

format_ioc([{El,Feeds}|Results],Req,json,"") ->
	Ind=io_lib:format("{\"ioc\": \"~s\", \"feeds\": ~s}",[El, parse_feeds(Feeds,Req,"",json)]),
	format_ioc(Results,Req,json, Ind);

%% Task 13.2: TXT per-match iteration. `txt' now threads through (instead of
%% being coerced to json), so a txt IOC lookup produces a readable text body.
format_ioc([{El,Feeds}|Results],Req,txt,"") ->
	Ind=io_lib:format("match: ~s\n~s",[El, parse_feeds(Feeds,Req,"",txt)]),
	format_ioc(Results,Req,txt, Ind);

format_ioc([{El,Feeds}|Results],Req,txt,Result) ->
	Ind=io_lib:format("match: ~s\n~s",[El, parse_feeds(Feeds,Req,"",txt)]),
	format_ioc(Results,Req,txt, Result ++ Ind);

format_ioc([{El,Feeds}|Results],Req,_Format,Result) ->
	Ind=io_lib:format("{\"ioc\": \"~s\", \"feeds\": ~s}",[El, parse_feeds(Feeds,Req,"",json)]),
	format_ioc(Results,Req,json, Result ++","++ Ind).


parse_feeds([],_Req,Result,json) ->
	"["++Result++"]";

parse_feeds([{Feed, Serial, Exp, Mask}|REST],{_IOC,_TKEY,Zones}=Req,"",json) ->
	Memb=maps:is_key(Feed,Zones),
	%% Task 13.1 (R1/R2/R3/R4): additive `sources' field. All existing fields
	%% (feed, wildcard, type, rpz_serial, ioc_expiration) stay in place and
	%% unchanged; `sources' is appended last via sources_json/3.
	Feed_Str=if (Memb) -> {FN,TY,WC,Sources,Tracked}=maps:get(Feed,Zones), SourcesStr=sources_json(Mask, Sources, Tracked), io_lib:format("{\"feed\":~p, \"wildcard\":~s, \"type\":~p, \"rpz_serial\": ~p, \"ioc_expiration\": ~p, \"sources\": ~s}",[FN, WC, binary_to_list(TY), Serial, Exp, SourcesStr]); true -> "" end,
	parse_feeds(REST,Req,Feed_Str,json);

parse_feeds([{Feed, Serial, Exp, Mask}|REST],{_IOC,_TKEY,Zones}=Req,Result,json) ->
	Memb=maps:is_key(Feed,Zones),
	%% Task 13.1 (R1/R2/R3/R4): additive `sources' field (see clause above).
	Feed_Str=if (Memb) -> {FN,TY,WC,Sources,Tracked}=maps:get(Feed,Zones), SourcesStr=sources_json(Mask, Sources, Tracked), ","++io_lib:format("{\"feed\":~p, \"wildcard\":~s, \"type\":~p, \"rpz_serial\": ~p, \"ioc_expiration\": ~p, \"sources\": ~s}",[FN, WC, binary_to_list(TY), Serial, Exp, SourcesStr]); true -> "" end,
	parse_feeds(REST,Req,Result++Feed_Str,json);

%% Task 13.2 (R1/R4): TXT success terminator — the accumulated per-feed body is
%% returned as-is (the txt lines already carry their own separators).
parse_feeds([],_Req,Result,txt) ->
	Result;

%% Task 13.2 (R1/R4): TXT per-feed rendering. Every existing field is emitted as
%% a `key: value' line (feed, wildcard, type, rpz_serial, ioc_expiration) and the
%% additive `sources:' line is appended last via sources_txt/3. The JSON output
%% (task 13.1) is untouched.
parse_feeds([{Feed, Serial, Exp, Mask}|REST],{_IOC,_TKEY,Zones}=Req,Result,txt) ->
	Memb=maps:is_key(Feed,Zones),
	Feed_Str=if (Memb) ->
			{FN,TY,WC,Sources,Tracked}=maps:get(Feed,Zones),
			SourcesStr=sources_txt(Mask, Sources, Tracked),
			io_lib:format("feed: ~s\nwildcard: ~s\ntype: ~s\nrpz_serial: ~p\nioc_expiration: ~p\nsources: ~s\n",[FN, WC, binary_to_list(TY), Serial, Exp, SourcesStr]);
		true -> "" end,
	parse_feeds(REST,Req,Result++lists:flatten(Feed_Str),txt).

%% @doc Build the additive JSON `sources' field value for one feed match.
%% Resolution rules (design §9.3, R1/R2/R3/R4):
%%  - Single-source feed (length(Sources)==1): emit the one source name as a
%%    single-element array (["only"]) for type consistency with the
%%    multi-source case (always an array); no mask needed (R3).
%%  - Tracking disabled for a multi-source feed (Tracked==false): attribution
%%    is unavailable, emit `null' (R2/R4) so old clients that ignore the field
%%    are unaffected and updated clients can detect "unknown".
%%  - Multi-source + tracked with Mask==0: attribution unknown (e.g. a
%%    pre-upgrade cached row before its one-time AXFR rebuild), emit `null'.
%%  - Multi-source + tracked with a non-zero mask: resolve set bits to source
%%    names via resolve_sources/2 and emit a JSON string array.
%% Returns an iolist suitable for `~s'.
sources_json(_Mask, Sources, _Tracked) when length(Sources) == 1 ->
	[Only] = Sources,
	"[\"" ++ ioc2rpz_fun:json_escape(Only) ++ "\"]";
sources_json(_Mask, _Sources, false) ->
	"null";
sources_json(0, _Sources, true) ->
	%% attribution unknown for this tracked multi-source feed
	"null";
sources_json(Mask, Sources, true) ->
	Names = resolve_sources(Mask, Sources),
	Escaped = [ "\"" ++ ioc2rpz_fun:json_escape(N) ++ "\"" || N <- Names ],
	"[" ++ string:join(Escaped, ",") ++ "]".

%% @doc Build the additive TXT `sources:' line VALUE for one feed match.
%% Follows the same resolution rules as sources_json/3 (design §9.3/§9.4,
%% R1/R2/R3/R4), reusing resolve_sources/2 for bit->name resolution, but renders
%% a plain text string instead of a JSON array. Sentinels are chosen so the
%% output is self-describing for humans triaging a false positive:
%%  - Single-source feed: the one source name (R3).
%%  - Tracking disabled (multi-source): `(disabled)' — attribution not tracked.
%%  - Multi-source tracked with Mask==0: `(unavailable)' — tracked but unknown
%%    (e.g. a pre-upgrade cached row before its one-time AXFR rebuild, R6).
%%  - Multi-source tracked, non-zero mask: comma-separated resolved names.
%% Returns a flat string suitable for `~s'.
sources_txt(_Mask, Sources, _Tracked) when length(Sources) == 1 ->
	[Only] = Sources,
	name_to_str(Only);
sources_txt(_Mask, _Sources, false) ->
	"(disabled)";
sources_txt(0, _Sources, true) ->
	"(unavailable)";
sources_txt(Mask, Sources, true) ->
	Names = resolve_sources(Mask, Sources),
	string:join([ name_to_str(N) || N <- Names ], ", ").

%% @doc Render a source name (binary or string) as a flat string.
name_to_str(N) ->
	lists:flatten(io_lib:format("~s", [N])).

%% @doc Resolve the set bits of a source mask to their source names.
%% Bit i (0-based) maps to the i-th source: `lists:nth(i+1, Sources)'. Names are
%% returned in ascending bit order. Bits >= length(Sources) are ignored
%% defensively (stale/invalid mask).
%%
%% Handles both mask representations (design §8, R7): an INTEGER mask (feeds with
%% =< ?MaskFixnumBits sources) and a binary-bitmap mask `{bitmap, _}' (feeds with
%% more than 63 sources). The integer path below is unchanged; the bitmap path
%% shares the same bit numbering via ioc2rpz_fun:mask_bits/1.
resolve_sources(Mask, Sources) when is_integer(Mask) ->
	N = length(Sources),
	[ lists:nth(I+1, Sources) || I <- lists:seq(0, N-1), (Mask bsr I) band 1 =:= 1 ];
resolve_sources({bitmap, _}=Mask, Sources) ->
	N = length(Sources),
	[ lists:nth(I+1, Sources) || I <- ioc2rpz_fun:mask_bits(Mask), I < N ].

%%%
%%% Get zones availble for TKey
%%%
get_tkey_zones(TKey) ->
	{ok, TKeyBin} = ioc2rpz:domstr_to_bin(TKey,0),
	Groups = [ X || [X,_Y] <- ets:match(cfg_table,{[key_group,'$1',TKeyBin],'$3'}) ],
	%% Read the #srv{} record once (7th element of the {srv,...} cfg_table row)
	%% so each zone's effective source-tracking flag can be resolved against the
	%% server global default. Absent row (e.g. in unit tests) ⇒ default tracking
	%% off (the /4 helper falls back to `false').
	Srv = case ets:match(cfg_table,{srv,'_','_','_','_','_','$7'}) of
		[[S]] -> S;
		_ -> undefined
	end,
	get_tkey_zones(TKeyBin, Groups, Srv, [ X || [X] <- ets:match(cfg_table,{[rpz,'_'],'_','$4'}) ], []). %{X#rpz.zone, X#rpz.zone_str, X#rpz.wildcards, X#rpz.akeys, X#rpz.ioc_type, X#rpz.key_groups}

get_tkey_zones(_TKeyBin, _Groups, _Srv, [], Zones) ->
	Recur = [ X || {_,{_,_,X,_,_}} <- Zones, X == <<"true">> ] /= [],
%	ZNames = [ X || {X,{_,_,_,_,_}} <- Zones ],
	{Recur, maps:from_list(lists:flatten(Zones))};

get_tkey_zones(TKeyBin, Groups, Srv, [RPZ|Rest], Zones) ->
	KZ = lists:member(TKeyBin, RPZ#rpz.akeys),
	GZ = [X || X <- Groups, lists:member(X,RPZ#rpz.key_groups)],
	%% Effective source-tracking flag for this zone (per-feed value resolved
	%% against the server global default; absent #srv{} ⇒ off).
	Tracked = case Srv of
		#srv{} -> ioc2rpz_sup:track_enabled(RPZ, Srv);
		_ -> false
	end,
	%% Per-zone map value carries, in addition to the original 3 fields, the
	%% zone's source name list and its effective tracking flag (5-tuple).
	ZVal = {RPZ#rpz.zone_str, RPZ#rpz.ioc_type, RPZ#rpz.wildcards, RPZ#rpz.sources, Tracked},
	AZ=case {KZ,GZ,TKeyBin} of
		{true,_,_} -> [{RPZ#rpz.zone, ZVal}];
		{_,Gr,_} when Gr /= [] -> [{RPZ#rpz.zone, ZVal}];
		{_,_,<<0,0>>} -> [{RPZ#rpz.zone, ZVal}];
		_Else -> []
	end,
	get_tkey_zones(TKeyBin, Groups, Srv, Rest, Zones ++ AZ).

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

%% Verifies get_tkey_zones/1 (task 11, R1/R4): each zone's per-zone map value is
%% extended from the original 3-tuple {ZoneStr, IoCType, Wildcards} to the
%% 5-tuple {ZoneStr, IoCType, Wildcards, Sources, Tracked}. Sources is the
%% zone's #rpz.sources list and Tracked is the effective tracking flag resolved
%% via ioc2rpz_sup:track_enabled/2 against the server global default. Also
%% verifies the Recur computation still reads wildcards at tuple position 3.
get_tkey_zones_test() ->
  TKey = <<"testkey.example.com">>,
  {ok, TKeyBin} = ioc2rpz:domstr_to_bin(TKey, 0),
  Srv = #srv{track_sources = on},
  Sources = [<<"abuse-ch">>, <<"internal-list">>],
  RPZ = #rpz{zone = <<"rpz.example">>, zone_str = <<"rpz.example">>,
             ioc_type = <<"fqdn">>, wildcards = <<"true">>,
             akeys = [TKeyBin], key_groups = [],
             sources = Sources, track_sources = true},
  catch ets:delete(cfg_table),
  ets:new(cfg_table, [named_table, public, bag]),
  ets:insert(cfg_table, {srv, srv, srv, srv, srv, srv, Srv}),
  ets:insert(cfg_table, {[rpz, <<"rpz.example">>], <<>>, RPZ}),
  {Recur, Zones} = get_tkey_zones(TKey),
  catch ets:delete(cfg_table),
  Val = maps:get(<<"rpz.example">>, Zones),
  [ %% Recur reads wildcards at position 3 of the new 5-tuple value
    ?assert(Recur =:= true),
    %% per-zone value carries sources + effective tracking flag (on ⇒ true)
    ?assertEqual({<<"rpz.example">>, <<"fqdn">>, <<"true">>, Sources, true}, Val) ].

%% Verifies get_tkey_zones/1 with tracking disabled (global default off, no
%% per-feed override) resolves the effective Tracked flag to false while still
%% carrying the source list into the API context.
get_tkey_zones_untracked_test() ->
  TKey = <<"testkey2.example.com">>,
  {ok, TKeyBin} = ioc2rpz:domstr_to_bin(TKey, 0),
  Srv = #srv{track_sources = off},
  Sources = [<<"src-a">>, <<"src-b">>],
  RPZ = #rpz{zone = <<"rpz.untracked">>, zone_str = <<"rpz.untracked">>,
             ioc_type = <<"ip">>, wildcards = <<"false">>,
             akeys = [TKeyBin], key_groups = [],
             sources = Sources, track_sources = undefined},
  catch ets:delete(cfg_table),
  ets:new(cfg_table, [named_table, public, bag]),
  ets:insert(cfg_table, {srv, srv, srv, srv, srv, srv, Srv}),
  ets:insert(cfg_table, {[rpz, <<"rpz.untracked">>], <<>>, RPZ}),
  {Recur, Zones} = get_tkey_zones(TKey),
  catch ets:delete(cfg_table),
  Val = maps:get(<<"rpz.untracked">>, Zones),
  [ ?assert(Recur =:= false),
    ?assertEqual({<<"rpz.untracked">>, <<"ip">>, <<"false">>, Sources, false}, Val) ].

%% Verifies parse_feeds/4 (tasks 12 + 13, R1/R4): the recursive clauses accept
%% per-match 4-tuples {Feed, Serial, Exp, Mask} (lookup_db_record returns the
%% source mask as of tasks 10.1/10.2) and, as of task 13, emit the additive
%% `sources' field resolved from Mask + Sources + Tracked. All existing fields
%% (feed, wildcard, type, rpz_serial, ioc_expiration) MUST stay in place and
%% unchanged; `sources' is appended last. The zone map value is the 5-tuple
%% from task 11.
parse_feeds_mask_threaded_test() ->
  Sources = [<<"abuse-ch">>, <<"internal-list">>],
  Zones = #{<<"rpz.example">> =>
              {<<"rpz.example">>, <<"fqdn">>, <<"true">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  %% single match — Mask has bit 0 set (abuse-ch): sources resolves to it
  Single = lists:flatten(parse_feeds([{<<"rpz.example">>, 1572419220, 0, 1}], Req, "", json)),
  ?assertEqual("[{\"feed\":<<\"rpz.example\">>, \"wildcard\":true, \"type\":\"fqdn\", \"rpz_serial\": 1572419220, \"ioc_expiration\": 0, \"sources\": [\"abuse-ch\"]}]", Single),
  %% two matches — Mask=3 ⇒ both sources; Mask=2 ⇒ internal-list only
  Multi = lists:flatten(parse_feeds([{<<"rpz.example">>, 100, 0, 3},
                                     {<<"rpz.example">>, 200, 5, 2}], Req, "", json)),
  ?assertEqual("[{\"feed\":<<\"rpz.example\">>, \"wildcard\":true, \"type\":\"fqdn\", \"rpz_serial\": 100, \"ioc_expiration\": 0, \"sources\": [\"abuse-ch\",\"internal-list\"]},{\"feed\":<<\"rpz.example\">>, \"wildcard\":true, \"type\":\"fqdn\", \"rpz_serial\": 200, \"ioc_expiration\": 5, \"sources\": [\"internal-list\"]}]", Multi).

%% Task 13.1 (R1): multi-source, tracked. resolve_sources/2 maps the SET bits of
%% an integer mask to source names in ascending bit order; bit 0 -> first
%% source. Mask with bits 0 and 2 set (binary 101 = 5) over ["a","b","c"]
%% resolves to ["a","c"].
resolve_sources_multi_test() ->
  Sources = [<<"a">>, <<"b">>, <<"c">>],
  ?assertEqual([<<"a">>, <<"c">>], resolve_sources(5, Sources)),
  %% ascending order regardless of which bits are set
  ?assertEqual([<<"b">>, <<"c">>], resolve_sources(6, Sources)),
  %% defensive: bits >= length(Sources) are ignored (bit 5 has no source)
  ?assertEqual([<<"c">>], resolve_sources(2#100100, Sources)).

%% Task 13.1 (R1): the JSON `sources' field for a multi-source tracked feed
%% lists the resolved names as a string array. Bits 0,2 over ["a","b","c"].
parse_feeds_sources_multi_test() ->
  Sources = [<<"a">>, <<"b">>, <<"c">>],
  Zones = #{<<"rpz.multi">> =>
              {<<"rpz.multi">>, <<"fqdn">>, <<"true">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.multi">>, 100, 0, 5}], Req, "", json)),
  %% existing fields present and unchanged, plus additive sources ["a","c"]
  ?assertEqual("[{\"feed\":<<\"rpz.multi\">>, \"wildcard\":true, \"type\":\"fqdn\", \"rpz_serial\": 100, \"ioc_expiration\": 0, \"sources\": [\"a\",\"c\"]}]", Out).

%% Task 13.1 (R3): single-source feed emits the one source name as a
%% single-element array, independent of the mask.
parse_feeds_sources_single_test() ->
  Sources = [<<"only">>],
  Zones = #{<<"rpz.single">> =>
              {<<"rpz.single">>, <<"fqdn">>, <<"false">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.single">>, 100, 0, 1}], Req, "", json)),
  ?assertEqual("[{\"feed\":<<\"rpz.single\">>, \"wildcard\":false, \"type\":\"fqdn\", \"rpz_serial\": 100, \"ioc_expiration\": 0, \"sources\": [\"only\"]}]", Out).

%% Task 13.1 (R2/R4): tracking disabled for a multi-source feed ⇒ sources: null.
parse_feeds_sources_disabled_test() ->
  Sources = [<<"a">>, <<"b">>],
  Zones = #{<<"rpz.off">> =>
              {<<"rpz.off">>, <<"ip">>, <<"false">>, Sources, false}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.off">>, 100, 0, 3}], Req, "", json)),
  ?assertEqual("[{\"feed\":<<\"rpz.off\">>, \"wildcard\":false, \"type\":\"ip\", \"rpz_serial\": 100, \"ioc_expiration\": 0, \"sources\": null}]", Out),
  %% existing fields remain present and unchanged
  ?assertNotEqual(nomatch, string:find(Out, "\"feed\":<<\"rpz.off\">>")),
  ?assertNotEqual(nomatch, string:find(Out, "\"rpz_serial\": 100")),
  ?assertNotEqual(nomatch, string:find(Out, "\"ioc_expiration\": 0")).

%% Task 13.1 (R6): multi-source tracked but Mask==0 (attribution unknown, e.g.
%% pre-upgrade cached row) ⇒ sources: null.
parse_feeds_sources_unknown_test() ->
  Sources = [<<"a">>, <<"b">>],
  Zones = #{<<"rpz.unk">> =>
              {<<"rpz.unk">>, <<"ip">>, <<"false">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.unk">>, 100, 0, 0}], Req, "", json)),
  ?assertEqual("[{\"feed\":<<\"rpz.unk\">>, \"wildcard\":false, \"type\":\"ip\", \"rpz_serial\": 100, \"ioc_expiration\": 0, \"sources\": null}]", Out).

%% Task 13.2 (R1/R4): the TXT `sources:' line for a multi-source tracked feed
%% lists the resolved names comma-separated, and every existing field is emitted
%% as a `key: value' line. Bits 0,2 over ["a","b","c"] resolve to "a, c".
parse_feeds_txt_multi_test() ->
  Sources = [<<"a">>, <<"b">>, <<"c">>],
  Zones = #{<<"rpz.multi">> =>
              {<<"rpz.multi">>, <<"fqdn">>, <<"true">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.multi">>, 100, 0, 5}], Req, "", txt)),
  ?assertEqual("feed: rpz.multi\nwildcard: true\ntype: fqdn\nrpz_serial: 100\nioc_expiration: 0\nsources: a, c\n", Out).

%% Task 13.2 (R3): single-source feed emits the one source name on the
%% `sources:' line, independent of the mask.
parse_feeds_txt_single_test() ->
  Sources = [<<"only">>],
  Zones = #{<<"rpz.single">> =>
              {<<"rpz.single">>, <<"fqdn">>, <<"false">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.single">>, 100, 0, 1}], Req, "", txt)),
  ?assertEqual("feed: rpz.single\nwildcard: false\ntype: fqdn\nrpz_serial: 100\nioc_expiration: 0\nsources: only\n", Out).

%% Task 13.2 (R2/R4): tracking disabled for a multi-source feed ⇒ the `sources:'
%% line reads `(disabled)'; all existing fields stay intact.
parse_feeds_txt_disabled_test() ->
  Sources = [<<"a">>, <<"b">>],
  Zones = #{<<"rpz.off">> =>
              {<<"rpz.off">>, <<"ip">>, <<"false">>, Sources, false}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.off">>, 100, 0, 3}], Req, "", txt)),
  ?assertEqual("feed: rpz.off\nwildcard: false\ntype: ip\nrpz_serial: 100\nioc_expiration: 0\nsources: (disabled)\n", Out),
  %% existing fields remain present and unchanged
  ?assertNotEqual(nomatch, string:find(Out, "feed: rpz.off")),
  ?assertNotEqual(nomatch, string:find(Out, "rpz_serial: 100")),
  ?assertNotEqual(nomatch, string:find(Out, "ioc_expiration: 0")).

%% Task 13.2 (R6): multi-source tracked but Mask==0 (attribution unknown, e.g.
%% pre-upgrade cached row) ⇒ the `sources:' line reads `(unavailable)'.
parse_feeds_txt_unknown_test() ->
  Sources = [<<"a">>, <<"b">>],
  Zones = #{<<"rpz.unk">> =>
              {<<"rpz.unk">>, <<"ip">>, <<"false">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.unk">>, 100, 0, 0}], Req, "", txt)),
  ?assertEqual("feed: rpz.unk\nwildcard: false\ntype: ip\nrpz_serial: 100\nioc_expiration: 0\nsources: (unavailable)\n", Out).

%% Task 13.2 (R1/R4): two matches in the same feed render as two consecutive
%% `key: value' blocks; the accumulator concatenates them in order.
parse_feeds_txt_two_matches_test() ->
  Sources = [<<"abuse-ch">>, <<"internal-list">>],
  Zones = #{<<"rpz.example">> =>
              {<<"rpz.example">>, <<"fqdn">>, <<"true">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.example">>, 100, 0, 3},
                                   {<<"rpz.example">>, 200, 5, 2}], Req, "", txt)),
  ?assertEqual("feed: rpz.example\nwildcard: true\ntype: fqdn\nrpz_serial: 100\nioc_expiration: 0\nsources: abuse-ch, internal-list\n"
               "feed: rpz.example\nwildcard: true\ntype: fqdn\nrpz_serial: 200\nioc_expiration: 5\nsources: internal-list\n", Out).

%% Task 13.2 (R1/R4): end-to-end txt lookup body via format_ioc/4 — the top-level
%% `ioc:'/`tkey:' lines plus a per-match block carrying the additive `sources:'
%% line. The JSON path is unaffected (covered by the json tests above).
format_ioc_txt_test() ->
  Sources = [<<"abuse-ch">>, <<"internal-list">>],
  Zones = #{<<"rpz.example">> =>
              {<<"rpz.example">>, <<"fqdn">>, <<"true">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Results = [{<<"bad.example.com">>, [{<<"rpz.example">>, 100, 0, 3}]}],
  Out = lists:flatten(format_ioc({ok, Results}, Req, txt)),
  ?assertEqual("ioc: bad.example.com\ntkey: tkey\nmatch: bad.example.com\n"
               "feed: rpz.example\nwildcard: true\ntype: fqdn\nrpz_serial: 100\nioc_expiration: 0\nsources: abuse-ch, internal-list\n", Out).

%% Task 13.3 (R1/R4/R8): backward-compatible / old-client JSON shape. Proves the
%% `sources' field is STRICTLY ADDITIVE across all three cases (tracked
%% multi-source, single-source, disabled multi-source): every original key
%% (feed, wildcard, type, rpz_serial, ioc_expiration) is present with the same
%% formatting/types as before the feature, and removing the `sources' key from
%% the produced JSON yields byte-for-byte the pre-feature feed object. The
%% pre-feature object is reconstructed here from the ORIGINAL format string so
%% the test fails if any legacy key's rendering ever drifts. An old client that
%% ignores the extra `sources' key therefore sees an unchanged response (R4).
parse_feeds_json_backward_compat_test() ->
  %% Pre-feature per-feed JSON object format (before `sources' was added).
  OldFmt = "{\"feed\":~p, \"wildcard\":~s, \"type\":~p, \"rpz_serial\": ~p, \"ioc_expiration\": ~p}",
  Strip = fun(Out) ->
    %% Remove the additive `sources' key (value is a JSON array or `null')
    %% exactly as an old client that ignores unknown keys would.
    re:replace(Out, ", \"sources\": (\\[[^\\]]*\\]|null)", "", [{return, list}])
  end,
  %% --- Case 1: tracked multi-source (mask 5 over [a,b,c] -> ["a","c"]) -------
  MultiSources = [<<"a">>, <<"b">>, <<"c">>],
  MultiZones = #{<<"rpz.multi">> =>
                   {<<"rpz.multi">>, <<"fqdn">>, <<"true">>, MultiSources, true}},
  MultiReq = {<<"bad.example.com">>, <<"tkey">>, MultiZones},
  MultiOut = lists:flatten(parse_feeds([{<<"rpz.multi">>, 100, 0, 5}], MultiReq, "", json)),
  OldMulti = "[" ++ lists:flatten(io_lib:format(OldFmt, [<<"rpz.multi">>, "true", "fqdn", 100, 0])) ++ "]",
  %% original keys present with correct values
  ?assertNotEqual(nomatch, string:find(MultiOut, "\"feed\":<<\"rpz.multi\">>")),
  ?assertNotEqual(nomatch, string:find(MultiOut, "\"wildcard\":true")),
  ?assertNotEqual(nomatch, string:find(MultiOut, "\"type\":\"fqdn\"")),
  ?assertNotEqual(nomatch, string:find(MultiOut, "\"rpz_serial\": 100")),
  ?assertNotEqual(nomatch, string:find(MultiOut, "\"ioc_expiration\": 0")),
  %% `sources' is present (additive) ...
  ?assertNotEqual(nomatch, string:find(MultiOut, "\"sources\":")),
  %% ... and stripping it yields exactly the pre-feature object.
  ?assertEqual(OldMulti, Strip(MultiOut)),

  %% --- Case 2: single-source feed (one name regardless of mask) -------------
  SingleZones = #{<<"rpz.single">> =>
                    {<<"rpz.single">>, <<"fqdn">>, <<"false">>, [<<"only">>], true}},
  SingleReq = {<<"bad.example.com">>, <<"tkey">>, SingleZones},
  SingleOut = lists:flatten(parse_feeds([{<<"rpz.single">>, 200, 5, 1}], SingleReq, "", json)),
  OldSingle = "[" ++ lists:flatten(io_lib:format(OldFmt, [<<"rpz.single">>, "false", "fqdn", 200, 5])) ++ "]",
  ?assertNotEqual(nomatch, string:find(SingleOut, "\"sources\": [\"only\"]")),
  ?assertEqual(OldSingle, Strip(SingleOut)),

  %% --- Case 3: disabled multi-source feed (sources: null) -------------------
  DisabledZones = #{<<"rpz.off">> =>
                      {<<"rpz.off">>, <<"ip">>, <<"false">>, [<<"a">>, <<"b">>], false}},
  DisabledReq = {<<"bad.example.com">>, <<"tkey">>, DisabledZones},
  DisabledOut = lists:flatten(parse_feeds([{<<"rpz.off">>, 300, 0, 3}], DisabledReq, "", json)),
  OldDisabled = "[" ++ lists:flatten(io_lib:format(OldFmt, [<<"rpz.off">>, "false", "ip", 300, 0])) ++ "]",
  ?assertNotEqual(nomatch, string:find(DisabledOut, "\"sources\": null")),
  ?assertEqual(OldDisabled, Strip(DisabledOut)).


%% Task 14.2 (R7/R8): resolve_sources/2 over a feed with MORE than 63 sources
%% uses the binary-bitmap mask path. Build 70 sources s0..s69 and set bits 0,
%% 63 and 69 (one strictly above the fixnum threshold) via a bitmap mask; the
%% resolved names MUST be exactly ["s0","s63","s69"] in ascending bit order.
resolve_sources_bitmap_over63_test() ->
  Sources = [ list_to_binary("s" ++ integer_to_list(I)) || I <- lists:seq(0,69) ],
  Mask = ioc2rpz_fun:mask_from_indices([0,63,69]),
  %% >63 sources ⇒ the constructed mask is the bitmap representation
  ?assertMatch({bitmap, _}, Mask),
  ?assertEqual([<<"s0">>, <<"s63">>, <<"s69">>], resolve_sources(Mask, Sources)).

%% Task 14.2 (R7): defensive — bits set at or beyond length(Sources) in a bitmap
%% mask are ignored (stale/invalid mask), so only in-range bits resolve.
resolve_sources_bitmap_out_of_range_test() ->
  Sources = [ list_to_binary("s" ++ integer_to_list(I)) || I <- lists:seq(0,69) ],
  %% bit 69 is the last valid index; bits 70 and 100 have no source and are dropped
  Mask = ioc2rpz_fun:mask_from_indices([0,69,70,100]),
  ?assertMatch({bitmap, _}, Mask),
  ?assertEqual([<<"s0">>, <<"s69">>], resolve_sources(Mask, Sources)),
  %% a bitmap with ONLY out-of-range bits resolves to the empty list
  OOB = ioc2rpz_fun:mask_from_indices([70,80,100]),
  ?assertEqual([], resolve_sources(OOB, Sources)).

%% Task 14.2 (R1/R4/R7): end-to-end parse_feeds/4 JSON for a >63-source zone
%% (Tracked=true) carrying a bitmap mask. The additive `sources' array resolves
%% correctly (bits 0,63,69 -> ["s0","s63","s69"]) and every existing field
%% (feed, wildcard, type, rpz_serial, ioc_expiration) is intact and unchanged.
parse_feeds_json_bitmap_over63_test() ->
  Sources = [ list_to_binary("s" ++ integer_to_list(I)) || I <- lists:seq(0,69) ],
  Mask = ioc2rpz_fun:mask_from_indices([0,63,69]),
  Zones = #{<<"rpz.big">> =>
              {<<"rpz.big">>, <<"fqdn">>, <<"true">>, Sources, true}},
  Req = {<<"bad.example.com">>, <<"tkey">>, Zones},
  Out = lists:flatten(parse_feeds([{<<"rpz.big">>, 100, 0, Mask}], Req, "", json)),
  ?assertEqual("[{\"feed\":<<\"rpz.big\">>, \"wildcard\":true, \"type\":\"fqdn\", \"rpz_serial\": 100, \"ioc_expiration\": 0, \"sources\": [\"s0\",\"s63\",\"s69\"]}]", Out).
