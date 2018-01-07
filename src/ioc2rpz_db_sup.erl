%Copyright 2017-2018 Vadim Pavlov pvm(dot)del[at]gmail[.]com
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

%IOC2RPZ DB Supervisor

-module(ioc2rpz_db_sup).
-behaviour(gen_server).

-export([start_db/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_db() ->
  gen_server:start_link(?MODULE, [], []).

%% supervisor.
init(_Init) ->
	{ok, []}.
	
handle_info({'ETS-TRANSFER',Tab,_FromPid,_GiftData}, _State) ->
  ioc2rpz_fun:logMessage("DB_sup got ~p table ownership ~n", [Tab]),
  {noreply, ok};

handle_info(Info, State) ->
  ioc2rpz_fun:logMessage("DB_sup got an unmached handle_info request:~p ~n", [Info]),
  {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_E, _From, State) ->
  {noreply, State}.

terminate(_Reason, _Tab) ->
%  ioc2rpz_db:tab2file([]),
  ok.

code_change(_OldVersion, Tab, _Extra) ->
  {ok, Tab}.