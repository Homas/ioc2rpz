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

%ioc2rpz application
-module(ioc2rpz_app).
-behaviour(application).
-export([start/2, stop/1]).
-include_lib("ioc2rpz.hrl").

start(_StartType, _Start_Args) ->
    IPv4=get_env(ioc2rpz, ipv4, ""),
    IPv6=get_env(ioc2rpz, ipv6, ""),
    Conf_File=get_env(ioc2rpz, conf_file, ?DefConf),
    DB=get_env(ioc2rpz, db_dir, ?DefDB),
    {ok, CWD} = file:get_cwd(),
    Dir=get_env(ioc2rpz, cd, CWD),
    file:set_cwd(Dir),
    io:format("Env ip4: ~p ip6: ~p conf: ~p db: ~p cwd: ~p ~n",[IPv4,IPv6,Conf_File,DB,Dir]),
    ioc2rpz_sup:start_ioc2rpz_sup([IPv4,IPv6,Conf_File,DB]).

stop(_State) ->
    ok.
    

get_env(App, Param, Default) ->
  case application:get_env(App, Param) of
    undefined   ->  Default;
    {ok, []}    ->  Default;
    {ok, X}     ->  X
  end.