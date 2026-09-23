%Copyright 2017-2025 Vadim Pavlov ioc2rpz[at]gmail[.]com
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

%% @doc IOC2RPZ process supervisor.
%%
%% Manages TCP, TLS (DoT), UDP, REST, and DoH listener pools using
%% OTP supervisor behaviour. Each protocol gets its own named supervisor
%% instance with an appropriate restart strategy:
%% <ul>
%%   <li>`simple_one_for_one' for TCP/TLS accept worker pools</li>
%%   <li>`one_for_one' for UDP, REST, and DoH services</li>
%% </ul>
%% @end
-module(ioc2rpz_proc_sup).
-behaviour(supervisor).
-include_lib("kernel/include/file.hrl").
-include_lib("ioc2rpz.hrl").
-export([start_ioc2rpz_proc_sup/1,stop_ioc2rpz_proc_sup/0, start_socket/1, empty_listeners/1]).
-export([init/1]).

start_ioc2rpz_proc_sup([Proc,IP,Proto]) ->
	supervisor:start_link({local, Proc}, ?MODULE, [Proc,IP,Proto]). %list_to_atom(atom_to_list(?MODULE) ++ atom_to_list(Proto))

stop_ioc2rpz_proc_sup() ->
  ioc2rpz_fun:logMessage("ioc2rpz tcp recieved stop message ~n", []),
  ioc2rpz_fun:logMessage("ioc2rpz tcp is terminating ~n", []),
  gen_server:stop(?MODULE).

%% @doc Initializes the supervisor for the given protocol type.
%%
%% Dispatches on `Proc' to configure the appropriate listener:
%% <ul>
%%   <li>`tcp_sup | tcp6_sup' — Opens a TCP listen socket, spawns an
%%       initial pool of accept workers via {@link empty_listeners/1},
%%       and returns a `simple_one_for_one' child spec for
%%       {@link ioc2rpz} gen_server workers.</li>
%%   <li>`udp_sup | udp6_sup' — Returns a `one_for_one' child spec
%%       that starts a single {@link ioc2rpz_udp} worker bound to
%%       `IPStr' and `Proto'.</li>
%%   <li>`tls_sup | tls6_sup' — Opens a TLS listen socket (with
%%       certificate/key from `cfg_table'), spawns an initial pool of
%%       accept workers, and returns a `simple_one_for_one' child spec
%%       for {@link ioc2rpz} gen_server workers.</li>
%%   <li>`rest_tls_sup | rest_tls6_sup' — Starts a Cowboy TLS listener
%%       for the REST management API on `?PortREST'.</li>
%%   <li>`doh_sup | doh6_sup' — Starts a Cowboy TLS listener for
%%       DNS-over-HTTPS on `?PortDoH'.</li>
%% </ul>
%%
%% @param Args A list `[Proc, IPStr, Proto]' where `Proc' is the
%%        supervisor name atom, `IPStr' is the bind address string
%%        (or `""' for INADDR_ANY), and `Proto' is `inet | inet6'.
%% @returns `{ok, {SupFlags, ChildSpecs}}'
%% @end
init([Proc,IPStr,Proto]) when Proc == tcp_sup; Proc == tcp6_sup -> %DNS TCP
  Pid=self(),
  {ok, TCPSocket} = open_tcp_sockets(IPStr, Proto) ,
	spawn_opt(ioc2rpz_proc_sup,empty_listeners,[Proc],[link,{fullsweep_after,0}]),
  ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [Proc]),
  {ok, {{simple_one_for_one, 1000, 60}, [{ioc2rpz, {ioc2rpz, start_ioc2rpz, [TCPSocket, [Pid,Proc,no]]}, transient, 1000, worker, [ioc2rpz]}]}};


init([Proc,IPStr,Proto]) when Proc == udp_sup; Proc == udp6_sup -> %DNS UDP
  ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [udp_sup]),
  {ok, {{one_for_one, 60, 3600}, [{ioc2rpz, {ioc2rpz_udp, start_ioc2rpz_udp, [IPStr, [Proto]]}, temporary, 1000, worker, [ioc2rpz_udp]}]}};


init([Proc,IPStr,Proto]) when Proc == tls_sup; Proc == tls6_sup -> %DoT
  Pid=self(),
  case open_tls_sockets(IPStr, Proto) of
    {ok, TLSSocket} ->
      spawn_opt(ioc2rpz_proc_sup,empty_listeners,[Proc],[link,{fullsweep_after,0}]),
      ioc2rpz_fun:logMessage("ioc2rpz ~p started ~n", [Proc]),
      {ok, {{simple_one_for_one, 1000, 60}, [{ioc2rpz, {ioc2rpz, start_ioc2rpz, [TLSSocket, [Pid,Proc,yes]]}, transient, 1000, worker, [ioc2rpz]}]}};
    {error, Reason} ->
      %No usable certificate, or the port could not be bound: come up with no
      %children rather than crashing the whole supervision tree. DNS over
      %TCP/UDP keeps serving.
      ioc2rpz_fun:logMessage("ioc2rpz ~p was NOT started: ~p ~n", [Proc, Reason]),
      {ok, {{one_for_one, 10, 10}, []}}
  end;


init([Proc,_IPStr,_Proto]) when Proc == rest_tls_sup; Proc == rest_tls6_sup -> %REST
	Dispatch = cowboy_router:compile([{'_', [
				{"/", ioc2rpz_rest, [root]},
				{"/api/[:api_ver]/stats/serv", ioc2rpz_rest, [stats_serv]},
				{"/api/[:api_ver]/stats/rpz", ioc2rpz_rest, [stats_rpz]},
				{"/api/[:api_ver]/stats/source", ioc2rpz_rest, [stats_source]},
				{"/api/[:api_ver]/update/all_rpz", ioc2rpz_rest, [update_all_rpz]},
				{"/api/[:api_ver]/update/:rpz", ioc2rpz_rest, [update_rpz]},
				{"/api/[:api_ver]/mgmt/reload_cfg", ioc2rpz_rest, [reload_cfg]},
				{"/api/[:api_ver]/mgmt/update_tkeys", ioc2rpz_rest, [update_tkeys]},
				{"/api/[:api_ver]/cache/sources/clear/all", ioc2rpz_rest, [cache_sources_clear_all]},
				{"/api/[:api_ver]/cache/sources/clear/:source", ioc2rpz_rest, [cache_sources_clear_one]},
				{"/api/[:api_ver]/cache/sources/load/all", ioc2rpz_rest, [cache_sources_load_all]},
				%%%refresh sources (clear cache and load sources)
				%%%refresh all RPZ
				{"/api/[:api_ver]/mgmt/terminate", ioc2rpz_rest, [terminate]},
				{"/api/[:api_ver]/feed/:rpz", ioc2rpz_rest, [get_rpz]}, %pull feed
				{"/api/[:api_ver]/ioc/:ioc", ioc2rpz_rest, [get_ioc]}, %check ioc
				{'_', ioc2rpz_rest, [catch_all]}
					]}]),
  {ok, {{one_for_one, 10, 10}, cowboy_tls_childspecs(Proc, https, ?PortREST, Dispatch)}};

init([Proc,_IPStr,_Proto]) when Proc == doh_sup; Proc == doh6_sup -> %DoH
	Dispatch = cowboy_router:compile([{'_', [
				{"/", ioc2rpz_doh, [root]},
				{"/dns-query", ioc2rpz_doh, [dns_query]},
				{'_', ioc2rpz_doh, [catch_all]}
					]}]),
  {ok, {{one_for_one, 10, 10}, cowboy_tls_childspecs(Proc, doh, ?PortDoH, Dispatch)}}.

%% @doc Builds the supervised child spec for a Cowboy TLS listener.
%%
%% The REST and DoH listeners used to be started with `cowboy:start_tls/3'
%% called for its side effect inside {@link init/1}, and the child list returned
%% to the supervisor was `[]'. That put the listener under Cowboy's own
%% `ranch_sup' and left this supervisor with nothing to supervise, so ioc2rpz
%% never learned about (nor recovered from) a listener that gave up.
%%
%% `ranch:child_spec/5' returns the same listener as an ordinary child spec, so
%% it is started, restarted and shut down as part of the ioc2rpz supervision
%% tree. `cowboy_tls' is the Ranch protocol module `cowboy:start_tls/3' uses
%% internally, and the `env => #{dispatch => ...}' protocol options are the same
%% ones it would have passed, so the running listener is unchanged.
%%
%% Both `{versions, ...}' and `{ciphers, ...}' are derived from `?TLSVersion'
%% (task 17): passing the versions explicitly makes the configured protocol
%% version authoritative during negotiation instead of relying on there being no
%% cipher suite in common for the versions we did not want.
%%
%% Returns `[]' (no children, listener not started) when no certificate is
%% configured, instead of failing the `[[Cert]] = ets:match(...)' match and
%% taking the supervisor down with it.
%% @private
cowboy_tls_childspecs(Proc, Ref, Port, Dispatch) ->
  case ioc2rpz_fun:srv_cert() of
    {ok, Cert} ->
      Ciphers=ioc2rpz_fun:get_cipher_suites(?TLSVersion),
      Versions=ioc2rpz_fun:get_tls_versions(?TLSVersion),
      %The two settings cowboy:start_tls/3 adds on top of ranch:start_listener/5
      %are reproduced here so the listener behaves identically: the ALPN
      %preference list (without it a client negotiating h2 - common for DoH -
      %would not get HTTP/2) and connection_type => supervisor in BOTH the
      %transport and the protocol options.
      TransOpts = #{connection_type => supervisor,
                    socket_opts => [{alpn_preferred_protocols, [<<"h2">>, <<"http/1.1">>]},
                                    {port, Port},
                                    {certfile, Cert#cert.certfile},
                                    {keyfile, Cert#cert.keyfile},
                                    {ciphers, Ciphers},
                                    {versions, Versions}]},
                                    %{cacertfile, Cert#cert.cacertfile},
      ProtoOpts = #{connection_type => supervisor, env => #{dispatch => Dispatch}},
      ioc2rpz_fun:logMessage("ioc2rpz ~p started on port ~p, TLS versions ~p ~n", [Proc, Port, Versions]),
      [ranch:child_spec(Ref, ranch_ssl, TransOpts, cowboy_tls, ProtoOpts)];
    {error, Reason} ->
      ioc2rpz_fun:logMessage("ioc2rpz ~p not started: no TLS certificate configured (~p) ~n", [Proc, Reason]),
      []
  end.

%% @doc Opens a TCP listen socket on `?Port'.
%%
%% When `IPStr' is a non-empty string, the socket is bound to that
%% specific IP address. Otherwise it listens on all interfaces.
%%
%% @param IPStr Bind address as a string, or `""'/`[]' for INADDR_ANY.
%% @param Proto `inet' or `inet6'.
%% @returns `{ok, TCPSocket}'
%% @end
open_tcp_sockets(IPStr,Proto) when IPStr /= "", IPStr /= [] ->
  {ok,IP}=inet:parse_address(IPStr),
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{ip, IP},{reuseaddr, true}, binary, Proto]),
  {ok, TCPSocket};

open_tcp_sockets(_IPStr,Proto) ->
  {ok, TCPSocket} = gen_tcp:listen(?Port, [{reuseaddr, true}, binary, Proto]),  %{ipv6_v6only,true}
  {ok, TCPSocket}.


%% @doc Opens a TLS listen socket on `?PortTLS'.
%%
%% Reads the server certificate and key from `cfg_table' and
%% configures cipher suites based on `?TLSVersion'. When `IPStr' is
%% a non-empty string, the socket is bound to that specific IP
%% address. Otherwise it listens on all interfaces.
%%
%% @param IPStr Bind address as a string, or `""'/`[]' for INADDR_ANY.
%% @param Proto `inet' or `inet6'.
%% @returns `{ok, TLSSocket}'
%% @end
open_tls_sockets(IPStr,Proto) when IPStr /= "", IPStr /= [] ->
  {ok,IP}=inet:parse_address(IPStr),
	case tls_listen_opts() of
		{ok, TLSOpts} ->
			ssl:listen(?PortTLS, [{ip, IP} | TLSOpts ++ [Proto]]);
		{error, Reason} ->
			{error, Reason}
	end;

open_tls_sockets(_IPStr,Proto) ->
	case tls_listen_opts() of
		{ok, TLSOpts} ->
			ssl:listen(?PortTLS, TLSOpts ++ [Proto]);
		{error, Reason} ->
			{error, Reason}
	end.

%% @doc Builds the common `ssl:listen/2' options for the DoT listener.
%%
%% Reads the certificate via {@link ioc2rpz_fun:srv_cert/0} instead of a hard
%% `[[Cert]] = ets:match(...)' match, so a missing `srv' row is reported rather
%% than crashing the supervisor.
%%
%% `{versions, ...}' is passed alongside `{ciphers, ...}' (task 17): `?TLSVersion'
%% previously reached only the cipher-suite selection, leaving the accepted
%% protocol versions at the ssl application defaults.
%% @private
tls_listen_opts() ->
	case ioc2rpz_fun:srv_cert() of
		{ok, Cert} ->
			Ciphers=ioc2rpz_fun:get_cipher_suites(?TLSVersion),
			Versions=ioc2rpz_fun:get_tls_versions(?TLSVersion),
			{ok, [{active,once},{reuseaddr, true},{send_timeout, 5000},{send_timeout_close, true}, binary,
			      {certfile, Cert#cert.certfile}, {keyfile, Cert#cert.keyfile},
			      {ciphers, Ciphers}, {versions, Versions}]}; %,{cacertfile, Cert#cert.cacertfile}
		{error, Reason} ->
			ioc2rpz_fun:logMessage("Cannot open the DoT listener: no TLS certificate configured (~p) ~n", [Reason]),
			{error, Reason}
	end.

%% @doc Asks the supervisor `Proc' to start a new child worker.
%%
%% For `simple_one_for_one' supervisors (TCP/TLS pools) this spawns
%% a new accept worker using the child spec defined in {@link init/1}.
%%
%% @param Proc The registered name of the supervisor.
%% @returns Result of `supervisor:start_child/2'.
%% @end
start_socket(Proc) ->
  supervisor:start_child(Proc, []).

%% @doc Pre-spawns the initial pool of 5 accept workers.
%%
%% Called via `spawn_opt/4' (linked to the supervisor) during
%% {@link init/1} for TCP and TLS pools. Each worker immediately
%% enters the accept loop waiting for incoming connections.
%%
%% @param Proc The registered name of the supervisor to add workers to.
%% @returns `ok'
%% @end
empty_listeners(Proc) ->
  [start_socket(Proc) || _ <- lists:seq(1,5)],
  ok.
