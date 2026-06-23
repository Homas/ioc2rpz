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

%% @doc IOC2RPZ Connectors.
%%
%% This module handles downloading, parsing, and cleaning IOC (Indicator of
%% Compromise) data from various source types: local files, shell commands,
%% and HTTP/HTTPS/FTP URLs. Downloaded IOC feeds are cleaned using configurable
%% regex patterns and returned as lists of `{IOC, Expiration, Type}' tuples
%% suitable for RPZ zone construction.
%% @end

-module(ioc2rpz_conn).
-include_lib("ioc2rpz.hrl").
-export([get_ioc/3,clean_feed_bin/2,clean_feed/3]).

-define(IP_REGEX, re:compile("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}(\\/[0-9]{1,3})?)$|(:)")). %regex to check ip/fqdn
-define(DEFAULT_REGEX,re:compile("^([A-Za-z0-9][A-Za-z0-9\-\._]+)[^A-Za-z0-9\-\._]*.*$",[{newline, any}])). %default clean up regex
-define(NELINE_REGEX,re:compile(REX,[{newline, any}])).

%% @doc Fetches IOC data from a source URL, cleans it, and returns parsed indicators.
%%
%% Downloads the raw feed via {@link get_ioc/2}, then splits the content by
%% newline delimiters and applies regex-based cleaning via {@link p_clean_feed/4}.
%% Logs the source size, MD5 hash, indicator count, and processing time.
%%
%% @param URL The source URL binary (e.g., `<<"http://...">>', `<<"file:...">>', `<<"shell:...">>').
%% @param REGEX The regex pattern for IOC extraction: `none', `[]' (default), or a custom regex string.
%% @param Source The `#source{}' record with name, max_ioc, and ioc_type fields.
%% @returns A list of `{IOC, Expiration, Type}' tuples, or `[]' on download failure.
get_ioc(URL,REGEX,Source) ->
  case get_ioc(URL,?Src_Retry) of
    {ok, Bin} ->
      ioc2rpz_fun:logMessage("Source: ~p, REX ~p, max IoCs ~p, type ~p ~n",[Source#source.name, REGEX,Source#source.max_ioc,Source#source.ioc_type]),
      ioc2rpz_fun:logMessage("Source: ~p, size: ~s (~p), MD5: ~p ~n",[Source#source.name, ioc2rpz_fun:conv_to_Mb(byte_size(Bin)),byte_size(Bin), ioc2rpz_fun:bin_to_hexstr(crypto:hash(md5,Bin))]), %TODO debug

      %TODO spawn cleanup
      CTime=ioc2rpz_fun:curr_serial_60(),
      %L=[ {ioc2rpz_fun:bin_to_lowcase(X),Y} || {X,Y} <- clean_feed(ioc2rpz_fun:split_tail(Bin,<<"\n">>),REGEX) ],

      L=p_clean_feed(ioc2rpz_fun:split_tail(Bin,[<<"\r\n">>,<<"\n">>,<<"\r">>]),REGEX,Source#source.max_ioc,Source#source.ioc_type),

      ioc2rpz_fun:logMessage("Source: ~p, got ~p indicators, clean time ~p ~n",[Source#source.name, length(L), (ioc2rpz_fun:curr_serial_60()-CTime)]), %TODO debug
      L;
    _ ->
      []
  end.

%% @doc Receives cleaned IOC data from a spawned worker process.
%% @param PID The PID of the worker process to receive from.
%% @returns A list of `{LowercaseIOC, Expiration, Type}' tuples.
w_clean_feed(PID) ->
  receive
    { ok, PID, IOC } -> [ {ioc2rpz_fun:bin_to_lowcase(X),Y,Z} || {X,Y,Z} <- IOC ]
  end.

%% @doc Parallelized IOC feed cleaning with optional max indicator limit.
%%
%% Splits the IOC list into chunks of `?IOCperProc' and spawns worker
%% processes to clean each chunk concurrently via {@link clean_feed/3}.
%% When `Max' is undefined or 0, all indicators are returned. Otherwise
%% the result is truncated to `Max' entries.
%%
%% @param IOC List of raw IOC binaries (one per line).
%% @param REGEX The regex pattern for extraction.
%% @param Max Maximum number of indicators to return, or `undefined'/`0' for unlimited.
%% @param IoCType The IOC type string (e.g., `"fqdn"', `"ip"', `"mixed"').
%% @returns A list of `{IOC, Expiration, Type}' tuples.
p_clean_feed(IOC,REGEX,Max,IoCType) when Max == undefined; Max == 0 ->
  p_clean_feed(IOC,REGEX,Max,0,IoCType);

p_clean_feed(IOC,REGEX,Max,IoCType) when Max /= undefined ->
  lists:sublist(p_clean_feed(IOC,REGEX,Max,0,IoCType),Max).

%% @doc Internal recursive worker for {@link p_clean_feed/4}.
%% Splits IOC list, spawns a cleaning process for the first chunk, recurses
%% on the remainder, then collects and concatenates results.
%% @param IOC Remaining IOC binaries to process.
%% @param REGEX The regex pattern.
%% @param Max Maximum indicator limit.
%% @param Count Running count of indicators processed so far.
%% @param IoCType The IOC type string.
%% @returns A list of `{IOC, Expiration, Type}' tuples.
p_clean_feed(IOC,REGEX,Max,Count,IoCType)  ->
  ParentPID = self(),
  [IOC1,IOC2]=ioc2rpz_fun:split(IOC,?IOCperProc),
  PID=spawn_opt(fun() ->
      ParentPID ! {ok, self(), ioc2rpz_conn:clean_feed(IOC1,REGEX,IoCType)  }
      end
      ,[{fullsweep_after,0}]),
  L = if IOC2 /= [] , Count+?IOCperProc < Max ; IOC2 /= [],Max == undefined; IOC2 /= [],Max == 0 ->
    p_clean_feed(IOC2,REGEX,Max,Count+?IOCperProc,IoCType);
    true -> []
  end,
  w_clean_feed(PID) ++ L .



%% @doc Reads IOC data from a local file.
%%
%% Validates the path first (task 12): filenames containing `..' parent-directory
%% segments are rejected to prevent path traversal. Reads the entire file into
%% memory. Retries up to `Retry' times with `?Src_Retry_TimeOut' second delays
%% on failure.
%%
%% @param URL Binary of the form `<<"file:Path">>'.
%% @param Retry Number of remaining retry attempts.
%% @returns `{ok, Binary}' on success, `{error, invalid_file_path}' if the path
%%          is rejected, or `{error, Reason}' after all retries exhausted.
get_ioc(<<"file:",Filename/binary>> = URL, Retry) ->
  case valid_file_path(Filename) of
    false ->
      ioc2rpz_fun:logMessage("Rejected file source ~p: path contains '..' (directory traversal)~n",[URL]),
      {error, invalid_file_path};
    true ->
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
      end
  end;

%% @doc Executes a local shell command and returns its output as IOC data.
%%
%% The command is first validated by {@link ioc2rpz_fun:validate_shell_cmd/1}
%% (task 11 security hardening): each pipeline segment's executable must be an
%% absolute path or an allowlisted safe text utility, destructive commands and
%% shell interpreters are blocked, and command substitution / output redirection
%% are rejected. On success the full command is logged via CEF 150 and passed to
%% `os:cmd/1'; on rejection a CEF 151 security warning is logged and
%% `{error, shell_cmd_rejected}' is returned without executing anything.
%% The output is converted to a UTF-8 binary. No retries are attempted.
%%
%% @param URL Binary of the form `<<"shell:Command">>'.
%% @param Retry Unused (shell commands are not retried).
%% @returns `{ok, Binary}' containing the command output, or
%%          `{error, shell_cmd_rejected}' if the command failed validation.
get_ioc(<<"shell:",CMD/binary>> = _URL, _Retry) ->
  case ioc2rpz_fun:validate_shell_cmd(CMD) of
    {ok, _} ->
      ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(150),[CMD]),
      {ok, unicode:characters_to_binary(os:cmd(binary_to_list(CMD)))}; %fix for https://github.com/Homas/ioc2rpz/issues/47
    {error, Reason} ->
      ioc2rpz_fun:logMessageCEF(ioc2rpz_fun:msg_CEF(151),[CMD, Reason]),
      {error, shell_cmd_rejected}
  end;

%% @doc Downloads IOC data from an HTTP, HTTPS, or FTP URL.
%%
%% Uses `httpc:request/4' with a Mozilla User-Agent header, cookies enabled,
%% and a configurable timeout (`?SourcePullTimeout'). For `https://' URLs the
%% remote server's TLS certificate is verified (task 17): `verify_peer' against
%% the system CA store (`public_key:cacerts_get/0') plus HTTPS hostname
%% verification, so MITM/self-signed certificates are rejected. On HTTP 200,
%% returns the response body. On non-200 status codes, logs a warning and
%% returns an empty binary. Retries up to `Retry' times with
%% `?Src_Retry_TimeOut' second delays on connection errors.
%%
%% @param URL Binary URL starting with `<<"http:">>', `<<"https">>', or `<<"ftp:/">>'.
%% @param Retry Number of remaining retry attempts.
%% @returns `{ok, Binary}' on success (including empty binary for non-200),
%%          or `{error, Reason}' after all retries exhausted.
get_ioc(<<Proto:5/bytes,_/binary>> = URL, Retry) when Proto == <<"http:">>;Proto == <<"https">>;Proto == <<"ftp:/">> ->
	httpc:set_options([{cookies,enabled}]),
	HTTPOptions = case Proto of
		<<"https">> ->
			[{timeout, ?SourcePullTimeout},
			 {ssl, [{verify, verify_peer},
			        {cacerts, public_key:cacerts_get()},
			        {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]}];
		_ ->
			[{timeout, ?SourcePullTimeout}]
	end,
  case httpc:request(get,{binary_to_list(URL),[{"User-Agent", "Mozilla"}]},HTTPOptions,[{body_format,binary},{sync,true}]) of %,{socket_opts,[{cookies,enabled}]}
  {ok,{{_,200,_},_,Response}} ->
    {ok,Response};
  {ok,{{_,Code,_},Headers,_Response}} ->
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

%% @doc Returns `true' if a `file:' source path is safe to read, `false' if it
%% contains a `..' parent-directory segment (path traversal). Task 12 guard.
%% A `..' that is merely part of a filename (e.g. `foo..bar') is allowed; only
%% a `..' that is a full path segment is rejected.
valid_file_path(Filename) ->
  Parts = binary:split(Filename, <<"/">>, [global]),
  not lists:member(<<"..">>, Parts).

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

%% @doc Cleans an IOC feed with no regex transformation.
%%
%% When REGEX is `none', returns each non-empty line as-is with expiration 0.
%% For `"mixed"' IOC type, auto-detects whether each entry is an IP or FQDN.
%%
%% @param IOC List of raw IOC binaries.
%% @param none Atom indicating no regex cleaning.
%% @param IoCType `"mixed"' for auto-detection, or a fixed type string.
%% @returns A list of `{IOC, 0, Type}' tuples.
clean_feed(IOC,none, "mixed") ->
  {ok,IPREX} = ?IP_REGEX,
  [ {X,0,check_if_ip(X, "mixed", IPREX)} || X <- IOC, X /= <<>>];

%% @doc Cleans an IOC feed with no regex (non-mixed type).
%% @see clean_feed/3
clean_feed(IOC,none,IoCType) ->
  [ {X,0,IoCType} || X <- IOC, X /= <<>>];

%% @doc Cleans an IOC feed using the default regex.
%%
%% Applies `?DEFAULT_REGEX' to extract the first capture group from each line,
%% filtering out empty results. Auto-detects IP vs FQDN for mixed types.
%%
%% @param IOC List of raw IOC binaries.
%% @param REGEX Empty list `[]' indicating default regex.
%% @param IoCType The IOC type string.
%% @returns A list of `{IOC, Expiration, Type}' tuples.
clean_feed(IOC,[],IoCType) ->
  %TODO update regex
  {ok,MP} = ?DEFAULT_REGEX,
  %do not allow "." and better hostname handeling
  % to validate regex below 2024-08-18
%  {ok,MP} = re:compile("^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$",[{newline, any}]),
  {ok,IPREX} = ?IP_REGEX,
  [ X || X <- clean_feed(IOC,[],MP, IoCType, IPREX), X /= <<>>];


%% @doc Cleans an IOC feed using a user-defined regex.
%%
%% Compiles the user's regex string and applies it to extract IOCs with
%% optional expiration dates. The regex should have two capture groups:
%% the first for the IOC value, the second for the expiration timestamp.
%%
%% @param IOC List of raw IOC binaries.
%% @param REX User-defined regex string.
%% @param IoCType The IOC type string.
%% @returns A list of `{IOC, Expiration, Type}' tuples.
clean_feed(IOC,REX,IoCType) -> %REX - user's regular expression
  {ok,MP} = ?NELINE_REGEX,
  {ok,IPREX} = ?IP_REGEX,
  [ X || X <- clean_feed(IOC,[],MP,IoCType, IPREX), X /= <<>>].

%% @doc Recursive IOC cleaning worker. Applies the compiled regex to each line,
%% extracting the IOC and optional expiration date from capture groups.
%% Logs a warning for lines that don't match the regex pattern.
%% @param Head The current IOC line binary.
%% @param Tail Remaining IOC lines.
%% @param CleanIOC Accumulator of cleaned IOC tuples.
%% @param REX Compiled regex reference.
%% @param IoCType The IOC type string.
%% @param IPREX Compiled IP detection regex.
%% @returns A list of `{IOC, Expiration, Type}' tuples.
clean_feed([Head|Tail],CleanIOC,REX,IoCType, IPREX) ->
  IOC2 = case re:run(Head,REX,[global,notempty,{capture,[1,2],binary}]) of
    {match,[[IOC,<<>>]]} -> {IOC,0, check_if_ip(IOC, IoCType, IPREX)};
    {match,[[IOC,EXP]]} -> {IOC, conv_t2i(EXP), check_if_ip(IOC, IoCType, IPREX)};
    _Else -> ioc2rpz_fun:logMessage("Bad IOC: ~p, Type ~p ~n",[Head,IoCType]), <<>>
  end,
  clean_feed(Tail, [IOC2 | CleanIOC], REX, IoCType, IPREX);


%% @doc Base case for recursive IOC cleaning. Returns the accumulated list.
clean_feed([],CleanIOC,_REX,_IoCType, _IPREX) ->
  CleanIOC.

%% @doc Determines whether an IOC is an IP address or FQDN.
%%
%% For `"mixed"' type sources, matches the IOC against the IP regex.
%% For non-mixed types, returns the type as-is.
%%
%% @param IOC The IOC binary to check.
%% @param IoCType `"mixed"' for auto-detection, or a fixed type.
%% @param IPREX Compiled IP detection regex.
%% @returns `"ip"' or `"fqdn"' for mixed type; the original IoCType otherwise.
check_if_ip(IOC, "mixed", IPREX) ->
  case re:run(IOC,IPREX,[global,notempty,{capture,[1],binary}]) of
   {match,_} ->"ip";
   _ -> "fqdn"
  end;

%% @doc Passthrough for non-mixed IOC types.
%% @see check_if_ip/3
check_if_ip(_IOC, IoCType, _IPREX) ->
  IoCType.

%% @doc Alternative binary-accumulator IOC feed cleaner (experimental).
%%
%% Similar to {@link clean_feed/3} but accumulates results as a single
%% binary with semicolon delimiters before splitting into tuples at the end.
%% Used for memory consumption testing.
%%
%% @param IOC List of raw IOC binaries.
%% @param REGEX `none', `[]' (default), or a custom regex string.
%% @returns A list of `{IOC, Expiration}' tuples (without type field).
clean_feed_bin(IOC,none) ->
  [ {X,0} || X <- IOC, X /= <<>>];

clean_feed_bin(IOC,[]) ->
  {ok,MP} = ?DEFAULT_REGEX,
  [ X || X <- clean_feed_bin(IOC,<<>>,MP), X /= <<>>];

clean_feed_bin(IOC,REX) -> %REX - user's regular expression
  {ok,MP} = ?NELINE_REGEX,
  [ X || X <- clean_feed_bin(IOC,<<>>,MP), X /= <<>>].

%% @doc Recursive binary-accumulator IOC cleaner.
%% @see clean_feed_bin/2
clean_feed_bin([Head|Tail],CleanIOC,REX) ->
  IOC2 = case re:run(Head,REX,[global,notempty,{capture,[1,2],binary}]) of
    {match,[[IOC,<<>>]]} -> <<(ioc2rpz_fun:bin_to_lowcase(IOC))/binary,",",0,";">>;
    {match,[[IOC,EXP]]} -> <<(ioc2rpz_fun:bin_to_lowcase(IOC))/binary,",",(conv_t2i(EXP))/binary,";">>;
    _Else -> <<>>
  end,
  clean_feed_bin(Tail, <<CleanIOC/binary,IOC2/binary>>, REX);
%% @doc Base case: splits the accumulated binary by semicolons and commas
%% into `{IOC, Expiration}' tuples.
clean_feed_bin([],CleanIOC,_REX) ->
  [ {A,B} || [A,B] <- [ ioc2rpz_fun:split_tail(X,<<",">>) || X <- ioc2rpz_fun:split_tail(CleanIOC,<<";">>), X /= <<>> ]].
%%%Check memory consumtion


%% @doc Converts an ISO 8601 datetime binary to a Unix timestamp (integer).
%%
%% Accepts format `<<"YYYY-MM-DDThh:mm:ss">>' (with `T', `t', or space separator).
%% Returns 0 for unrecognized formats.
%%
%% @param Binary An ISO 8601 datetime binary.
%% @returns Unix timestamp as integer, or `0' if parsing fails.
conv_t2i(<<Y:4/bytes,"-",M:2/bytes,"-",D:2/bytes,Sep:1/bytes,HH:2/bytes,":",MM:2/bytes,":",SS:2/bytes,_Rest/binary>>) when Sep==<<"T">>;Sep==<<"t">>;Sep==<<" ">>->
  calendar:datetime_to_gregorian_seconds({{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}, {binary_to_integer(HH), binary_to_integer(MM), binary_to_integer(SS)}})-62167219200;
conv_t2i(_EXP) ->
  0.
