%%%-------------------------------------------------------------------
%%% File    : tcp_server.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%%
%%% Created : 11 Nov 2010 by sinnus <sinnus@linux>
%%%-------------------------------------------------------------------
-module(tcp_server).

-export([start/0, init/1]).

-define(TCP_PORT, 9234).
-define(TCP_OPTS, [binary, inet,
                   {active,    false},
                   {backlog,   10},
                   {nodelay,   true},
                   {packet,    raw},
                   {reuseaddr, true}]).

start() ->
    tcp_connection_manager:start_link(),
    case gen_tcp:listen(?TCP_PORT, ?TCP_OPTS) of
	{ok, LSocket} ->
	    Pid = spawn(?MODULE, init, [LSocket]),
	    ok = gen_tcp:controlling_process(LSocket, self()),
	    {ok, Pid};
	{error, Reason} -> {error, Reason}
    end.

init(LSocket) ->
    error_logger:info_msg("tcp server listener started. Port: ~p~n", [?TCP_PORT]),
    loop(LSocket).

loop(LSocket) ->
    case gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    error_logger:info_msg("accepted connection~n", []),
	    {ok, _Pid} = tcp_connection:start(Socket),
	    loop(LSocket);
	{error, Reason} ->
	    error_logger:error_msg("couldn't accept socket. Reason: ~p~n", [Reason]),
	    exit(Reason)
    end.
