%%%-------------------------------------------------------------------
%%% File    : fsm_sup.erl
%%% Author  :  sinnus
%%% Description : 
%%%
%%% Created : 16 Nov 2010 by  sinnus
%%%-------------------------------------------------------------------
-module(fsm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Listener = {tcp_listener,
		{tcp_listener, start_link,[]},
		permanent,
		brutal_kill,
		worker,
		['tcp_listener']},
    {ok,{{one_for_one,0,1}, [Listener]}}.

%%====================================================================
%% Internal functions
%%====================================================================
