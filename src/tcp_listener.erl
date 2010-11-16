%%%-------------------------------------------------------------------
%%% File    : tcp_listener.erl
%%% Author  :  sinnus
%%% Description : 
%%%
%%% Created : 16 Nov 2010 by  sinnus
%%%-------------------------------------------------------------------
-module(tcp_listener).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start_listener/0,
	 accept/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(TCP_PORT, 9234).
-define(TCP_OPTS, [binary, inet,
                   {active,    false},
                   {backlog,   10},
                   {nodelay,   true},
                   {packet,    raw},
                   {reuseaddr, true}]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    {_Pid, _MonitorRef} =
	spawn_monitor(?MODULE, start_listener, []),
    {ok, #state{}}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
    error_logger:info_msg("listiner failed~n", []),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_listener() ->
    case gen_tcp:listen(?TCP_PORT, ?TCP_OPTS) of
	{ok, LSocket} ->
	    accept(LSocket);
	{error, Reason} ->
	    {error, Reason}
    end.

accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    error_logger:info_msg("accepted connection~n", []),

	    {ok, Pid} = tcp_connection:start(Socket),
	    gen_tcp:controlling_process(Socket, Pid),

	    accept(LSocket);
	{error, Reason} ->
	    error_logger:error_msg("couldn't accept socket. Reason: ~p~n", [Reason]),
	    exit(Reason)
    end.
