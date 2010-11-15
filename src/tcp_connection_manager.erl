%%%-------------------------------------------------------------------
%%% File    : tcp_connection_manager.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%%
%%% Created : 12 Nov 2010 by sinnus <sinnus@linux>
%%%-------------------------------------------------------------------
-module(tcp_connection_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, add_connection/1, remove_connection/1,
	 add_principal_connection/2, remove_principal_connection/2,
	 send_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {connections,
		principal2connections}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_connection(TcpConnectionPid) ->
    gen_server:call(?MODULE, {add_connection, TcpConnectionPid}),
    ok.

remove_connection(TcpConnectionPid) ->
    gen_server:call(?MODULE, {remove_connection, TcpConnectionPid}),
    ok.

add_principal_connection(Principal, TcpConnectionPid) ->
    gen_server:call(?MODULE, {add_principal_connection, Principal, TcpConnectionPid}),
    ok.

remove_principal_connection(Principal, TcpConnectionPid) ->
    gen_server:call(?MODULE, {remove_principal_connection, Principal, TcpConnectionPid}),
    ok.

send_message(Principal, Message) ->
    Connections = gen_server:call(?MODULE, {get_connections, Principal}),
    lists:foreach(fun(Connection) -> tcp_connection:send_message(Connection, Message) end, 
		  Connections),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{connections=[],
		principal2connections = dict:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_connection, TcpConnectionPid}, _From, State) ->
    Connections = State#state.connections,
    NewState = State#state{connections = [TcpConnectionPid|Connections]},
    error_logger:info_msg("add tcp connection. New list ~p~n", [NewState#state.connections]),
    {reply, ok, NewState};

handle_call({remove_connection, TcpConnectionPid}, _From, State) ->
    NewConnections = lists:delete(TcpConnectionPid, State#state.connections),
    NewState = State#state{connections = NewConnections},
    error_logger:info_msg("remove tcp connection. New list ~p~n", [NewConnections]),
    {reply, ok, NewState};

handle_call({add_principal_connection, Principal, TcpConnectionPid}, _From, State) ->
    NewPrincipal2Connections = case dict:find(Principal, State#state.principal2connections) of
				   {ok, Connections} ->
				       dict:append(Principal, TcpConnectionPid, State#state.principal2connections);
				   error ->
				       dict:store(Principal, [TcpConnectionPid], State#state.principal2connections)
			       end,
    error_logger:info_msg("add principal ~p, new list: ~p~n", [Principal, dict:find(Principal, NewPrincipal2Connections)]),
    NewState = State#state{principal2connections = NewPrincipal2Connections},
    {reply, ok, NewState};

handle_call({remove_principal_connection, Principal, TcpConnectionPid}, _From, State) ->
    NewPrincipal2Connections = case dict:find(Principal, State#state.principal2connections) of
				   {ok, Connections} ->
				       NewConnections = lists:delete(TcpConnectionPid, Connections),
				       case NewConnections of
					   [] ->
					       dict:erase(Principal, State#state.principal2connections);
					   _ ->
					       dict:store(Principal, NewConnections, State#state.principal2connections)
				       end;
				   error ->
				       State#state.principal2connections
			       end,

    error_logger:info_msg("remove principal ~p, new list: ~p~n", [Principal, dict:find(Principal, NewPrincipal2Connections)]),
    NewState = State#state{principal2connections = NewPrincipal2Connections},
    {reply, ok, NewState};

handle_call({get_connections, Principal}, _From, State) ->
    ResultConnections = case dict:find(Principal, State#state.principal2connections) of
		      {ok, Connections} ->
			  Connections;
		      error ->
			  []
		  end,
    {reply, ResultConnections, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
