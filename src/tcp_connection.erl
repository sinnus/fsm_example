%%%-------------------------------------------------------------------
%%% File    : tcp_connection.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%%
%%% Created : 11 Nov 2010 by sinnus <sinnus@linux>
%%%-------------------------------------------------------------------
-module(tcp_connection).

-behaviour(gen_server).

%% API
-export([start_link/1, start/1, reader_start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket}).
-record(reader_state, {principal}).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

start(Socket) ->
    gen_server:start(?MODULE, [Socket], []).

reader_start(Socket, Pid) ->
    tcp_connection_manager:add_connection(Pid),
    ReaderState = #reader_state{principal = none},
    reader_loop(Socket, Pid, ReaderState).

reader_loop(Socket, Pid, ReaderState) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    error_logger:info_msg("Got Data: ~p", [Data]),
	    Principal = ReaderState#reader_state.principal,
	    case Principal of
		none ->
		    check_principal(Socket, Pid, ReaderState, Data);
		_ ->
		    handle_data(Data, Socket, Pid, ReaderState)
	    end;
	{error, closed} ->
	    close_socket(Pid)
    end.

%%% --------------------------------------------------------------------
handle_data(<<"quit\r\n">>, _Socket, Pid, _ReaderState) ->
    close_socket(Pid);

handle_data(_Data, Socket, Pid, ReaderState) ->
    reader_loop(Socket, Pid, ReaderState).

%%% --------------------------------------------------------------------


close_socket(Pid) ->
    unlink(Pid),
    gen_server:cast(Pid, socket_closed).

send_message(Pid, Message) ->
    gen_server:cast(Pid, {send_message, Message}).

check_principal(Socket, Pid, ReaderState, Data) ->
    case Data of
	<<"user1\r\n">> ->
	    reader_loop(Socket, Pid, ReaderState#reader_state{principal = Data});
	_ ->
	    send_message(Pid, "Wrong principal\r\n"),
	    close_socket(Pid)
    end.

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
init([Socket]) ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, reader_start, [Socket, self()]),
    {ok, #state{socket = Socket}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
    case Request of
	socket_closed ->
	    error_logger:info_msg("socket closed~n", []),
	    {stop,normal,State};
	{send_message, Message} ->
	    Socket = State#state.socket,
	    gen_tcp:send(Socket, Message),
	    {noreply,State};
	Other ->
	    error_logger:info_msg("unknown cast, request=~w", [Other]),
	    {noreply,State}
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', _From, Reason}, State) ->    
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    Socket = State#state.socket,
    inet:close(Socket),
    tcp_connection_manager:remove_connection(self()),
    error_logger:info_msg("terminating, pid=~w, reason=~w", [self(), Reason]),
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
