%%%-------------------------------------------------------------------
%%% File    : game_fsm.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : The sea battle game
%%%
%%% Created :  7 Nov 2010 by sinnus <sinnus@linux>
%%%-------------------------------------------------------------------
-module(game_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).
-define(MAX_TURNS, 3).
%% gen_fsm callbacks
-export([init/1,
	 join/2, turn/2,
	 started/3, wait_turn/3,
	 handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {player1,
		player2,
		current_player_no,
		turn_no,
		event_manager_pid}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Player, EventManagerPid) ->
    gen_fsm:start_link(?MODULE, [Player, EventManagerPid], []).

join(Pid, Player) ->
    gen_fsm:sync_send_event(Pid, {join, Player}).

turn(Pid, Player) ->
    gen_fsm:sync_send_event(Pid, {turn, Player}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([Player, EventManagerPid]) ->
    gen_event:notify(EventManagerPid, {init, Player}),
    {ok, started, #state{player1 = Player,
			 player2 = none,
			 current_player_no = 0,
			 turn_no = 0,
			 event_manager_pid = EventManagerPid}}.

started({join, Player}, From, State) ->
    case try_join(Player, State) of
	{ok, NewState} ->
	    gen_event:notify(State#state.event_manager_pid, {join, Player}),
	    {reply, ok, wait_turn, NewState};
	{error} ->
	    {reply, error, started, State}
    end;

started(_Event, _From, State) ->
    {reply, error, started, State}.

try_join(Player, State) when State#state.player1 =:= Player ->
    {error};

try_join(Player, State) when State#state.player2 =:= none ->
    {ok, State#state{player2 = Player}};

try_join(_Player, _State) ->
    {error}.

wait_turn({turn, Player}, From, State) ->
    case is_current_player(Player, State) of
	true ->
	    TurnResult = do_turn(Player, State),
	    case TurnResult of
		{ok, NewState} ->
		    {reply, ok, wait_turn, NewState};
		{finish, NewState} ->
		    {stop, normal, stop, NewState}
	    end;
	false ->
	    {reply, error, wait_turn, State}
    end;

wait_turn(_Event, From, State) ->
    {reply, error, wait_turn, State}.

is_current_player(Player, State) ->
    IsCurrent = ((State#state.player1 =:= Player andalso State#state.current_player_no =:= 0) orelse
		 (State#state.player2 =:= Player andalso State#state.current_player_no =:= 1)),
    IsCurrent.

do_turn(Player, State) ->
    State1 = case State#state.current_player_no == 0 of
		 true ->
		     State#state{current_player_no = 1};
		 false ->
		     State#state{current_player_no = 0}
	     end,
    State2 = State1#state{turn_no = State1#state.turn_no +1},

    gen_event:notify(State#state.event_manager_pid, {turn, Player}),    
    case State2#state.turn_no == ?MAX_TURNS of
	true ->
	    gen_event:notify(State#state.event_manager_pid, {finish}),
	    {finish, State2};
	false ->
	    {ok, State2}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
