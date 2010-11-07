-module(fsm_example_tests).

-include_lib("eunit/include/eunit.hrl").

hook_test() ->
    {ok, EventManagerPid} = gen_event:start_link(),
    gen_event:add_handler(EventManagerPid, print_event_handler, []),

    {ok, Pid} = game_fsm:start_link("player1", EventManagerPid),
    Result1 = game_fsm:join(Pid, "player1"),
    ?assert(Result1 =:= error),
    Result2 = game_fsm:join(Pid, "player2"),
    ?assert(Result2 =:= ok),
    Result3 = game_fsm:join(Pid, "player2"),
    ?assert(Result3 =:= error),

    Result4 = game_fsm:turn(Pid, "player2"),
    ?assert(Result4 =:= error),
    Result5 = game_fsm:turn(Pid, "player1"),
    ?assert(Result5 =:= ok),
    Result6 = game_fsm:turn(Pid, "player2"),
    ?assert(Result6 =:= ok),

    Result7 = game_fsm:turn(Pid, "player2"),
    ?assert(Result7 =:= error),

    Result8 = game_fsm:turn(Pid, "player1"),
    ?assert(Result8 =:= stop).
