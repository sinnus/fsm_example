-module(fsm_example_tests).

-include_lib("eunit/include/eunit.hrl").

hook_test() ->
    {ok, Pid} = game_fsm:start_link("player1"),
    Result1 = game_fsm:join(Pid, "player1"),
    ?assert(Result1 =:= error),
    Result2 = game_fsm:join(Pid, "player2"),
    ?assert(Result2 =:= ok),
    Result3 = game_fsm:join(Pid, "player2"),
    ?assert(Result3 =:= error).
