-module(test_suite).

-export([test/0]).

test() ->
    eunit:test(fsm_example_tests).
