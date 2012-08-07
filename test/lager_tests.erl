%%% File    : lager_tests.erl
%%% Description : 


-module(lager_tests).

-include_lib("eunit/include/eunit.hrl").


logtest_test() ->
    io:format(">>Hello"),
    lager:debug(">>HelloE"),
    lager:error(">>Some message"),
    ?assertEqual(6,6).
