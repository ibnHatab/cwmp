%%% File    : utils_tests.erl
%%% Description : 


-module(utils_tests).

-include_lib("eunit/include/eunit.hrl").

gproc_reg_test_() ->
    {setup,
     fun register_child/0,
     fun clean/1,
     fun verifying_values/1
     %% ?_test (begin
     %% 		 ?assertEqual(0,1)
     %% 	     end)
    }.

%% create a new process and returning the pid of the created process
server_start() ->
    application:start(sasl),
    application:start(gproc).

register_child () ->
    ok = server_start(),    
    Pid = spawn(fun ()->
			io:format(user, ">> REG ~p~n", [self()]),
			gproc:reg({n, l, me}, 11),
			%% gproc:reg({n,l,1},11),
			%% gproc:reg({n,l,2},22),
			%% gproc:reg({n,l,3},33),
			io:format(user, ">> NL ~p~n", [
						       gproc:get_value({n,l,1},self())]),
			receive
			   A -> A
			end
			    
		end
	       ),
    Pid.

%% killing the process Pid
clean(Pid)->
    exit(Pid,kill).


verifying_values(Pid)->
	    io:format(user, ">> ~p~n", [Pid]),
	    ?assertEqual(11,gproc:get_value({n, l, me}, Pid))
	    %% ?assertEqual(11,gproc:get_value({n,l,1},Pid)),
	    %% ?assertEqual(22,gproc:get_value({n,l,2},Pid)),
	    %% ?assertEqual(32,gproc:get_value({n,l,3},Pid))
    .
