%% common_test suite for hex

-module(hex_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 20}},
	    {require,global_names},
	    {userdata,[{info,"This suite tests database transactions."}]},
	    {silent_connections,[telnet]},
	    {stylesheet,"db_testing.css"}
	   ].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() -> [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%
%%      NB: By default, we export all 1-arity user defined functions
%%--------------------------------------------------------------------
all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    Tests = [ FName || {FName, _} <- lists:filter(
				       fun ({module_info,_})	-> false;
					   ({all,_})		-> false;
					   ({init_per_suite,1}) -> false;
					   ({end_per_suite,1})	-> false;
					   ({_,1})		-> true;
					   ({_,_})		-> false
				       end, Functions)],
    Tests.

-ifdef (ZERO).
all_1(suite) ->    
    [test_case,
     {conf,
      configuration_start, encode_test_1, configuration_stop}].

all_2(suite) -> 
    [{conf,
      tracing_start,[encode_test_1],
      tracing_stop}].
-endif.
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
%    {tc_status, Status} = lists:keyfind(tc_status, 1, Config),
    Status = ?config(tc_status, Config),
    io:format(user, ">>Status: ~p~n", [Status]),
    Config. 

%%--------------------------------------------------------------------
configuration_start(doc) ->
  ["Start of configuration case"];
configuration_start(suite) ->
  [];
configuration_start(Config) when is_list(Config) ->
  io:format("Configuration test case starts...\n"),
  Config.

configuration_stop(doc) ->
  ["Start of configuration case"];
configuration_stop(suite) ->
  [];
configuration_stop(Config) when is_list(Config) ->
  io:format("Configuration test case ends!\n"),
  Config.
%%--------------------------------------------------------------------

tracing_start(doc) ->
  ["Starts tracing on test_case"];
tracing_start(suite) ->
  [];
tracing_start(Config) ->
  debughelper:start(),
  debughelper:trace(?MODULE,test_case),
  Config.

%%--------------------------------------------------------------------

test_hex() ->
    [{userdata,[{doc,"Testing the hex module"}]}].

test_hex(_Config) ->
    %%    ct:get_config
    {skip,"Not implemented."}.

%%--------------------------------------------------------------------

encode_test_1() -> 
    [{require, ftp},
     {default_config, ftp, [{ftp, "my_ftp_host"},
			    {username, "aladdin"},
			    {password, "sesame"}]}].
encode_test_1(doc) -> 
  ["Tests that hex:encode(...) works"];

encode_test_1(Config) when is_list(Config) -> 
  ?line ExpectedHexString = "48656C6C6F21576F726C64",
  ?line HexString = hex:encode("Hello World"),
  ?line ExpectedHexString = HexString.

run_eunit(_Config) ->
    TestsToRun = [],
    ok = eunit:test(TestsToRun).

