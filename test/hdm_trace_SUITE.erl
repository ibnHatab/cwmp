%%%-------------------------------------------------------------------
%%% @author Gratiela Ghergu <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, Gratiela Ghergu
%%% @doc
%%% Parser testing against traces takrn from HDM and cpe_sim
%%% @end
%%% Created :  3 Oct 2012 by Gratiela Ghergu <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(hdm_trace_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-include("cwmp.hrl").

-import(cwmp_parser, [parse/1]).

-define(CTDBG(ARG), io:format(user, "~n>> ~p: ~p~n", [??ARG, ARG])).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
isXmlFile(File) ->
    case lists:dropwhile(fun (C) when C =:= $.
				      -> false;
			     (_) -> true end, File) of
	".xml" -> true;
	_ -> false
    end.

init_per_suite(Config) ->
    TraceDir = ?config(data_dir, Config),
    {ok, FileList} = file:list_dir(TraceDir),
    TestList = [begin
		    case string:tokens(File, "_.") of
			[_Prefix, Name, Sufix] ->
			    Sufix == "xml",
			    Name
		    end
		end
		|| File <- FileList, isXmlFile(File)],
    TempDir = mktemp(),
    ok = filelib:ensure_dir(TempDir ++ "/"),
    [{trace_rpc_methods, TestList}, {trace_temp_dir, TempDir} | Config].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [hdm_read_trace_tc, hdm_validate_trace_tc, hdm_compare_trace_tc].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
hdm_read_trace_tc() ->
    [].
hdm_validate_trace_tc() ->
    [].
hdm_compare_trace_tc() ->
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------

hdm_trace_test_case(Config) ->
    TraceDir = ?config(data_dir, Config),
    Methods = ?config(trace_rpc_methods, Config),
    TempDir = ?config(trace_temp_dir, Config),
    lists:foreach(fun (Method) ->
			  TraceFile = filename:join([TraceDir,
						     "cwmp_" ++ Method ++ ".xml"]),
			  XqlFile = filename:join([TraceDir,
						   "cwmp_" ++ Method ++ ".xql"]),
			  RpcFile = filename:join([TempDir,
						   "cwmp_" ++ Method ++ ".xml"]),

			  ct:print("CT parse trace: ~p ~n", [TraceFile]),

			  {Doc, _Rest} = xmerl_scan:file(TraceFile),
			  Rpc = parse(Doc),
			  ?CTDBG(Rpc),
			  ct:print("CT generate trace: ~p ~n", [RpcFile]),
			  RpcDoc = cwmp_builder:build(Rpc),
			  ?CTDBG(RpcDoc),
			  {ok, saved} = savexml(RpcDoc, RpcFile),
			  ok = compare_test(XqlFile, TraceFile, RpcFile)
		  end,
		  Methods),
    ok.


hdm_read_trace_tc(Config) ->
    TraceDir = ?config(data_dir, Config),
    Methods = ?config(trace_rpc_methods, Config),
    lists:foreach(fun (Method) ->
			  TraceFile = filename:join([TraceDir,
						     "cwmp_" ++ Method ++ ".xml"]),
			  ct:print("CT read trace: ~p ~n", [TraceFile]),

			  {Doc, _Rest} = xmerl_scan:file(TraceFile),
			  Rpc = parse(Doc),
			  ?CTDBG(Rpc)
		  end,
		  Methods),
    ok.

hdm_validate_trace_tc(Config) ->
    TraceDir = ?config(data_dir, Config),
    Methods = ?config(trace_rpc_methods, Config),
    TempDir = ?config(trace_temp_dir, Config),
    lists:foreach(fun (Method) ->
			  TraceFile = filename:join([TraceDir,
						     "cwmp_" ++ Method ++ ".xml"]),
			  ct:print("CT validate trace: ~p ~n", [TraceFile]),

			  RpcFile = filename:join([TempDir, "cwmp_" ++ Method ++ ".xml"]),
			  SchemaFile = filename:join([TraceDir, "../../doc/cwmp-1-2.xsd"]),
			  {Doc, _Rest} = xmerl_scan:file(TraceFile),
			  Rpc = parse(Doc),
			  ?CTDBG(Rpc),
			  RpcDoc = cwmp_builder:build(Rpc),
			  ?CTDBG(RpcDoc),
			  {ok, saved} = savexml(RpcDoc, RpcFile),
			  ok = validate_cwmp(RpcFile, SchemaFile)
		  end,
		  Methods),
    ok.

hdm_compare_trace_tc(Config) ->
    ok.


%%--------------------------------------------------------------------
%% Utilities
%%--------------------------------------------------------------------
mktemp() ->
    Time = calendar:local_time(),
    Dir = make_dirname(Time),
    filename:join(["/tmp", Dir]).

make_dirname({{YY,MM,DD},{H,M,S}}) ->
    io_lib:format("cwmp"++".~w-~2.2.0w-~2.2.0w_~2.2.0w.~2.2.0w.~2.2.0w",
		  [YY,MM,DD,H,M,S]).

savexml(Doc, File) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    Content = xmerl:export_simple(Doc, xmerl_xml, [{prolog,[]}]),
	    ok = file:write(Fd, Content),
	    file:close(Fd),
	    {ok, saved};
	_Error ->
	    ct:print("ERROR storing(~p) -> ~p~n", [File, _Error]),
	    {nok, not_saved}
    end.


compare_test(XqlFile, TraceFile, RpcFile) ->
    Command = "xx",

    case os:cmd(Command) of
	"passed" ->
	    ok;
	Error ->
	    ct:print("Error in comparator: ~p~n", [Error]),
	    {error, Error}
    end.

validate_cwmp(RpcFile, SchemaFile) ->
    Command = "xmllint --noout --path doc --schema "
	++ SchemaFile ++ " " ++ RpcFile,
    Result = os:cmd(Command),
    ct:print("Validate: <~p ~n", [Result]),
    ok.
    
    
