%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%
%%% @end
%%% Created : 13 Aug 2012 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(soap_parser_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


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
init_per_suite(Config) ->
    Config.

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
init_per_group(soap_parse_doc, Config) ->
    Config;
init_per_group(soap_parse_types, Config) ->
    Config;
init_per_group(_Group, Config) ->
    Config.


%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(soap_parse_doc, _Config) ->
    ok;
end_per_group(soap_parse_types, _Config) ->
    ok;
end_per_group(_Group, _Config) ->
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
    [{soap_parse_types, [sequence], [parse_boolean_tc,
				     parse_iso8601_tc,
				     parse_string_tc,
				     parse_int_tc,
				     parse_unsignedInt_tc
				    ]},
     {soap_parse_doc, [sequence], [
				  ]}
    ].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->    
    [parse_boolean_tc,
     parse_iso8601_tc,
     parse_string_tc,
     parse_int_tc,
     parse_unsignedInt_tc
    ].
     %% 	,
    %% [{group, soap_parse_types},
    %%  {group, soap_parse_doc}].


%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
parse_boolean_tc() -> 
    [].

parse_iso8601_tc() ->
    [].

parse_string_tc() ->
    [].

parse_int_tc() ->
    [].

parse_unsignedInt_tc() ->
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
parse_boolean_tc(_Config) ->
    [parse_boolean_check(Expect, String)
     || {Expect, String} <- [
			     {false,   "0"}
			     , {false, "false"}
			     , {true,  "1"}
			     , {true,  "true"}
			    ]],
    ok.


parse_boolean_check(Expect, String) ->
    %% setup
    E = make_Element('NoMoreRequests', String),
    %% execute
    ?line Res = tr_soap_types:parse_boolean(E),
    %assert
    Expect = Res.



parse_iso8601_tc(_Config) ->
     [
      parse_iso8601_check(Expect, String)
      ||
	 {Expect, String} <- lists:zip([{{2004, 11, 01},    {04, 40, 35}}
					,{ {2004, 11, 01},  {04, 40, 35}}
					,{{-2000, 01, 12},  {12, 13, 14}}
					,{ {2000, 01, 12},  {0,   0,  0}}
					,{ {2009, 06, 25},  {04, 32, 31}}
				       ],
				       [ "2004-10-31T21:40:35.5-07:00",
					 "2004-11-01T04:40:35.5Z",
					 "-2000-01-12T12:13:14Z",
					 "2000-01-12T00:00:00Z",
					 "2009-06-25T05:32:31+01:00"
				       ])],
    ok.

parse_iso8601_check(Expect, String) ->
    %execute
    Res = tr_soap_types:convert_iso8601_date(String),
    %assert
    Expect = Res.

parse_string_tc(_Config) ->
    [
     begin
	 parse_string_check(Expect, String),
	 ct:print("--> ~p ~p ~n",[Expect, parse_string_check(Expect, String)])
     end
      ||
	{Expect, String} <- [
			     {"This is a sentence", "This is a sentence"}
			     , {"false", "false"}
			     , {"1", "1"}
			     , {"true", "true"}
			    ]],
    ok.
			   
parse_string_check(Expect, String) ->
    %setup
    E = make_Element('ParseString',String),
    %execute
    Res = tr_soap_types:parse_string(E),
    %assert
    Expect = Res.

parse_int_tc(_Config) ->
    [
     begin
	 parse_int_check(Expect, String),
	 ct:print("--> ~p ~p ~n",[Expect, parse_int_check(Expect, String)])
     end
      ||
	{Expect, String} <- [
			     { -123, "-123"}
			     , { 12, "   12"}
			     , {  0, "+0  "}
			     , {-21, " -21 "}
			     , {  1, " +1   "}
			     , { 11, "  11  "}
			     , {  0, "   -0  "}
			    ]],
    ok.
    
parse_int_check(Expect, String) ->
    %setup
    E = make_Element('ParseInt', String),
    %execute
    Res = tr_soap_types:parse_int(E),
    %assert
    Expect = Res.


parse_unsignedInt_tc(_Config) ->
    [
     begin
	 parse_unsignedInt_check(Expect, String),
	 ct:print("--> ~p ~p ~n",[Expect, parse_unsignedInt_check(Expect, String)])
     end
      ||
	{Expect, String} <- [
			     {123,  "123"}
			     , {12, "   12"}
			     , { 0, "-0  "}
			     , { 1, " 1   "}
			     , {11, "  11  "}
			     , { 0, " +0 "}
			    ]],
    ok.


parse_unsignedInt_check(Expect, String) ->
    %setup
    E = make_Element('ParseUnsignedInt', String),
    %execute
    Res = tr_soap_types:parse_unsignedInt(E),
    %assert
    Expect = Res.

    
%%--------------------------------------------------------------------
%%% Local API
%%--------------------------------------------------------------------

make_Element(Name, Text) when is_atom(Name) ->
    QName = tr_soap_lib:get_QName(Name, 'cwmp'),
 %   ?DBG(QName),    
    {xmlElement, QName, QName,
     {"cwmp", Name},
         {xmlNamespace,[],
          [{"soap-enc",'http://schemas.xmlsoap.org/soap/encoding/'},
           {"soap-env",'http://schemas.xmlsoap.org/soap/envelope/'},
           {"cwmp",'urn:dslforum-org:cwmp-1-0'}]},
     [{'soap-env:Header',2},{'soap-env:Envelope',1}], 4,[],
     [
      {xmlText,
       [{QName,4},{'soap-env:Header',2},{'soap-env:Envelope',1}],1,[],
       Text
       ,text}
     ], [],"/local/vlad/repos/otp/tr69/src",undeclared}.
