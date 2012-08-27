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
-include_lib("xmerl/include/xmerl.hrl").

-include("proto.hrl").
-include("tr69.hrl").

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
    [{soap_parse_types, [sequence], [
				     parse_boolean_tc
				     ,parse_iso8601_tc
				     ,parse_string_tc
				     ,parse_int_tc
				     ,parse_unsignedInt_tc
				     ,parse_anyURI_tc
				     ,name_namespace_tc
				     ,check_namespace_tc
				     ,parse_EventCodeType_tc
				     ,parse_ArraySize_tc
				     ,parse_FileType_tc
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
    [
     parse_boolean_tc
     ,parse_iso8601_tc
     ,parse_string_tc
     ,parse_int_tc
     ,parse_unsignedInt_tc
     ,parse_anyURI_tc
     ,name_namespace_tc
     ,check_namespace_tc
     ,parse_EventCodeType_tc
     ,parse_ArraySize_tc
     ,parse_FileType_tc
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

parse_anyURI_tc() ->
    [].

name_namespace_tc() ->
    [].

check_namespace_tc()->
    [].

parse_EventCodeType_tc() ->
    [].

parse_ArraySize_tc() ->
    [].

parse_FileType_tc() ->
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
    [
     parse_boolean_check(Expect, String)
     ||
	{Expect, String} <- [
			     {false,    "0"}
			     , {false,  "false"}
			     , { true,  "1"}
			     , { true,  "true"}
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
				       [ "2004-10-31T21:40:35.5-07:00"
					 ,"2004-11-01T04:40:35.5Z"
					 ,"-2000-01-12T12:13:14Z"
					 ,"2000-01-12T00:00:00Z"
					 ,"2009-06-25T05:32:31+01:00"
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
	 parse_string_check(Expect, String)
	 %% ct:print("--> ~p ~p ~n",[Expect, parse_string_check(Expect, String)])
     end
      ||
	{Expect, String} <- [
			     {"This is a sentence", "This is a sentence"}
			     , {           "false", "false"}
			     , {               "1", "1"}
			     , {            "true", "true"}
			    ]],
    ok.
			   
parse_string_check(Expect, String) ->
    %setup
    E = make_Element('ParseString', String),
    %execute
    Res = tr_soap_types:parse_string(E),
    %assert
    Expect = Res.

parse_int_tc(_Config) ->
    [
     begin
	 parse_int_check(Expect, String)
	 %% ct:print("--> ~p ~p ~n",[Expect, parse_int_check(Expect, String)])
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
	 parse_unsignedInt_check(Expect, String)
	 %% ct:print("--> ~p ~p ~n",[Expect, parse_unsignedInt_check(Expect, String)])
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

parse_anyURI_tc(_Config) ->
     [
     begin
	 parse_anyURI_check(Expect, String)
	 %% ct:print("--> ~p ~p ~n",[Expect, parse_anyURI_check(Expect, String)])
     end
      ||
	 {Expect, String} <- lists:zip([{http, "schemas.xmlsoap.org", 80, "/soap/envelope/", [] }
					,{http, "schemas.xmlsoap.org", 80, "/soap/encoding/", [] }
					,{http, "www.w3.org", 80, "/2001/XMLSchema", []}
					,{http, "www.w3.org", 80, "/2001/XMLSchema-instance", []}
				       ],
				       [ "http://schemas.xmlsoap.org/soap/envelope/"
					 ,"http://schemas.xmlsoap.org/soap/encoding/"
					 ,"http://www.w3.org/2001/XMLSchema"
					 ,"http://www.w3.org/2001/XMLSchema-instance"
				       ])],
    ok.

parse_anyURI_check(Expect, String) ->
    %setup
    E = make_Element('ParseAnyURI', String),
    %execute
    Res = tr_soap_types:parse_anyURI(E),
    %assert
    Expect = Res.


name_namespace_tc(_Config) ->
    {'','name'} = tr_soap_lib:local_name('name'),

    'name' = tr_soap_lib:get_local_name('ns:name'),
    
    {Name, Ns} = {'ns:name', 'ns'},
    Name = tr_soap_lib:get_QName(tr_soap_lib:get_local_name(Name), Ns),
    
    ok.

%% name_namespace_check(_Expect, _String) ->  
%%     ok.

-define(XML_NAMESPACE,
	{xmlNamespace,[],
	 [{"soapenc",'http://schemas.xmlsoap.org/soap/encoding/'},
	  {"soapenv",'http://schemas.xmlsoap.org/soap/envelope/'},
	  {"cwmp",'urn:dslforum-org:cwmp-1-0'}]}
       ).


check_namespace_tc(_Config) ->
    Nss = tr_soap_lib:match_cwmp_ns_and_version(?XML_NAMESPACE),
    State = tr_soap_lib:check_namespace('soap-env:Envelope',
     			    #xmlElement {name='soapenv:Envelope'}, #parser{ns=Nss}),
    
    %%    ct:print(">>> ~p ~n>>> ~p ~n",[Nss#rpc_ns{inherited='soapenv'},State#parser.ns]),
    %% illegal pattern 
    %%Nss#rpc_ns{inherited='soapenv'} =  State#parser.ns,
    
    Body = tr_soap_lib:check_namespace('soap-env:Body',#xmlElement {name='soapenv:Body'}, State),
    State = Body,
    %%    ct:print(">>> ~p ~n>>> ~p ~n",[State, Body]),

    Header = tr_soap_lib:check_namespace('soap-env:Header',#xmlElement {name='soapenv:Header'}, State),
    State = Header,
    %%    ct:print(">>> ~p ~n>>> ~p ~n",[State, Header]),

    ok.
    
%% FIXME
parse_EventCodeType_tc(_Config) ->
    Elem = make_Element('ParseEventCodeType',"9 REQUEST, DOWNLOAD"),
    State = #parser{},
    
    ct:print(">>>  ~p ~n",[tr_soap_types:parse_EventCodeType(Elem, State)]),
    
    E = make_Element('ParseEventCodeType',"M ChangeDUState"),
    ct:print("--> ~p returned --> ~p ~n",[tr_soap_types:parse_string(E), tr_soap_types:parse_EventCodeType(E, State)]),
    
    M = make_Element('ParseEventCodeType',"M ScheduleInform"),
    ct:print("--> ~p returned --> ~p ~n",[tr_soap_types:parse_string(E), tr_soap_types:parse_EventCodeType(M, State)]),

    ok.

parse_ArraySize_tc(_Config) ->
    %% Nss = #parser{},
    %% ct:print(">>> ~p ~n",[tr_soap_types:parse_ArraySize("10", "abc", Nss)]),
    ok.


%%FIXME when String = "OUI Vendor speceific, ID"
parse_FileType_tc(_Config) ->
    [
     begin
	 parse_FileType_check(Expect, String, Type)
	 %% ,ct:print(">> ~p ~p ~p ~n",[Expect, String, Type])
     end
     ||
	{Expect, String, Type} <- [{ 1, "1 Firmware Upgrade, Image"             , 'DownloadFileType'}
				   ,{ 3, "3 Vendor Configuration, File"         , 'DownloadFileType'}
				   ,{ 2, "2 Vendor Log, File"                   , 'UploadFileType'}
				   ,{ 3, "3 Vendor Configuration File, [1-9]\d*", 'UploadFileType'}
				   ,{ 4, "4 Vendor Log, File"                   , 'TransferFileType'}
				   ,{ 6, "6 Ringer, File"                       , 'TransferFileType'}
				  ]],			    	    
    ok.

parse_FileType_check(Expect, String, Type) ->
    %setup
    E = make_Element(Type, String),
    %execute
    Res = tr_soap_types:parse_FileType(Type, E),
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
