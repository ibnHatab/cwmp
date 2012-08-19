%%% File    : tr_soap_types.erl
%%% Description : SOAP Type Parser for TR-069 RPC Methods

-module(tr_soap_types).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-export([ parse_AccessListChange/1,
	  parse_AnnounceURL/1,
	  parse_Arg/1,
	  parse_Command/1,
	  parse_CommandKeyType/1,
	  parse_CompleteTime/1,
	  parse_CurrentTime/1,
	  parse_DelaySeconds/1,
	  parse_DeploymentUnitOperationType/1,
	  parse_DeploymentUnitRef/1,
	  parse_DeploymentUnitState/1,
	  parse_EventCodeType/2,
	  parse_ExecutionEnvRef/1,
	  parse_ExecutionUnitRefList/1,
	  parse_ExpirationDate/1,
	  parse_FailureURL/1,
	  parse_FaultCode/1,
	  parse_FaultString/1,
	  parse_FileSize/1,
	  parse_FileType/1,
	  parse_InstanceNumber/1,
	  parse_IsDownload/1,
	  parse_IsTransferable/1,
	  parse_Manufacturer/1,
	  parse_MaxEnvelopes/1,
	  parse_MaxRetries/1,
	  parse_Mode/1,
	  parse_Name/1,
	  parse_Next/1,
	  parse_NextLevel/1,
	  parse_NextURL/1,
	  parse_Notification/1,
	  parse_NotificationChange/1,
	  parse_ObjectNameType/1,
	  parse_OptionName/1,
	  parse_OUI/1,
	  parse_ParameterKeyType/1,
	  parse_ParameterPath/1,
	  parse_Password/1,
	  parse_ProductClass/1,
	  parse_Referer/1,
	  parse_Resolved/1,
	  parse_RetryCount/1,
	  parse_SerialNumber/1,
	  parse_StartDate/1,
	  parse_StartTime/1,
	  parse_State/1,
	  parse_Status/1,
	  parse_SuccessURL/1,
	  parse_TargetFileName/1,
	  parse_TransferURL/1,
	  parse_URL/1,
	  parse_UserMessage/1,
	  parse_Username/1,
	  parse_UUID/1,
	  parse_Value/1,
	  parse_Version/1,
	  parse_VoucherSN/1,
	  parse_WindowEnd/1,
	  parse_WindowMode/1,
	  parse_WindowStart/1,
	  parse_Writable/1
	]).

-export([ parse_string/1,
	  parse_boolean/1,
	  parse_int/1,
	  parse_dateTime/1,
	  parse_base64/1,
	  parse_anyURI/1,
	  parse_attribete/3,
	  parse_anySimpleType/1
	]).

-import(tr_soap_lib, [return_error/2, parse_error/2]).


-ifdef(TEST).
-import(tr_soap_lib, [get_QName/2]).
-endif.

%%%-----------------------------------------------------------------------------
%% Internal API
%%%-----------------------------------------------------------------------------

get_xmlText(Content) ->
    XS = lists:filter(fun tr_soap_lib:xmlText/1, Content),
    Text = lists:foldl(fun (#xmlText{value=Val}, Acct) ->
			       Val ++ Acct
		       end, "", XS),
    Text.

check_Value(Name, String, Type) ->
    case xmerl_xsd_type:check_simpleType(Type, String, []) of
        {ok, Value} -> Value;
        {error, Error} -> return_error(Name, {String, Error})
    end.

%% This datatype describes instances identified by the combination of
%% a date and a time. Its value space is described as a combination of
%% date and time of day in Chapter 5.4 of ISO 8601. Its lexical space
%% is the extended format:
%%
%%  [-]CCYY-MM-DDThh:mm:ss[Z|(+|-)hh:mm]
%%
%% The time zone may be specified as Z (UTC) or (+|-)hh:mm. Time zones
%% that aren't specified are considered undetermined.
convert_iso8601_date(DateTime) ->
    [SDate,Time] = string:tokens(DateTime,"T"),
    {Sign, Date} = case SDate of
		       "-" ++ Val -> {-1, Val};
		       Val -> {1, Val}
		   end,
    [Y,M,D] = string:tokens(Date,"-"),
    DT = {Sign * list_to_integer(Y),
	  list_to_integer(M),
	  list_to_integer(D)
	 },
    convert_iso8601_date(DT, Time).

convert_iso8601_date(DT, Time) ->
%% hh:mm:ss (.s+)? TZ
    {HMS,TZ} =
	case lists:split(8,Time) of
	    {T,"."++SecFractionsTZ} ->
		OnlyDigits = fun(X) when X>=$0,X=<$9 ->true;(_)->false end,
		{_SecFrac,TZone} = lists:splitwith(OnlyDigits,SecFractionsTZ),
		{T,TZone};
	    {T,TZone} ->
		{T,TZone}
	end,
    [H,M,S] = string:tokens(HMS,":"),
    TM = {list_to_integer(H),
	  list_to_integer(M),
	  list_to_integer(S)
	 },
    case TZ of
	[] ->
	    {DT, TM}; %% timezone optional
	_ ->
	    convert_iso8601_date(DT, TM, TZ)
    end.
%% Adjust Time Zone
convert_iso8601_date(DT, TM, "Z") ->
    {DT, TM};
convert_iso8601_date({Syear,Smonth,Sday} = _DT,
		     {Shour,Sminute,Ssec} = _TM, TZ) ->
    Szone = case TZ of
		"+" ++ HM ->
		    [H,M] = string:tokens(HM,":"),
		    {pos, list_to_integer(H), list_to_integer(M)};
		"-" ++ HM ->
		    [H,M] = string:tokens(HM,":"),
		    {neg, list_to_integer(H), list_to_integer(M)}
	    end,
    {NY,NM,ND,NHour,NMin,Sec,_Nzone} =
	xmerl_xsd_type:normalize_dateTime({Syear,Smonth,Sday,Shour,Sminute,Ssec,Szone}),
    {{NY,NM,ND},{NHour,NMin,Sec}}.


%%%-----------------------------------------------------------------------------
%% SOAP Type parsers
%%%-----------------------------------------------------------------------------

-spec parse_boolean(#xmlElement{content::[any()]}) -> boolean().
parse_boolean(#xmlElement{name=Name, content = Content}) ->
    String = string:strip(get_xmlText(Content)),    
    list_to_boolean(check_Value(Name, String, boolean)).

list_to_boolean("1") -> true;
list_to_boolean("true") -> true;
list_to_boolean("0") -> false;
list_to_boolean("false") -> false.

-spec parse_string(#xmlElement{content::[any()]}) -> string().
parse_string(#xmlElement{name=Name, content = Content}) ->
    check_Value(Name, get_xmlText(Content), string).

-spec parse_int(#xmlElement{content::[any()]}) -> integer().
parse_int(#xmlElement{name=Name, content = Content}) ->
    String = string:strip(get_xmlText(Content)),
    ValueString = check_Value(Name, String, int),
    {Int,_Rest} = string:to_integer(ValueString),
    Int.

-spec parse_unsignedInt(#xmlElement{content::[any()]}) -> non_neg_integer().
parse_unsignedInt(#xmlElement{name=Name, content = Content}) ->
    String = string:strip(get_xmlText(Content)),
    ValueString = check_Value(Name, String, unsignedInt),
    {UInt,_Rest} = string:to_integer(ValueString),
    UInt.

-spec parse_anyURI(#xmlElement{content::[any()]}) -> tuple().
parse_anyURI(#xmlElement{name=Name, content = Content}) ->
    String = get_xmlText(Content),
    case xmerl_uri:parse(String) of
	{error,Error} ->
	    return_error(Name, {String, Error});
        URI -> URI
    end.

parse_dateTime(#xmlElement{name=Name, content = Content} = _E) when is_tuple(_E)->
    String = string:strip(get_xmlText(Content)),
    ValueString = check_Value(Name, String, dateTime),
    parse_dateTime(ValueString);
parse_dateTime(String) when is_list(String) ->
    convert_iso8601_date(String).

parse_base64(_E) ->
    <<"aa">>.

-spec parse_attribete(#xmlElement{}, atom(), atom()) -> any() | undefined.
parse_attribete(Elem, AttrName, boolean) ->
    case  get_AttributeByName(Elem, AttrName) of
	undefined ->
	    undefined;
	Value ->
	    list_to_boolean(check_Value(AttrName, Value, boolean))
    end;
parse_attribete(Elem, AttrName, string) ->
    case  get_AttributeByName(Elem, AttrName) of
	undefined ->
	    undefined;
	Value ->
	    check_Value(AttrName, Value, string)
    end;
parse_attribete(Elem, AttrName, int) ->
    case  get_AttributeByName(Elem, AttrName) of
	undefined ->
	    undefined;
	Value ->
	    IntS = check_Value(AttrName, Value, int),
	    string:to_integer(IntS)
    end;
parse_attribete(Elem, AttrName, Type) ->
    case  get_AttributeByName(Elem, AttrName) of
	undefined ->
	    undefined;
	Value ->
	    check_Value(AttrName, Value, Type)
    end.

get_AttributeByName(Elem, AttrName) ->
    case lists:keyfind(AttrName, 2, Elem#xmlElement.attributes) of
	false ->
	    undefined;
	Attr ->
	    Attr#xmlAttribute.value
    end.


parse_DeploymentUnitOperationType(E) ->
    case parse_string(E) of
	V when V == "Install"; V == "Update"; V == "Uninstall"
	       -> V;
	V ->
	    parse_error(E, {V, "Enumeration"})
    end.

parse_AccessListChange(E)     -> parse_boolean(E).
parse_AnnounceURL(E)          -> parse_string(E).
parse_anySimpleType(E)        -> parse_string(E).
parse_Arg(E)                  -> parse_string(E).
parse_Command(E)              -> parse_string(E).
parse_CommandKeyType(E)       -> parse_string(E).
parse_CompleteTime(E)         -> parse_dateTime(E).
parse_CurrentTime(E)          -> parse_dateTime(E).
parse_DelaySeconds(E)         -> parse_unsignedInt(E).
parse_DeploymentUnitRef(E)    -> parse_string(E).
parse_DeploymentUnitState(E)  -> parse_string(E).
parse_ExecutionEnvRef(E)      -> parse_string(E).
parse_ExecutionUnitRefList(E) -> parse_string(E).
parse_ExpirationDate(E)       -> parse_dateTime(E).
parse_FailureURL(E)           -> parse_string(E).
parse_FaultCode(E)            -> parse_unsignedInt(E).
parse_FaultString(E)          -> parse_string(E).
parse_FileSize(E)             -> parse_unsignedInt(E).
parse_FileType(E)             -> parse_string(E).
parse_InstanceNumber(E)       -> parse_unsignedInt(E).
parse_IsDownload(E)           -> parse_boolean(E).
parse_IsTransferable(E)       -> parse_int(E).
parse_Manufacturer(E)         -> parse_string(E).
parse_MaxEnvelopes(E)         -> parse_unsignedInt(E). %FIXME: inline
parse_MaxRetries(E)           -> parse_int(E).
parse_Mode(E)                 -> parse_int(E).
parse_Name(E)                 -> parse_string(E).
parse_Next(E)                 -> parse_string(E).
parse_NextLevel(E)            -> parse_boolean(E).
parse_NextURL(E)              -> parse_string(E).
parse_NotificationChange(E)   -> parse_boolean(E).
parse_Notification(E)         -> parse_int(E).
parse_ObjectNameType(E)       -> parse_string(E).
parse_OptionName(E)           -> parse_string(E).
parse_OUI(E)                  -> parse_string(E).
parse_ParameterKeyType(E)     -> parse_string(E).
parse_ParameterPath(E)        -> parse_string(E).
parse_Password(E)             -> parse_string(E).
parse_ProductClass(E)         -> parse_string(E).
parse_Referer(E)              -> parse_string(E).
parse_Resolved(E)             -> parse_boolean(E).
parse_RetryCount(E)           -> parse_unsignedInt(E).
parse_SerialNumber(E)         -> parse_string(E).
parse_StartDate(E)            -> parse_dateTime(E).
parse_StartTime(E)            -> parse_dateTime(E).
parse_State(E)                -> parse_unsignedInt(E).
parse_Status(E)               -> parse_int(E).
parse_SuccessURL(E)           -> parse_string(E).
parse_TargetFileName(E)       -> parse_string(E).
parse_TransferURL(E)          -> parse_string(E).
parse_URL(E)                  -> parse_anyURI(E).
parse_UserMessage(E)          -> parse_string(E).
parse_Username(E)             -> parse_string(E).
parse_UUID(E)                 -> parse_string(E).
parse_Value(E)                -> parse_string(E).
parse_Version(E)              -> parse_string(E).
parse_VoucherSN(E)            -> parse_unsignedInt(E).
parse_WindowEnd(E)            -> parse_unsignedInt(E).
parse_WindowMode(E)           -> parse_string(E).
parse_WindowStart(E)          -> parse_unsignedInt(E).
parse_Writable(E)             -> parse_boolean(E).


match_event_code(Code, Description) ->
    case Code of
	"M" ->
	    Key = Code ++ " " ++ Description, 
	    case lists:keyfind(Key, 2, ?SUPPORTED_EVENT_CODE_TYPE) of
		{OpCode, _Key} ->
		    OpCode;
		false ->
		    {error, "Wrong event: " ++ Key}
	    end;
	S ->
	    case string:to_integer(S) of
		{error, Reason} ->
		    {error,Reason};
		OpCode ->
		    OpCode
	    end
    end.

-spec parse_EventCodeType(#xmlElement{},#parser{}) -> event_code_type().
parse_EventCodeType(Elem, State) ->
    String = parse_string(Elem),
    case string:chr(String, $\s) of
	0 ->
	    parse_error(Elem, State);
	Space ->
	    Code = string:substr(String, 1, Space -1),
	    Description = string:substr(String, Space +1),
	    case match_event_code(Code, Description) of
		{error,Reason} ->
		    parse_error(Elem, Reason);
		Event ->
		    Event
	    end
    end.
    

%% end

%%%-----------------------------------------------------------------------------
%%% Unitary tetsts
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

make_Element(Name, Text) when is_list(Name) ->
    QName = get_QName(Name, "cwmp"),
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


parse_boolean_test() ->
    E = make_Element("NoMoreRequests", "0"),
    ?assertEqual(false, parse_boolean(E)),
    ok.

parse_int_test() ->
    E = make_Element("NoMoreRequests", "42"),
    ?assertEqual(42, parse_int(E)),
    ok.

parse_anyURI_test() ->
    E = make_Element("URL", "http://cpe-host-name/kick.html?command=cmd&arg=1&next=home"),
    ?assertEqual({http,"cpe-host-name",80,"/kick.html","?command=cmd&arg=1&next=home"},
		 parse_anyURI(E)).

parse_iso8601_test() ->
    [
     begin
	 ?DBG({DT, Str, convert_iso8601_date(Str)}),
	 DT = convert_iso8601_date(Str)
	     
     end
      ||
	{DT, Str} <- lists:zip([{{2004, 11, 01}, {04, 40, 35}},
				{{2004, 11, 01}, {04, 40, 35}},
				{{-2000, 01, 12}, {12, 13, 14}},
				{{2000, 01, 12}, {0,   0,  0}},
				{{2009, 06, 25}, {04, 32, 31}}
			       ],
			       [ "2004-10-31T21:40:35.5-07:00",
				 "2004-11-01T04:40:35.5Z",
				 "-2000-01-12T12:13:14Z",
				 "2000-01-12T00:00:00Z",
				 "2009-06-25T05:32:31+01:00"
			       ])
    ].


-endif.
