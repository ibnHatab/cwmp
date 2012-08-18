
-module(tr_soap_types).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-import(tr_soap_lib, [return_error/2, parse_error/2, get_local_name/2]).

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
%parse_attribete(Elem, AttrName, string) ->
    

%FIXME: booom !!!
parse_(_E) ->  ok.

parse_DeploymentUnitOperationType(E,_S) -> parse_string(E).
parse_anySimpleType(E,_S) -> parse_string(E).

%FIXME: add context knowledge here
parse_URL(E,_S) when is_list(E) -> parse_string(E);
parse_URL(E,_S) -> parse_anyURI(E).

parse_AccessListChange(E,_S) -> parse_boolean(E).
parse_AccessListValueType(E,_S) -> parse_string(E).
parse_ACSFaultCodeType(E,_S) -> parse_unsignedInt(E).
parse_ACSVendorFaultCodeType(E,_S) -> parse_unsignedInt(E).
parse_AnnounceURL(E,_S) -> parse_string(E).
parse_Arg(E,_S) -> parse_string(E).
parse_Command(E,_S) -> parse_string(E).
parse_CommandKeyType(E,_S) -> parse_string(E).
parse_CompleteTime(E,_S) -> parse_dateTime(E).
parse_CPEExtensionFaultCodeType(E,_S) -> parse_unsignedInt(E).
parse_CPEFaultCodeType(E,_S) -> parse_unsignedInt(E).
parse_CPEVendorFaultCodeType(E,_S) -> parse_unsignedInt(E).
parse_CurrentTime(E,_S) -> parse_dateTime(E).
parse_DefaultDeploymentUnitOperationType(E,_S) -> parse_string(E).
parse_DelaySeconds(E,_S) -> parse_unsignedInt(E).
parse_DeploymentUnitCPEFaultCodeType(E,_S) -> parse_string(E). %parse_CPEFaultCodeType(E).
parse_DeploymentUnitRef(E,_S) -> parse_string(E).
parse_DeploymentUnitState(E,_S) -> parse_string(E).
parse_DeploymentUnitUUID(E,_S) -> parse_string(E).
parse_DownloadFileType(E,_S) -> parse_string(E).


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
    case string:chr(String) of
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
    
parse_ExecutionEnvRef(E,_S) -> parse_string(E).
parse_ExecutionUnitRefList(E,_S) -> parse_string(E).
parse_ExpirationDate(E,_S) -> parse_dateTime(E).
parse_FailureURL(E,_S) -> parse_string(E).

parse_FaultCode(E) -> parse_unsignedInt(E).
parse_FaultString(E) -> parse_string(E).

parse_FileSize(E,_S) -> parse_unsignedInt(E).
parse_InstanceNumber(E,_S) -> parse_unsignedInt(E).
parse_IsDownload(E,_S) -> parse_boolean(E).
parse_IsTransferable(E,_S) -> parse_int(E).
parse_Manufacturer(E,_S) -> parse_string(E).
parse_MaxEnvelopes(E,_S) -> parse_unsignedInt(E). %FIXME: inline
parse_MaxRetries(E,_S) -> parse_int(E).
parse_Mode(E,_S) -> parse_int(E).
parse_Name(E,_S) -> parse_string(E).
parse_Next(E,_S) -> parse_string(E).
parse_NextLevel(E,_S) -> parse_boolean(E).
parse_NextURL(E,_S) -> parse_string(E).
parse_NotificationChange(E,_S) -> parse_boolean(E).
parse_ObjectNameType(E,_S) -> parse_string(E).
parse_OptionName(E,_S) -> parse_string(E).
parse_OUI(E,_S) -> parse_string(E).
parse_ParameterAttributeNotificationValueType(E,_S) -> parse_int(E).
parse_ParameterKeyType(E,_S) -> parse_string(E).
parse_ParameterName(E,_S) -> parse_string(E).
parse_ParameterPath(E,_S) -> parse_string(E).
parse_Password(E,_S) -> parse_string(E).
parse_ProductClass(E,_S) -> parse_string(E).
parse_Referer(E,_S) -> parse_string(E).
parse_Resolved(E,_S) -> parse_boolean(E).
parse_RetryCount(E,_S) -> parse_unsignedInt(E).
parse_SerialNumber(E,_S) -> parse_string(E).
parse_StartDate(E,_S) -> parse_dateTime(E).
parse_StartTime(E,_S) -> parse_dateTime(E).
parse_State(E,_S) -> parse_unsignedInt(E).
parse_Status(E,_S) -> parse_int(E).
parse_string(E,_S) -> parse_string(E).
parse_SuccessURL(E,_S) -> parse_string(E).
parse_TargetFileName(E,_S) -> parse_string(E).
parse_TimeWindowModeValueType(E,_S) -> parse_string(E).
parse_TransferCompleteCPEFaultCodeType(E,_S) -> parse_string(E). %parse_CPEFaultCodeType(E).
parse_TransferFileType(E,_S) -> parse_string(E).
parse_TransferStateType(E,_S) -> parse_int(E).
parse_TransferURL(E,_S) -> parse_string(E).
parse_UploadFileType(E,_S) -> parse_string(E).
parse_UserMessage(E,_S) -> parse_string(E).
parse_Username(E,_S) -> parse_string(E).
parse_Value(E) -> parse_string(E).
parse_Version(E,_S) -> parse_string(E).
parse_VoucherSN(E,_S) -> parse_unsignedInt(E).
parse_WindowEnd(E,_S) -> parse_unsignedInt(E).
parse_WindowStart(E,_S) -> parse_unsignedInt(E).
parse_Writable(E,_S) -> parse_boolean(E).

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
