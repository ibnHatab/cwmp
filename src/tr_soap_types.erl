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
	  parse_FileType/2,
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
	  parse_attribete/3, % FIXME: attribete speling
	  parse_anySimpleType/1
	]).

-export([ build_anyURI/1
	]).


-import(tr_soap_lib, [return_error/2, parse_error/2, parse_error/3,
		      get_QName/2, local_ns/2, normalize_to_local_ns/2,
		      maybe_tag/3, maybe_tag/4
		     ]).


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
 
parse_base64(_E) -> %% FIXME: implement
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
parse_AnnounceURL(E)          -> parse_anyURI(E).
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
parse_FailureURL(E)           -> parse_anyURI(E).
parse_FaultString(E)          -> parse_string(E).
parse_FileSize(E)             -> parse_unsignedInt(E).
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
parse_NextURL(E)              -> parse_anyURI(E).
parse_NotificationChange(E)   -> parse_boolean(E).
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
parse_SuccessURL(E)           -> parse_anyURI(E).
parse_TargetFileName(E)       -> parse_string(E).
parse_TransferURL(E)          -> parse_anyURI(E).
parse_URL(E)                  -> parse_anyURI(E).
parse_UserMessage(E)          -> parse_string(E).
parse_Username(E)             -> parse_string(E).
parse_UUID(E)                 -> parse_string(E).
parse_Value(E)                -> parse_string(E).
parse_Version(E)              -> parse_string(E).
parse_VoucherSN(E)            -> parse_unsignedInt(E).
parse_WindowEnd(E)            -> parse_unsignedInt(E).

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
		{OpCode, _Rest} ->
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

parse_ArraySize(Value, ContentTag, Nss) ->
    case re:run(Value, "\(.*\)\\[\(.*\)\\]") of
	{match,[_All, {TagStart,TagLength},{DigitStart,DigitLength}]} ->
	    ValueStr = string:substr(Value, TagStart+1, TagLength),

	    {ValueName, ValueNs} = normalize_to_local_ns(list_to_atom(ValueStr), Nss),
	    {ContentName, ContentNs} = normalize_to_local_ns(ContentTag, Nss),
	    
	    if {ContentName, ContentNs} == {ValueName, ValueNs} ->
		    DigitStr = string:substr(Value, DigitStart+1, DigitLength),
		    case string:to_integer(DigitStr) of
			{error, Reason} ->
			    parse_error(Value, DigitStr, Reason);
			{Int, _Rest} ->
			    Int
		    end;    
	       true ->
		    parse_error(Value, ContentTag, "Bad Tag")
	    end;
	_ ->								
	    parse_error(Value, ContentTag, "Parse array size")
    end.

parse_XS_Array(Mapper, #xmlElement{content = Content} = E,
	       ContentTag, #parser{ns = Nss} = State) when is_function(Mapper) ->
    NsSoapEnc = local_ns('soap-enc', Nss),
    AttrName = get_QName('arrayType', NsSoapEnc),
    Value = parse_attribete(E, AttrName, string),
    Size = parse_ArraySize(Value, ContentTag, Nss),
    List = [Mapper(Elem, State) || Elem <- Content, tr_soap_lib:xmlElement(Elem)],
    if
	length(List) == Size ->
	    List;
	true ->
	    parse_error(E, State, "Array size")
    end.

parse_FileType('TransferFileType', E) ->
    parse_withSuportedValues(E, ?SUPPORTED_TRANSFER_FILE_TYPE);
parse_FileType('DownloadFileType', E) ->
    parse_withSuportedValues(E, ?SUPPORTED_DOWNLOAD_FILE_TYPE);
parse_FileType('UploadFileType', E) ->
    parse_withSuportedValues(E, ?SUPPORTED_UPLOAD_FILE_TYPE).
					      
parse_withSuportedValues(E, SuportedValues) ->
    String = parse_string(E),
    [Type | _Descr] = string:tokens(String, " "),
    Key = case string:to_integer(Type) of
	      {error, _Reason} ->
		  list_to_atom(Type);
	      {Int, _Rest} ->
		  Int
	  end,    
    case lists:keyfind(Key, 1, SuportedValues) of
	{K, _S} ->
	    K;
	false ->
	    parse_error(E, String, "Value")
    end.

parse_FaultCode(E) ->
    Code = parse_unsignedInt(E),
    case lists:keyfind(Code, 1, ?SUPPORTED_CPE_FAULT_CODES) of
	{K, _S} ->
	    K;
	false ->
	    parse_error(E, Code, "Fault Code")
    end.

parse_Notification(E) ->
    Value =parse_int(E),
    if
	0 =< Value, Value =< 6	->
	    Value;
	true -> 
	    parse_error(E, Value, "Notification Value")
    end.

parse_WindowMode(E) ->
    parse_withSuportedValues(E, ?SUPPORTED_TIME_WINDOW_MODE_VALUE_TYPE).


%%%-----------------------------------------------------------------------------
%% SOAP Type builder
%%%-----------------------------------------------------------------------------


format_string(Data) -> Data.
format_boolean(Data) -> atom_to_list(Data).
format_int(Data) ->
    integer_to_list(Data).
format_unsignedInt(Data) ->
    integer_to_list(Data).
format_dateTime(Data) ->
    Data. %FIXME: match parse
format_base64(Data) -> Data. %FIXME: match parse
     

-define(HTTP_DEFAULT_PORT, 80).
-define(FTP_DEFAULT_PORT, 21).

build_anyURI(Data) ->
    case Data of
	{http, Host,Port,Path,Query} ->
	    "http://" ++ build_HosPort(Host, Port, ?HTTP_DEFAULT_PORT) ++	       
		"/" ++ Path ++ "?" ++ Query;	%FIXME: normalize URI / build_host_port ?HTTP_DEFAULT_PORT
	{ftp, Creds,Host,Port,Path} -> 
	    "ftp://" ++	build_Credentials(Creds) ++ build_HosPort(Host, Port, ?FTP_DEFAULT_PORT) ++ "/" ++ Path;
	_ ->
	    return_error(Data, "Unknown schema")
	end.

build_HosPort(Host, Port, DefaultPort) ->
    if
	Port =:= DefaultPort ->
	    Host;
	true ->
	    Host ++ ":" ++ integer_to_list(Port)
    end.    

build_Credentials(Creds) -> %% FIXME: none case
    element(1, Creds) ++ "@" ++ element(2, Creds).

build_AccessListChange(Data)		-> maybe_tag('AccessListChange', fun format_boolean/1, Data).
build_CompleteTime(Data)		-> maybe_tag('CompleteTime', fun format_dateTime/1, Data).
build_CurrentTime(Data)			-> maybe_tag('CurrentTime', fun format_dateTime/1, Data).
build_DelaySeconds(Data)		-> maybe_tag('DelaySeconds', fun format_unsignedInt/1, Data).
build_DeploymentUnitRef(Data)		-> maybe_tag('DeploymentUnitRef', fun format_string/1, Data).
build_ExecutionUnitRefList(Data)	-> maybe_tag('ExecutionUnitRefList', fun format_string/1, Data).
build_ExpirationDate(Data)		-> maybe_tag('ExpirationDate', fun format_dateTime/1, Data).
build_FaultString(Data)			-> maybe_tag('FaultString', fun format_string/1, Data).
build_FileSize(Data)			-> maybe_tag('FileSize', fun format_unsignedInt/1, Data).
build_IsDownload(Data)			-> maybe_tag('IsDownload', fun format_boolean/1, Data).
build_MaxEnvelopes(Data)		-> maybe_tag('MaxEnvelopes', fun format_unsignedInt/1, Data).
build_MaxRetries(Data)			-> maybe_tag('MaxRetries', fun format_int/1, Data).
build_NextLevel(Data)			-> maybe_tag('NextLevel', fun format_boolean/1, Data).
build_NotificationChange(Data)		-> maybe_tag('NotificationChange', fun format_boolean/1, Data).
build_ParameterName(Data)		-> maybe_tag('ParameterName', fun format_string/1, Data).
build_Resolved(Data)			-> maybe_tag('Resolved', fun format_boolean/1, Data).
build_RetryCount(Data)			-> maybe_tag('RetryCount', fun format_unsignedInt/1, Data).
build_StartDate(Data)			-> maybe_tag('StartDate', fun format_dateTime/1, Data).
build_StartTime(Data)			-> maybe_tag('StartTime', fun format_dateTime/1, Data).
build_VoucherSN(Data)			-> maybe_tag('VoucherSN', fun format_unsignedInt/1, Data).
build_WindowEnd(Data)			-> maybe_tag('WindowEnd', fun format_unsignedInt/1, Data).
build_WindowStart(Data)			-> maybe_tag('WindowStart', fun format_unsignedInt/1, Data).
build_Writable(Data)			-> maybe_tag('Writable', fun format_boolean/1, Data).

%
build_AnnounceURL(Data)			-> maybe_tag('AnnounceURL', fun format_string/1, Data).
build_Arg(Data)				-> maybe_tag('Arg', fun format_string/1, Data).
build_Command(Data)			-> maybe_tag('Command', fun format_string/1, Data).
build_ExecutionEnvRef(Data)		-> maybe_tag('ExecutionEnvRef', fun format_string/1, Data).
build_FailureURL(Data)			-> maybe_tag('FailureURL', fun format_string/1, Data).
build_FaultString(Data)			-> maybe_tag('FaultString', fun format_string/1, Data).
build_InstanceNumber(Data)		-> maybe_tag('InstanceNumber', fun format_unsignedInt/1, Data).
build_IsTransferable(Data)		-> maybe_tag('IsTransferable', fun format_int/1, Data).
build_Manufacturer(Data)		-> maybe_tag('Manufacturer', fun format_string/1, Data).
build_Mode(Data)			-> maybe_tag('Mode', fun format_int/1, Data).
build_Name(Data)			-> maybe_tag('Name', fun format_string/1, Data).
build_Next(Data)			-> maybe_tag('Next', fun format_string/1, Data).
build_NextURL(Data)			-> maybe_tag('NextURL', fun format_string/1, Data).
build_OptionName(Data)			-> maybe_tag('OptionName', fun format_string/1, Data).
build_OUI(Data)				-> maybe_tag('OUI', fun format_string/1, Data).
build_ParameterPath(Data)		-> maybe_tag('ParameterPath', fun format_string/1, Data).
build_Password(Data)			-> maybe_tag('Password', fun format_string/1, Data).
build_ProductClass(Data)		-> maybe_tag('ProductClass', fun format_string/1, Data).
build_Referer(Data)			-> maybe_tag('Referer', fun format_string/1, Data).
build_SerialNumber(Data)		-> maybe_tag('SerialNumber', fun format_string/1, Data).
build_State(Data)			-> maybe_tag('State', fun format_unsignedInt/1, Data).
build_Status(Data)			-> maybe_tag('Status', fun format_int/1, Data).
build_string(Data)			-> maybe_tag('string', fun format_string/1, Data).
build_SuccessURL(Data)			-> maybe_tag('SuccessURL', fun format_string/1, Data).
build_TargetFileName(Data)		-> maybe_tag('TargetFileName', fun format_string/1, Data).
build_TransferURL(Data)			-> maybe_tag('TransferURL', fun format_string/1, Data).
build_URL(Data)				-> maybe_tag('URL', fun format_string/1, Data).
build_UserMessage(Data)			-> maybe_tag('UserMessage', fun format_string/1, Data).
build_Username(Data)			-> maybe_tag('Username', fun format_string/1, Data).
build_Value(Data)			-> maybe_tag('Value', fun format_string/1, Data).
build_Version(Data)			-> maybe_tag('Version', fun format_string/1, Data).

%% end

%%%-----------------------------------------------------------------------------
%%% Unitary tetsts
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

make_Element(Name, Text) when is_atom(Name) ->
    QName = get_QName(Name, 'cwmp'),
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
    E = make_Element('NoMoreRequests', "0"),
    ?assertEqual(false, parse_boolean(E)),
    ok.

parse_int_test() ->
    E = make_Element('NoMoreRequests', "42"),
    ?assertEqual(42, parse_int(E)),
    ok.


parse_iso8601_test() ->
    [
     begin
%	 ?DBG({DT, Str, convert_iso8601_date(Str)}),
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


parse_anyURI_test() ->
    E = make_Element('URL', "http://cpe-host-name/kick.html?command=cmd&arg=1&next=home"),
    ?assertEqual({http,"cpe-host-name",80,"/kick.html","?command=cmd&arg=1&next=home"},
		 parse_anyURI(E)).

-define(TEST_URI_BUILDER,
	[
	 "http://cpe-host-name.com/kick.html?command=cmd&arg=1&next=home",
	 "ftp://user:pass@cpe-host-name.com:21/kick.pcap"
	]
       ).


build_anyURI_test() ->
    [
     begin
	 Data = parse_anyURI(make_Element('URL', URI)),
	 ?DBG({URI, Data}),
	 NewURI = build_anyURI(Data),
	 ?assertEqual(URI, NewURI)
     end ||  URI <- ?TEST_URI_BUILDER
    ].


-endif.


