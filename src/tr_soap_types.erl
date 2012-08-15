
-module(tr_soap_types).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-import(tr_soap_lib, [return_error/2,
		      get_QName/2]).

-spec parse_boolean(#xmlElement{content::[any()]}) -> boolean().
%FIXME: do exhausive test
parse_boolean(#xmlElement{name=Name, content = Content}) ->
    case lists:filter(fun tr_soap_lib:xmlText/1, Content) of
        []-> false;
        [#xmlText{value="1"} |_] -> true;
        [#xmlText{value="0"} |_] -> false;
        [#xmlText{value=Value} |_] -> return_error(Name, {Value, "Value"})
    end.

parse_dateTime(_E) -> ok.
parse_int(_E) -> ok.
parse_string(_E) -> ok.
parse_unsignedInt(_E) -> ok.
parse_anyURI(_E) -> ok.
    
parse_(_E) ->  ok.

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
parse_EventCodeType(E,_S) -> parse_string(E).
parse_ExecutionEnvRef(E,_S) -> parse_string(E).
parse_ExecutionUnitRefList(E,_S) -> parse_string(E).
parse_ExpirationDate(E,_S) -> parse_dateTime(E).
parse_FailureURL(E,_S) -> parse_string(E).
parse_FaultCode(E,_S) -> parse_(E).
parse_FaultString(E,_S) -> parse_string(E).
parse_FileSize(E,_S) -> parse_unsignedInt(E).
parse_InstanceNumber(E,_S) -> parse_unsignedInt(E).
parse_IsDownload(E,_S) -> parse_boolean(E).
parse_IsTransferable(E,_S) -> parse_int(E).
parse_Manufacturer(E,_S) -> parse_string(E).
parse_MaxEnvelopes(E,_S) -> parse_unsignedInt(E).
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
%    ?DBG(E),
    ?assertEqual(false, parse_boolean(E)),
    ok.


-endif.
