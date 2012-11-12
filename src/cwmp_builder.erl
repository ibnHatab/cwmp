%%% File    : cwmp_builder.erl
%%% Description :

-module(cwmp_builder).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69/include/tr69.hrl").
-include("cwmp.hrl").

-export([builder/1, build/1]).

-import(cwmp_lib, [ build_error/2,
		       maybe_tag/3, maybe_tag/4
		     ]).

-import(cwmp_types, [ build_AccessListChange/1,
			 build_CompleteTime/1,
			 build_CurrentTime/1,
			 build_DelaySeconds/1,
			 build_DeploymentUnitRef/1,
			 build_ExecutionUnitRefList/1,
			 build_ExpirationDate/1,
			 build_FaultString/1,
			 build_FileSize/1,
			 build_IsDownload/1,
			 build_MaxEnvelopes/1,
			 build_MaxRetries/1,
			 build_NextLevel/1,
			 build_NotificationChange/1,
			 build_ParameterName/1,
			 build_Resolved/1,
			 build_RetryCount/1,
			 build_StartDate/1,
			 build_StartTime/1,
			 build_VoucherSN/1,
			 build_WindowEnd/1,
			 build_WindowStart/1,
			 build_Writable/1,

			 build_AnnounceURL/1,
			 build_Arg/1,
			 build_Command/1,
			 build_ExecutionEnvRef/1,
			 build_FailureURL/1,
			 build_InstanceNumber/1,
			 build_IsTransferable/1,
			 build_Manufacturer/1,
			 build_Mode/1,
			 build_Name/1,
			 build_Next/1,
			 build_NextURL/1,
			 build_OptionName/1,
			 build_OUI/1,
			 build_ParameterPath/1,
			 build_Password/1,
			 build_ProductClass/1,
			 build_Referer/1,
			 build_SerialNumber/1,
			 build_State/1,
			 build_Status/1,
			 build_string/1,
			 build_SuccessURL/1,
			 build_TargetFileName/1,
			 build_TransferURL/1,
			 build_URL/1,
			 build_UserMessage/1,
			 build_Username/1,
			 build_Value/1,
			 build_Version/1,

			 build_DownloadFileType/1,
			 build_EventCodeType/1,
			 build_TimeWindowModeValueType/1,
			 build_CommandKeyType/1,
			 build_ObjectNameType/1,
			 build_ParameterKeyType/1,
			 %% build_AccessListValueType/1,
			 build_ParameterAttributeNotificationValueType/1,
			 %% build_TransferStateType/1,
			 build_DeploymentUnitUUID/1,
			 build_DeploymentUnitState/1,
			 %% build_DefaultDeploymentUnitOperationType/1,


			 %% Missed types
			 build_FaultCode/1,
			 build_base64/1
		       ]).



%%%-----------------------------------------------------------------------------
%%%        SOAP Encoder
%%%-----------------------------------------------------------------------------


%% @doc Create an buildr/1 with the given options.
-spec builder([builder_option()]) -> function().
builder(Options) ->
    State = parse_builder_options(Options, #builder{}),
    fun (O) -> build_cwmp_obj(O, State) end.

%% @doc Build the given as SOAP to an cwmp_obj.
-spec build(#cwmp_obj{}) -> [export_element()].
build(Any) ->
    build_cwmp_obj(Any, #builder{}).


%%%-----------------------------------------------------------------------------
%% Internal API
%%%-----------------------------------------------------------------------------

parse_builder_options([], State) ->
    State;
parse_builder_options([{version, Version} | Rest], State) ->
    parse_builder_options(Rest, State#builder{version=Version});
parse_builder_options([{handler, Handler} | Rest], State) ->
    parse_builder_options(Rest, State#builder{handler=Handler});
parse_builder_options([{namespaces, Nss} | Rest], State) ->
    parse_builder_options(Rest, State#builder{ns=Nss}).

%%%-----------------------------------------------------------------------------
%%%        Build SOAP Envelope
%%%-----------------------------------------------------------------------------



-spec build_cwmp_obj(#cwmp_obj{}, #builder{}) ->  [export_element()].
build_cwmp_obj(#cwmp_obj{data=Data} = _D, State) ->
						%    ?DBG(Data),
    [build_Envelope(Data, State)].

-spec build_Envelope(#envelope{}, #builder{}) ->  export_element().
build_Envelope(#envelope{header=Header, body=Body} = _D, State) ->
    ElHeader = build_Header(Header, State),
    ElBody = build_Body(Body, State),
    Attribs = [{'xmlns:soapenv', 'http://schemas.xmlsoap.org/soap/envelope/'},
	       {'xmlns:soapenc', 'http://schemas.xmlsoap.org/soap/encoding/'},
	       {'xmlns:xsd', 'http://www.w3.org/2001/XMLSchema'},
	       {'xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance'},
	       {'xmlns:cwmp', 'urn:dslforum-org:cwmp-1-0'}],
    {'soapenv:Envelope', Attribs, [ElHeader, ElBody]}.


-spec build_Header(#header{}, #builder{}) ->  export_element().
build_Header(_D, _State) ->
    'soapenv:Header'.

-spec build_Body(body_type(), #builder{}) ->  export_element().
build_Body(Body, State) ->
    {'soapenv:Body', [],
     [case Method of
	  #fault{} = Data -> build_Fault(Data, State);
	  #get_rpc_methods{} = Data -> build_TagName(Data, "GetRPCMethods");
	  #get_rpc_methods_response{} = Data -> build_GetRPCMethodsResponse(Data, State);
	  #set_parameter_values{} = Data -> build_SetParameterValues(Data, State);
	  #set_parameter_values_response{} = Data -> build_SetParameterValuesResponse(Data, State);
	  #get_parameter_values{} = Data -> build_GetParameterValues(Data, State);
	  #get_parameter_values_response{} = Data -> build_GetParameterValuesResponse(Data, State);
	  #get_parameter_names{} = Data -> build_GetParameterNames(Data, State);
	  #get_parameter_names_response{} = Data -> build_GetParameterNamesResponse(Data, State);
	  #set_parameter_attributes{} = Data -> build_SetParameterAttributes(Data, State);
	  #set_parameter_attributes_response{} = Data -> build_TagName(Data, "SetParameterAttributesResponse");
	  #get_parameter_attributes{} = Data -> build_GetParameterAttributes(Data, State);
	  #get_parameter_attributes_response{} = Data -> build_GetParameterAttributesResponse(Data, State);
	  #add_object{} = Data -> build_AddObject(Data, State);
	  #add_object_response{} = Data -> build_AddObjectResponse(Data, State);
	  #delete_object{} = Data -> build_DeleteObject(Data, State);
	  #delete_object_response{} = Data -> build_DeleteObjectResponse(Data, State);
	  #download{} = Data -> build_Download(Data, State);
	  #download_response{} = Data -> build_DownloadResponse(Data, State);
	  #reboot{} = Data -> build_Reboot(Data, State);
	  #reboot_response{} = Data -> build_TagName(Data, "RebootResponse");
	  #get_queued_transfers{} = Data -> build_TagName(Data, "GetQueuedTransfers");
	  #get_queued_transfers_response{} = Data -> build_GetQueuedTransfersResponse(Data, State);
	  #schedule_inform{} = Data -> build_ScheduleInform(Data, State);
	  #schedule_inform_response{} = Data -> build_TagName(Data, "ScheduleInformResponse");
	  #set_vouchers{} = Data -> build_SetVouchers(Data, State);
	  #set_vouchers_response{} = Data -> build_TagName(Data, "SetVouchersResponse");
	  #get_options{} = Data -> build_GetOptions(Data, State);
	  #get_options_response{} = Data -> build_GetOptionsResponse(Data, State);
	  #upload{} = Data -> build_Upload(Data, State);
	  #upload_response{} = Data -> build_UploadResponse(Data, State);
	  #factory_reset{} = Data -> build_TagName(Data, "FactoryReset");
	  #factory_reset_response{} = Data -> build_TagName(Data, "FactoryResetResponse");
	  #get_all_queued_transfers{} = Data -> build_TagName(Data, "GetAllQueuedTransfers");
	  #get_all_queued_transfers_response{} = Data -> build_GetAllQueuedTransfersResponse(Data, State);
	  #schedule_download{} = Data -> build_ScheduleDownload(Data, State);
	  #schedule_download_response{} = Data -> build_TagName(Data, "ScheduleDownloadResponse");
	  #cancel_transfer{} = Data -> build_CancelTransfer(Data, State);
	  #cancel_transfer_response{} = Data -> build_TagName(Data, "CancelTransferResponse");
	  #change_du_state{} = Data -> build_ChangeDUState(Data, State);
	  #change_du_state_response{} = Data -> build_TagName(Data, "ChangeDUStateResponse");
	  #inform{} = Data -> build_Inform(Data, State);
	  #inform_response{} = Data -> build_InformResponse(Data, State);
	  #transfer_complete{} = Data -> build_TransferComplete(Data, State);
	  #transfer_complete_response{} = Data -> build_TagName(Data, "TransferCompleteResponse");
	  #autonomous_transfer_complete{} = Data -> build_AutonomousTransferComplete(Data, State);
	  #autonomous_transfer_complete_response{} = Data -> build_TagName(Data, "AutonomousTransferCompleteResponse");
	  #kicked{} = Data -> build_Kicked(Data, State);
	  #kicked_response{} = Data -> build_KickedResponse(Data, State);
	  #request_download{} = Data -> build_RequestDownload(Data, State);
	  #request_download_response{} = Data -> build_TagName(Data, "RequestDownloadResponse");
	  #du_state_change_complete{} = Data -> build_DUStateChangeComplete(Data, State);
	  #du_state_change_complete_response{} = Data -> build_TagName(Data, "DUStateChangeCompleteResponse");
	  #autonomous_du_state_change_complete{} = Data -> build_AutonomousDUStateChangeComplete(Data, State);
	  #autonomous_du_state_change_complete_response{} = Data -> build_TagName(Data, "AutonomousDUStateChangeCompleteResponse");
	  _ ->
	      build_error(Method, State)
      end || Method <- Body]}.


%%%-----------------------------------------------------------------------------
%%%        Build SOAP Fault
%%%-----------------------------------------------------------------------------

-spec build_SoapFault(#soap_fault{}, #builder{}) -> export_element().
build_SoapFault(Data, State) ->
    {'soapenv:Fault', [],
     [P || P <- [
		 maybe_tag('faultcode', fun cwmp_types:format_string/1, Data#soap_fault.faultcode),
		 maybe_tag('faultstring', fun cwmp_types:format_string/1, Data#soap_fault.faultstring),
		 maybe_tag('faultactor', fun cwmp_types:build_anyURI/1, Data#soap_fault.faultactor),
		 maybe_tag('detail', fun build_Fault/2, Data#soap_fault.detail, State)
		], P /= null]}.



%%%-----------------------------------------------------------------------------
%%% Complex element
%%%-----------------------------------------------------------------------------

%% Type aliases
build_ParameterList(Data, State) -> build_ParameterValueList(Data, State).
build_ParameterKey(Data, _State) -> build_ParameterKeyType(Data).
build_ObjectName(Data, _State) -> build_ObjectNameType(Data).
build_CommandKey(Data, _State) ->  build_CommandKeyType(Data).
build_FileType(Data, _State) ->  build_DownloadFileType(Data).
build_UUID(Data) ->  build_DeploymentUnitUUID(Data).
build_DeviceId(Data, State) -> build_DeviceIdStruct(Data, State).
build_Event(Data, State) -> build_EventList(Data, State).
build_FaultStruct(Data, State) -> build_TransferCompleteFaultStruct(Data, State).
build_EventCode(Data, _State) -> build_EventCodeType(Data).
build_Notification(Data, _State) -> build_ParameterAttributeNotificationValueType(Data).
build_WindowMode(Data, _State) -> build_TimeWindowModeValueType(Data).
build_CurrentState(Data, _State) -> build_DeploymentUnitState(Data).
build_OperationPerformed(Data, _State) -> build_string(Data).

%%%-----------------------------------------------------------------------------
%%%        Build RPC Message
%%%-----------------------------------------------------------------------------

%% Build empty tags
-spec build_TagName(record(), string()) -> export_element().
build_TagName(Data, _TagName) when Data =:= undefined -> null;
build_TagName(_Data, TagName) -> { list_to_atom("cwmp:" ++ TagName), [], []}.

%% generated elements

-spec build_Fault(#fault{}, #builder{}) -> export_element().
build_Fault(Data, _S) when Data =:= undefined -> null;
build_Fault(Data, State) ->
    {'cwmp:Fault', [],
     [P || P <- [
		 build_FaultCode(Data#fault.fault_code),
		 build_FaultString(Data#fault.fault_string),
		 build_SetParameterValuesFault(Data#fault.set_parameter_values_fault, State)
		], P /= null]}.

-spec build_SetParameterValuesFault(#set_parameter_values_fault{}, #builder{}) -> export_element().
build_SetParameterValuesFault(Data, _State) when Data =:= undefined -> null;
build_SetParameterValuesFault(Data, _State) ->
    {'SetParameterValuesFault', [],
     [P || P <- [
		 build_ParameterName(Data#set_parameter_values_fault.parameter_name),
		 build_FaultCode(Data#set_parameter_values_fault.fault_code),
		 build_FaultString(Data#set_parameter_values_fault.fault_string)
		], P /= null]}.


-spec build_GetRPCMethodsResponse(#get_rpc_methods_response{}, #builder{}) -> export_element().
build_GetRPCMethodsResponse(Data, _S) when Data =:= undefined -> null;
build_GetRPCMethodsResponse(Data, State) ->
    {'cwmp:GetRPCMethodsResponse', [],
     [P || P <- [
		 build_MethodList(Data#get_rpc_methods_response.method_list, State)
		], P /= null]}.

-spec build_SetParameterValues(#set_parameter_values{}, #builder{}) -> export_element().
build_SetParameterValues(Data, _S) when Data =:= undefined -> null;
build_SetParameterValues(Data, State) ->
    {'cwmp:SetParameterValues', [],
     [P || P <- [
		 build_ParameterList(Data#set_parameter_values.parameter_list, State),
		 build_ParameterKey(Data#set_parameter_values.parameter_key, State)
		], P /= null]}.

-spec build_SetParameterValuesResponse(#set_parameter_values_response{}, #builder{}) -> export_element().
build_SetParameterValuesResponse(Data, _S) when Data =:= undefined -> null;
build_SetParameterValuesResponse(Data, _State) ->
    {'cwmp:SetParameterValuesResponse', [],
     [P || P <- [
		 build_Status(Data#set_parameter_values_response.status)
		], P /= null]}.

-spec build_GetParameterValues(#get_parameter_values{}, #builder{}) -> export_element().
build_GetParameterValues(Data, _S) when Data =:= undefined -> null;
build_GetParameterValues(Data, State) ->
    {'cwmp:GetParameterValues', [],
     [P || P <- [
		 build_ParameterNames(Data#get_parameter_values.parameter_names, State)
		], P /= null]}.

-spec build_GetParameterValuesResponse(#get_parameter_values_response{}, #builder{}) -> export_element().
build_GetParameterValuesResponse(Data, _S) when Data =:= undefined -> null;
build_GetParameterValuesResponse(Data, State) ->
    {'cwmp:GetParameterValuesResponse', [],
     [P || P <- [
		 build_ParameterList(Data#get_parameter_values_response.parameter_list, State)
		], P /= null]}.

-spec build_GetParameterNames(#get_parameter_names{}, #builder{}) -> export_element().
build_GetParameterNames(Data, _S) when Data =:= undefined -> null;
build_GetParameterNames(Data, _State) ->
    {'cwmp:GetParameterNames', [],
     [P || P <- [
		 build_ParameterPath(Data#get_parameter_names.parameter_path),
		 build_NextLevel(Data#get_parameter_names.next_level)
		], P /= null]}.

-spec build_GetParameterNamesResponse(#get_parameter_names_response{}, #builder{}) -> export_element().
build_GetParameterNamesResponse(Data, _S) when Data =:= undefined -> null;
build_GetParameterNamesResponse(Data, State) ->
    {'cwmp:GetParameterNamesResponse', [],
     [P || P <- [
		 build_ParameterList(Data#get_parameter_names_response.parameter_list, State)
		], P /= null]}.

-spec build_SetParameterAttributes(#set_parameter_attributes{}, #builder{}) -> export_element().
build_SetParameterAttributes(Data, _S) when Data =:= undefined -> null;
build_SetParameterAttributes(Data, State) ->
    {'cwmp:SetParameterAttributes', [],
     [P || P <- [
		 build_ParameterList(Data#set_parameter_attributes.parameter_list, State)
		], P /= null]}.

-spec build_GetParameterAttributes(#get_parameter_attributes{}, #builder{}) -> export_element().
build_GetParameterAttributes(Data, _S) when Data =:= undefined -> null;
build_GetParameterAttributes(Data, State) ->
    {'cwmp:GetParameterAttributes', [],
     [P || P <- [
		 build_ParameterNames(Data#get_parameter_attributes.parameter_names, State)
		], P /= null]}.

-spec build_GetParameterAttributesResponse(#get_parameter_attributes_response{}, #builder{}) -> export_element().
build_GetParameterAttributesResponse(Data, _S) when Data =:= undefined -> null;
build_GetParameterAttributesResponse(Data, State) ->
    {'cwmp:GetParameterAttributesResponse', [],
     [P || P <- [
		 build_ParameterList(Data#get_parameter_attributes_response.parameter_list, State)
		], P /= null]}.

-spec build_AddObject(#add_object{}, #builder{}) -> export_element().
build_AddObject(Data, _S) when Data =:= undefined -> null;
build_AddObject(Data, State) ->
    {'cwmp:AddObject', [],
     [P || P <- [
		 build_ObjectName(Data#add_object.object_name, State),
		 build_ParameterKey(Data#add_object.parameter_key, State)
		], P /= null]}.

-spec build_AddObjectResponse(#add_object_response{}, #builder{}) -> export_element().
build_AddObjectResponse(Data, _S) when Data =:= undefined -> null;
build_AddObjectResponse(Data, _State) ->
    {'cwmp:AddObjectResponse', [],
     [P || P <- [
		 build_InstanceNumber(Data#add_object_response.instance_number),
		 build_Status(Data#add_object_response.status)
		], P /= null]}.

-spec build_DeleteObject(#delete_object{}, #builder{}) -> export_element().
build_DeleteObject(Data, _S) when Data =:= undefined -> null;
build_DeleteObject(Data, State) ->
    {'cwmp:DeleteObject', [],
     [P || P <- [
		 build_ObjectName(Data#delete_object.object_name, State),
		 build_ParameterKey(Data#delete_object.parameter_key, State)
		], P /= null]}.

-spec build_DeleteObjectResponse(#delete_object_response{}, #builder{}) -> export_element().
build_DeleteObjectResponse(Data, _S) when Data =:= undefined -> null;
build_DeleteObjectResponse(Data, _State) ->
    {'cwmp:DeleteObjectResponse', [],
     [P || P <- [
		 build_Status(Data#delete_object_response.status)
		], P /= null]}.

-spec build_Download(#download{}, #builder{}) -> export_element().
build_Download(Data, _S) when Data =:= undefined -> null;
build_Download(Data, State) ->
    {'cwmp:Download', [],
     [P || P <- [
		 build_CommandKey(Data#download.command_key, State),
		 build_FileType(Data#download.file_type, State),
		 build_URL(Data#download.url),
		 build_Username(Data#download.username),
		 build_Password(Data#download.password),
		 build_FileSize(Data#download.file_size),
		 build_TargetFileName(Data#download.target_file_name),
		 build_DelaySeconds(Data#download.delay_seconds),
		 build_SuccessURL(Data#download.success_url),
		 build_FailureURL(Data#download.failure_url)
		], P /= null]}.

-spec build_DownloadResponse(#download_response{}, #builder{}) -> export_element().
build_DownloadResponse(Data, _S) when Data =:= undefined -> null;
build_DownloadResponse(Data, _State) ->
    {'cwmp:DownloadResponse', [],
     [P || P <- [
		 build_Status(Data#download_response.status),
		 build_StartTime(Data#download_response.start_time),
		 build_CompleteTime(Data#download_response.complete_time)
		], P /= null]}.

-spec build_Reboot(#reboot{}, #builder{}) -> export_element().
build_Reboot(Data, _S) when Data =:= undefined -> null;
build_Reboot(Data, State) ->
    {'cwmp:Reboot', [],
     [P || P <- [
		 build_CommandKey(Data#reboot.command_key, State)
		], P /= null]}.

-spec build_GetQueuedTransfersResponse(#get_queued_transfers_response{}, #builder{}) -> export_element().
build_GetQueuedTransfersResponse(Data, _S) when Data =:= undefined -> null;
build_GetQueuedTransfersResponse(Data, State) ->
    {'cwmp:GetQueuedTransfersResponse', [],
     [P || P <- [
		 build_TransferList(Data#get_queued_transfers_response.transfer_list, State)
		], P /= null]}.

-spec build_ScheduleInform(#schedule_inform{}, #builder{}) -> export_element().
build_ScheduleInform(Data, _S) when Data =:= undefined -> null;
build_ScheduleInform(Data, State) ->
    {'cwmp:ScheduleInform', [],
     [P || P <- [
		 build_DelaySeconds(Data#schedule_inform.delay_seconds),
		 build_CommandKey(Data#schedule_inform.command_key, State)
		], P /= null]}.

-spec build_SetVouchers(#set_vouchers{}, #builder{}) -> export_element().
build_SetVouchers(Data, _S) when Data =:= undefined -> null;
build_SetVouchers(Data, State) ->
    {'cwmp:SetVouchers', [],
     [P || P <- [
		 build_VoucherList(Data#set_vouchers.voucher_list, State)
		], P /= null]}.

-spec build_GetOptions(#get_options{}, #builder{}) -> export_element().
build_GetOptions(Data, _S) when Data =:= undefined -> null;
build_GetOptions(Data, _State) ->
    {'cwmp:GetOptions', [],
     [P || P <- [
		 build_OptionName(Data#get_options.option_name)
		], P /= null]}.

-spec build_GetOptionsResponse(#get_options_response{}, #builder{}) -> export_element().
build_GetOptionsResponse(Data, _S) when Data =:= undefined -> null;
build_GetOptionsResponse(Data, State) ->
    {'cwmp:GetOptionsResponse', [],
     [P || P <- [
		 build_OptionList(Data#get_options_response.option_list, State)
		], P /= null]}.

-spec build_Upload(#upload{}, #builder{}) -> export_element().
build_Upload(Data, _S) when Data =:= undefined -> null;
build_Upload(Data, State) ->
    {'cwmp:Upload', [],
     [P || P <- [
		 build_CommandKey(Data#upload.command_key, State),
		 build_FileType(Data#upload.file_type, State),
		 build_URL(Data#upload.url),
		 build_Username(Data#upload.username),
		 build_Password(Data#upload.password),
		 build_DelaySeconds(Data#upload.delay_seconds)
		], P /= null]}.

-spec build_UploadResponse(#upload_response{}, #builder{}) -> export_element().
build_UploadResponse(Data, _S) when Data =:= undefined -> null;
build_UploadResponse(Data, _State) ->
    {'cwmp:UploadResponse', [],
     [P || P <- [
		 build_Status(Data#upload_response.status),
		 build_StartTime(Data#upload_response.start_time),
		 build_CompleteTime(Data#upload_response.complete_time)
		], P /= null]}.

-spec build_GetAllQueuedTransfersResponse(#get_all_queued_transfers_response{}, #builder{}) -> export_element().
build_GetAllQueuedTransfersResponse(Data, _S) when Data =:= undefined -> null;
build_GetAllQueuedTransfersResponse(Data, State) ->
    {'cwmp:GetAllQueuedTransfersResponse', [],
     [P || P <- [
		 build_TransferList(Data#get_all_queued_transfers_response.transfer_list, State)
		], P /= null]}.

-spec build_ScheduleDownload(#schedule_download{}, #builder{}) -> export_element().
build_ScheduleDownload(Data, _S) when Data =:= undefined -> null;
build_ScheduleDownload(Data, State) ->
    {'cwmp:ScheduleDownload', [],
     [P || P <- [
		 build_CommandKey(Data#schedule_download.command_key, State),
		 build_FileType(Data#schedule_download.file_type, State),
		 build_URL(Data#schedule_download.url),
		 build_Username(Data#schedule_download.username),
		 build_Password(Data#schedule_download.password),
		 build_FileSize(Data#schedule_download.file_size),
		 build_TargetFileName(Data#schedule_download.target_file_name),
		 build_TimeWindowList(Data#schedule_download.time_window_list, State)
		], P /= null]}.

-spec build_CancelTransfer(#cancel_transfer{}, #builder{}) -> export_element().
build_CancelTransfer(Data, _S) when Data =:= undefined -> null;
build_CancelTransfer(Data, State) ->
    {'cwmp:CancelTransfer', [],
     [P || P <- [
		 build_CommandKey(Data#cancel_transfer.command_key, State)
		], P /= null]}.

-spec build_ChangeDUState(#change_du_state{}, #builder{}) -> export_element().
build_ChangeDUState(Data, _S) when Data =:= undefined -> null;
build_ChangeDUState(Data, State) ->
    {'cwmp:ChangeDUState', [],
     [P || P <- [
		 %%FIXME		 build_Operations(Data#change_du_state.operations, State),
		 build_CommandKey(Data#change_du_state.command_key, State)
		], P /= null]}.

-spec build_Inform(#inform{}, #builder{}) -> export_element().
build_Inform(Data, _S) when Data =:= undefined -> null;
build_Inform(Data, State) ->
    {'cwmp:Inform', [],
     [P || P <- [
		 build_DeviceId(Data#inform.device_id, State),
		 build_Event(Data#inform.event, State),
		 build_MaxEnvelopes(Data#inform.max_envelopes),
		 build_CurrentTime(Data#inform.current_time),
		 build_RetryCount(Data#inform.retry_count),
		 build_ParameterList(Data#inform.parameter_list, State)
		], P /= null]}.

-spec build_InformResponse(#inform_response{}, #builder{}) -> export_element().
build_InformResponse(Data, _S) when Data =:= undefined -> null;
build_InformResponse(Data, _State) ->
    {'cwmp:InformResponse', [],
     [P || P <- [
		 build_MaxEnvelopes(Data#inform_response.max_envelopes)
		], P /= null]}.

-spec build_TransferComplete(#transfer_complete{}, #builder{}) -> export_element().
build_TransferComplete(Data, _S) when Data =:= undefined -> null;
build_TransferComplete(Data, State) ->
    {'cwmp:TransferComplete', [],
     [P || P <- [
		 build_CommandKey(Data#transfer_complete.command_key, State),
		 build_FaultStruct(Data#transfer_complete.fault_struct, State),
		 build_StartTime(Data#transfer_complete.start_time),
		 build_CompleteTime(Data#transfer_complete.complete_time)
		], P /= null]}.

-spec build_AutonomousTransferComplete(#autonomous_transfer_complete{}, #builder{}) -> export_element().
build_AutonomousTransferComplete(Data, _S) when Data =:= undefined -> null;
build_AutonomousTransferComplete(Data, State) ->
    {'cwmp:AutonomousTransferComplete', [],
     [P || P <- [
		 build_AnnounceURL(Data#autonomous_transfer_complete.announce_url),
		 build_TransferURL(Data#autonomous_transfer_complete.transfer_url),
		 build_IsDownload(Data#autonomous_transfer_complete.is_download),
		 build_FileType(Data#autonomous_transfer_complete.file_type, State),
		 build_FileSize(Data#autonomous_transfer_complete.file_size),
		 build_TargetFileName(Data#autonomous_transfer_complete.target_file_name),
		 build_FaultStruct(Data#autonomous_transfer_complete.fault_struct, State),
		 build_StartTime(Data#autonomous_transfer_complete.start_time),
		 build_CompleteTime(Data#autonomous_transfer_complete.complete_time)
		], P /= null]}.

-spec build_Kicked(#kicked{}, #builder{}) -> export_element().
build_Kicked(Data, _S) when Data =:= undefined -> null;
build_Kicked(Data, _State) ->
    {'cwmp:Kicked', [],
     [P || P <- [
		 build_Command(Data#kicked.command),
		 build_Referer(Data#kicked.referer),
		 build_Arg(Data#kicked.arg),
		 build_Next(Data#kicked.next)
		], P /= null]}.

-spec build_KickedResponse(#kicked_response{}, #builder{}) -> export_element().
build_KickedResponse(Data, _S) when Data =:= undefined -> null;
build_KickedResponse(Data, _State) ->
    {'cwmp:KickedResponse', [],
     [P || P <- [
		 build_NextURL(Data#kicked_response.next_url)
		], P /= null]}.

-spec build_RequestDownload(#request_download{}, #builder{}) -> export_element().
build_RequestDownload(Data, _S) when Data =:= undefined -> null;
build_RequestDownload(Data, State) ->
    {'cwmp:RequestDownload', [],
     [P || P <- [
		 build_FileType(Data#request_download.file_type, State),
		 build_FileTypeArg(Data#request_download.file_type_arg, State)
		], P /= null]}.

-spec build_DUStateChangeComplete(#du_state_change_complete{}, #builder{}) -> export_element().
build_DUStateChangeComplete(Data, _S) when Data =:= undefined -> null;
build_DUStateChangeComplete(Data, State) ->
    {'cwmp:DUStateChangeComplete', [],
     [P || P <- [
		 build_OpResultStruct(Data#du_state_change_complete.results, State),
		 build_CommandKey(Data#du_state_change_complete.command_key, State)
		], P /= null]}.

-spec build_AutonomousDUStateChangeComplete(#autonomous_du_state_change_complete{}, #builder{}) -> export_element().
build_AutonomousDUStateChangeComplete(Data, _S) when Data =:= undefined -> null;
build_AutonomousDUStateChangeComplete(Data, State) ->
    {'cwmp:AutonomousDUStateChangeComplete', [],
     [P || P <- [
		 build_AutonOpResultStruct(Data#autonomous_du_state_change_complete.results, State)
		], P /= null]}.

%% complexType/sequence

-spec build_TransferCompleteFaultStruct(#transfer_complete_fault_struct{}, #builder{}) -> export_element().
build_TransferCompleteFaultStruct(Data, _S) when Data =:= undefined -> null;
build_TransferCompleteFaultStruct(Data, _State) ->
    {'cwmp:TransferCompleteFaultStruct', [],
     [P || P <- [
		 build_FaultCode(Data#transfer_complete_fault_struct.fault_code),
		 build_FaultString(Data#transfer_complete_fault_struct.fault_string)
		], P /= null]}.

-spec build_DeploymentUnitFaultStruct(#deployment_unit_fault_struct{}, #builder{}) -> export_element().
build_DeploymentUnitFaultStruct(Data, _S) when Data =:= undefined -> null;
build_DeploymentUnitFaultStruct(Data, _State) ->
    {'cwmp:DeploymentUnitFaultStruct', [],
     [P || P <- [
		 build_FaultCode(Data#deployment_unit_fault_struct.fault_code),
		 build_FaultString(Data#deployment_unit_fault_struct.fault_string)
		], P /= null]}.


-spec build_ParameterValueStruct(#parameter_value_struct{}, #builder{}) -> export_element().
build_ParameterValueStruct(Data, _S) when Data =:= undefined -> null;
build_ParameterValueStruct(Data, _State) ->
    {'cwmp:ParameterValueStruct', [],
     [P || P <- [
		 build_Name(Data#parameter_value_struct.name),
		 build_Value(Data#parameter_value_struct.value)
		], P /= null]}.

-spec build_DeviceIdStruct(#device_id_struct{}, #builder{}) -> export_element().
build_DeviceIdStruct(Data, _S) when Data =:= undefined -> null;
build_DeviceIdStruct(Data, _State) ->
    {'cwmp:DeviceIdStruct', [],
     [P || P <- [
		 build_Manufacturer(Data#device_id_struct.manufacturer),
		 build_OUI(Data#device_id_struct.oui),
		 build_ProductClass(Data#device_id_struct.product_class),
		 build_SerialNumber(Data#device_id_struct.serial_number)
		], P /= null]}.

-spec build_EventStruct(#event_struct{}, #builder{}) -> export_element().
build_EventStruct(Data, _S) when Data =:= undefined -> null;
build_EventStruct(Data, State) ->
    {'cwmp:EventStruct', [],
     [P || P <- [
		 build_EventCode(Data#event_struct.event_code, State),
		 build_CommandKey(Data#event_struct.command_key, State)
		], P /= null]}.

-spec build_ParameterInfoStruct(#parameter_info_struct{}, #builder{}) -> export_element().
build_ParameterInfoStruct(Data, _S) when Data =:= undefined -> null;
build_ParameterInfoStruct(Data, _State) ->
    {'cwmp:ParameterInfoStruct', [],
     [P || P <- [
		 build_Name(Data#parameter_info_struct.name),
		 build_Writable(Data#parameter_info_struct.writable)
		], P /= null]}.

-spec build_SetParameterAttributesStruct(#set_parameter_attributes_struct{}, #builder{}) -> export_element().
build_SetParameterAttributesStruct(Data, _S) when Data =:= undefined -> null;
build_SetParameterAttributesStruct(Data, State) ->
    {'cwmp:SetParameterAttributesStruct', [],
     [P || P <- [
		 build_Name(Data#set_parameter_attributes_struct.name),
		 build_NotificationChange(Data#set_parameter_attributes_struct.notification_change),
		 build_Notification(Data#set_parameter_attributes_struct.notification, State),
		 build_AccessListChange(Data#set_parameter_attributes_struct.access_list_change),
		 build_AccessList(Data#set_parameter_attributes_struct.access_list, State)
		], P /= null]}.

-spec build_ParameterAttributeStruct(#parameter_attribute_struct{}, #builder{}) -> export_element().
build_ParameterAttributeStruct(Data, _S) when Data =:= undefined -> null;
build_ParameterAttributeStruct(Data, State) ->
    {'cwmp:ParameterAttributeStruct', [],
     [P || P <- [
		 build_Name(Data#parameter_attribute_struct.name),
		 build_Notification(Data#parameter_attribute_struct.notification, State),
		 build_AccessList(Data#parameter_attribute_struct.access_list, State)
		], P /= null]}.

-spec build_TimeWindowStruct(#time_window_struct{}, #builder{}) -> export_element().
build_TimeWindowStruct(Data, _S) when Data =:= undefined -> null;
build_TimeWindowStruct(Data, State) ->
    {'cwmp:TimeWindowStruct', [],
     [P || P <- [
		 build_WindowStart(Data#time_window_struct.window_start),
		 build_WindowEnd(Data#time_window_struct.window_end),
		 build_WindowMode(Data#time_window_struct.window_mode, State),
		 build_UserMessage(Data#time_window_struct.user_message),
		 build_MaxRetries(Data#time_window_struct.max_retries)
		], P /= null]}.

-spec build_QueuedTransferStruct(#queued_transfer_struct{}, #builder{}) -> export_element().
build_QueuedTransferStruct(Data, _S) when Data =:= undefined -> null;
build_QueuedTransferStruct(Data, State) ->
    {'cwmp:QueuedTransferStruct', [],
     [P || P <- [
		 build_CommandKey(Data#queued_transfer_struct.command_key, State),
		 build_State(Data#queued_transfer_struct.state)
		], P /= null]}.

-spec build_AllQueuedTransferStruct(#all_queued_transfer_struct{}, #builder{}) -> export_element().
build_AllQueuedTransferStruct(Data, _S) when Data =:= undefined -> null;
build_AllQueuedTransferStruct(Data, State) ->
    {'cwmp:AllQueuedTransferStruct', [],
     [P || P <- [
		 build_CommandKey(Data#all_queued_transfer_struct.command_key, State),
		 build_State(Data#all_queued_transfer_struct.state),
		 build_IsDownload(Data#all_queued_transfer_struct.is_download),
		 build_FileType(Data#all_queued_transfer_struct.file_type, State),
		 build_FileSize(Data#all_queued_transfer_struct.file_size),
		 build_TargetFileName(Data#all_queued_transfer_struct.target_file_name)
		], P /= null]}.

-spec build_InstallOpStruct(#install_op_struct{}, #builder{}) -> export_element().
build_InstallOpStruct(Data, _S) when Data =:= undefined -> null;
build_InstallOpStruct(Data, _State) ->
    {'cwmp:InstallOpStruct', [],
     [P || P <- [
		 build_URL(Data#install_op_struct.url),
		 build_UUID(Data#install_op_struct.uuid),
		 build_Username(Data#install_op_struct.username),
		 build_Password(Data#install_op_struct.password),
		 build_ExecutionEnvRef(Data#install_op_struct.execution_env_ref)
		], P /= null]}.

-spec build_UpdateOpStruct(#update_op_struct{}, #builder{}) -> export_element().
build_UpdateOpStruct(Data, _S) when Data =:= undefined -> null;
build_UpdateOpStruct(Data, _State) ->
    {'cwmp:UpdateOpStruct', [],
     [P || P <- [
		 build_UUID(Data#update_op_struct.uuid),
		 build_Version(Data#update_op_struct.version),
		 build_URL(Data#update_op_struct.url),
		 build_Username(Data#update_op_struct.username),
		 build_Password(Data#update_op_struct.password)
		], P /= null]}.

-spec build_UninstallOpStruct(#uninstall_op_struct{}, #builder{}) -> export_element().
build_UninstallOpStruct(Data, _S) when Data =:= undefined -> null;
build_UninstallOpStruct(Data, _State) ->
    {'cwmp:UninstallOpStruct', [],
     [P || P <- [
		 build_UUID(Data#uninstall_op_struct.uuid),
		 build_Version(Data#uninstall_op_struct.version),
		 build_ExecutionEnvRef(Data#uninstall_op_struct.execution_env_ref)
		], P /= null]}.

-spec build_OpResultStruct(#op_result_struct{}, #builder{}) -> export_element().
build_OpResultStruct(Data, _S) when Data =:= undefined -> null;
build_OpResultStruct(Data, State) ->
    {'cwmp:OpResultStruct', [],
     [P || P <- [
		 build_UUID(Data#op_result_struct.uuid),
		 build_DeploymentUnitRef(Data#op_result_struct.deployment_unit_ref),
		 build_Version(Data#op_result_struct.version),
		 build_CurrentState(Data#op_result_struct.current_state, State),
		 build_Resolved(Data#op_result_struct.resolved),
		 build_ExecutionUnitRefList(Data#op_result_struct.execution_unit_ref_list),
		 build_StartTime(Data#op_result_struct.start_time),
		 build_CompleteTime(Data#op_result_struct.complete_time),
		 build_Fault(Data#op_result_struct.fault, State)
		], P /= null]}.

-spec build_AutonOpResultStruct(#auton_op_result_struct{}, #builder{}) -> export_element().
build_AutonOpResultStruct(Data, _S) when Data =:= undefined -> null;
build_AutonOpResultStruct(Data, State) ->
    {'cwmp:AutonOpResultStruct', [],
     [P || P <- [
		 build_OperationPerformed(Data#auton_op_result_struct.operation_performed, State)
		], P /= null]}.

-spec build_OptionStruct(#option_struct{}, #builder{}) -> export_element().
build_OptionStruct(Data, _S) when Data =:= undefined -> null;
build_OptionStruct(Data, _State) ->
    {'cwmp:OptionStruct', [],
     [P || P <- [
		 build_OptionName(Data#option_struct.option_name),
		 build_VoucherSN(Data#option_struct.voucher_sn),
		 build_State(Data#option_struct.state),
		 build_Mode(Data#option_struct.mode),
		 build_StartDate(Data#option_struct.start_date),
		 build_ExpirationDate(Data#option_struct.expiration_date),
		 build_IsTransferable(Data#option_struct.is_transferable)
		], P /= null]}.

-spec build_ArgStruct(#arg_struct{}, #builder{}) -> export_element().
build_ArgStruct(Data, _S) when Data =:= undefined -> null;
build_ArgStruct(Data, _State) ->
    {'cwmp:ArgStruct', [],
     [P || P <- [
		 build_Name(Data#arg_struct.name),
		 build_Value(Data#arg_struct.value)
		], P /= null]}.


%%%-----------------------------------------------------------------------------
%%% Build Parameter List
%%% Array complexType/complexContent/restriction base="soapenc:Array"
%%% Generate list of sequence/element
%%%-----------------------------------------------------------------------------
attr_arrayType(Tag, Lenght) ->
    Value=io_lib:format("~s[~02p]", [Tag, Lenght]),
    {'soapenc:arrayType', list_to_atom(lists:flatten(Value))}.

-spec build_MethodList([string()], #builder{}) -> export_element().
build_MethodList(Data, _S) when Data =:= undefined -> null;
build_MethodList(Data,  _State) ->
    TagArray =  [build_string(E) || E <- Data],
    {'MethodList', [attr_arrayType("xsd:string",length(TagArray))], TagArray}.

-spec build_ParameterNames([string()], #builder{}) -> export_element().
build_ParameterNames(Data, _S) when Data =:= undefined -> null;
build_ParameterNames(Data,  _State) ->
    TagArray =  [build_string(E) || E <- Data],
    {'ParameterNames', [attr_arrayType("xsd:string", length(TagArray))], TagArray}.

-spec build_ParameterValueList([#parameter_value_struct{}], #builder{}) -> export_element().
build_ParameterValueList(Data, _S) when Data =:= undefined -> null;
build_ParameterValueList(Data,  State) ->
    TagArray =  [build_ParameterValueStruct(E, State) || E <- Data],
    {'ParameterValueList', [attr_arrayType("cwmp:ParameterValueStruct", length(TagArray))], TagArray}.


-spec build_EventList([#event_struct{}], #builder{}) -> export_element().
build_EventList(Data, _S) when Data =:= undefined -> null;
build_EventList(Data,  State) ->
    TagArray =  [build_EventStruct(E, State) || E <- Data],
    {'EventList', [attr_arrayType("cwmp:EventStruct", length(TagArray))], TagArray}.

-spec build_ParameterInfoList([#parameter_info_struct{}], #builder{}) -> export_element().
build_ParameterInfoList(Data, _S) when Data =:= undefined -> null;
build_ParameterInfoList(Data,  State) ->
    TagArray =  [build_ParameterInfoStruct(E, State) || E <- Data],
    {'ParameterInfoList', [attr_arrayType("cwmp:ParameterInfoStruct", length(TagArray))], TagArray}.

-spec build_AccessList([string()], #builder{}) -> export_element().
build_AccessList(Data, _S) when Data =:= undefined -> null;
build_AccessList(Data,  _State) ->
    TagArray =  [build_string(E) || E <- Data],
    {'AccessList', [attr_arrayType("xsd:string", length(TagArray))], TagArray}.

-spec build_SetParameterAttributesList([#set_parameter_attributes_struct{}], #builder{}) -> export_element().
build_SetParameterAttributesList(Data, _S) when Data =:= undefined -> null;
build_SetParameterAttributesList(Data, State) ->
    TagArray =  [build_SetParameterAttributesStruct(E, State) || E <- Data],
    {'SetParameterAttributesList', [attr_arrayType("cwmp:SetParameterAttributesStruct", length(TagArray))], TagArray}.

-spec build_ParameterAttributeList([#parameter_attribute_struct{}], #builder{}) -> export_element().
build_ParameterAttributeList(Data, _S) when Data =:= undefined -> null;
build_ParameterAttributeList(Data,  State) ->
    TagArray =  [build_ParameterAttributeStruct(E, State) || E <- Data],
    {'ParameterAttributeList', [attr_arrayType("cwmp:ParameterAttributeStruct", length(TagArray))], TagArray}.

-spec build_TimeWindowList([#time_window_struct{}], #builder{}) -> export_element().
build_TimeWindowList(Data, _S) when Data =:= undefined -> null;
build_TimeWindowList(Data,  State) ->
    TagArray =  [build_TimeWindowStruct(E, State) || E <- Data],
    {'TimeWindowList', [attr_arrayType("cwmp:TimeWindowStruct", length(TagArray))], TagArray}.

-spec build_TransferList([#queued_transfer_struct{}], #builder{}) -> export_element().
build_TransferList(Data, _S) when Data =:= undefined -> null;
build_TransferList(Data,  State) ->
    TagArray =  [build_QueuedTransferStruct(E, State) || E <- Data],
    {'TransferList', [attr_arrayType("cwmp:QueuedTransferStruct", length(TagArray))], TagArray}.

-spec build_AllTransferList([#all_queued_transfer_struct{}], #builder{}) -> export_element().
build_AllTransferList(Data, _S) when Data =:= undefined -> null;
build_AllTransferList(Data,  State) ->
    TagArray =  [build_AllQueuedTransferStruct(E, State) || E <- Data],
    {'AllTransferList', [attr_arrayType("cwmp:AllQueuedTransferStruct", length(TagArray))], TagArray}.

%-spec build_VoucherList([#base64{}], #builder{}) -> export_element().
build_VoucherList(Data, _S) when Data =:= undefined -> null;
build_VoucherList(Data,  _State) ->
    TagArray =  [build_base64(E) || E <- Data],
    {'VoucherList', [attr_arrayType("soapenc:base64", length(TagArray))], TagArray}.

-spec build_OptionList([#option_struct{}], #builder{}) -> export_element().
build_OptionList(Data, _S) when Data =:= undefined -> null;
build_OptionList(Data,  State) ->
    TagArray =  [build_OptionStruct(E, State) || E <- Data],
    {'OptionList', [attr_arrayType("cwmp:OptionStruct", length(TagArray))], TagArray}.

-spec build_FileTypeArg([#arg_struct{}], #builder{}) -> export_element().
build_FileTypeArg(Data, _S) when Data =:= undefined -> null;
build_FileTypeArg(Data,  State) ->
    TagArray =  [build_ArgStruct(E, State) || E <- Data],
    {'FileTypeArg', [attr_arrayType("cwmp:ArgStruct", length(TagArray))], TagArray}.



%%%-----------------------------------------------------------------------------
%%% Unitary tetsts
%%%-----------------------------------------------------------------------------
-define(RPC_DATA,
	{cwmp_obj,
	 {envelope,
	  {header,{id,true,"22_THOM_TR69_ID"},undefined,undefined},
	  [{get_rpc_methods_response,
	    ["GetRPCMethods","GetParameterNames",
	     "GetParameterValues","SetParameterValues","AddObject",
	     "DeleteObject","Download","Reboot","FactoryReset"]}]}}
       ).

-define(RPC_FAULT,
	{cwmp_obj,
	 {envelope,
	  {header,
	   {id,true,"0_THOM_TR69_ID"},
	   {hold_requests,true,false},
	   false},
	  [{soap_fault,"Client","CWMP fault",undefined,
	    {fault,9001,"Request Denied",undefined}}]}}
       ).

-define(XML_NAMESPACE,
	{xmlNamespace,[],
	 [{"soapenc",'http://schemas.xmlsoap.org/soap/encoding/'},
	  {"soapenv",'http://schemas.xmlsoap.org/soap/envelope/'},
	  {"cwmp",'urn:dslforum-org:cwmp-1-0'}]}
       ).

-define(TAG_DATA,
	[{'Tag', [{attr, "Attributes"}], []}]).


-define(GET_RPC_METHODS_RESPONSE,
	{get_rpc_methods_response,
	 ["GetRPCMethods","GetParameterNames","GetParameterValues",
	  "SetParameterValues","AddObject","DeleteObject","Download",
	  "Reboot","FactoryReset"]}).

get_rpc_methods_response() ->
    build_GetRPCMethodsResponse(?GET_RPC_METHODS_RESPONSE, #builder{}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

main() ->
    Nss = cwmp_lib:match_cwmp_ns_and_version(?XML_NAMESPACE),
%    ?DBG(Nss),
    Builder = builder([{namespaces, Nss}]),
    Export = Builder(?RPC_FAULT),
%    ?DBG(Export),
    XML = xmerl:export_simple(Export, xmerl_xml, [{prolog,[]}]),
						%    ?DBG(unicode:characters_to_list(XML)),
%    ?DBG(XML),
    ok.



build_cwmp_obj_test() ->
    Builder =  #builder{},
    GetRpcMethods = {cwmp_obj,{envelope,{header,{id,true,"1"},undefined,undefined},
                              [{get_rpc_methods}]}},
    XML_Object = build_cwmp_obj(GetRpcMethods, Builder), %?RPC_DATA
    XML = xmerl:export_simple(XML_Object, xmerl_xml, [{prolog,[]}]),
    ?DBG(XML),
    ok.

export_test_no() ->
    Data =
	{'cwmp:bike',
	 [{year,"2003"},{color,"black"},{condition,"new"}],
	 [{'soap:name',
	   [{manufacturer,["Harley Davidsson"]},
	    {brandName,["XL1200C"]},
	    {additionalName,["Sportster"]}]},
	  {engine,
	   ["V-engine, 2-cylinders, 1200 cc"]},
	  {kind,["custom"]},
	  {drive,["belt"]}]},
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>\
 <!DOCTYPE motorcycles SYSTEM \"motorcycles.dtd\">\
"],
    RootEl = #xmlElement{content=[Data]},
	      Data1 = [{'soapenv:Envelop'}],
	      Data2 =
		  {'soapenv:Envelop',
		   [],
		   []},
	      Data3 =
		  {envelope,[],
		   [
		    {header,[],
		     [{id,[{mustUnderstand,true}],
		       ["22_THOM_TR69_ID"]}]}
		   ]
		  },

	      XML = xmerl:export_simple([Data3], xmerl_xml, [{prolog,[]}]),
%	      ?DBG(unicode:characters_to_list(XML)),
	ok.


-define(EXML,
	{'soapenv:Envelop',
	 [{'xmlns:soapenv','http://schemas.xmlsoap.org/soap/envelope/'},
	  {'xmlns:soapenc','http://schemas.xmlsoap.org/soap/encoding/'},
	  {'xmlns:xsd','http://www.w3.org/2001/XMLSchema'},
	  {'xmlns:xsi','http://www.w3.org/2001/XMLSchema-instance'},
	  {'xmlns:cwmp','urn:dslforum-org:cwmp-1-0'}],
	 ['soapenv:Header',
	  {'soapenv:Body',[],
	   [{'cwmp:AutonomousTransferComplete',[],
	     [{'AnnounceURL',[],[{http,"announceURL-FwUpgr",80,"/",[]}]},
	      {'TransferURL',[],[{http,"transferURL-FwUpgr",80,"/",[]}]},
	      {'IsDownload',[],["true"]},
               {'DownloadFileType',[],[1]},
	      {'FileSize',[],["12345"]},
	      {'TargetFileName',[],["http://targetFileName_FwUpgr"]},
	      {'cwmp:TransferCompleteFaultStruct',[],
	       [{'FaultCode',[],["0"]},{'FaultString',[],[[]]}]},
	      {'StartTime',[],[{{2012,10,3},{18,53,34}}]},
	      {'CompleteTime',[],[{{2012,10,3},{18,53,44}}]}]}]}]},[[]]).



main_test() ->
    XML = [
	   {'soapenv:Envelope',
	    [{'xmlns:soapenv','http://schemas.xmlsoap.org/soap/envelope/'},
	     {'xmlns:soapenc','http://schemas.xmlsoap.org/soap/encoding/'},
	     {'xmlns:xsd','http://www.w3.org/2001/XMLSchema'},
	     {'xmlns:xsi','http://www.w3.org/2001/XMLSchema-instance'},
	     {'xmlns:cwmp','urn:dslforum-org:cwmp-1-0'}],
	    ['soapenv:Header',
	     {'soapenv:Body',[],	 		 
	      [{'cwmp:Inform',[],
		[
		 {'cwmp:DeviceIdStruct',[],
		  [{'Manufacturer',[],["Alcatel"]},
		   {'OUI',[],["001D4C"]},
		   {'ProductClass',[],["9365 BSR Femto"]},
		   {'SerialNumber',[],["4321"]}]}
		 ,
		 {'EventList',
		  [
		   %%{'soapenc:arrayType','cwmp:EventStruct[1]'} 
		  ],
		  [
		   {'cwmp:EventStruct',[],
		    [
		      {'EventCodeType',[],["2"]}
		     %%,
		     %% {'CommandKeyType',[],[]}
		    ]
		   }
		  ]
		 }

		 %% ,
		 
		 %% {'MaxEnvelopes',[],["1"]},
		 %% {'CurrentTime',[],["2012-10-03T18:16:02Z"]},
		 %% {'RetryCount',[],["0"]},
		 %% {'ParameterValueList',
		 %%  [{'soapenc:arrayType','cwmp:ParameterValueStruct[10]'}],
		 %%  [{'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],["Device.DeviceSummary"]},
		 %%     {'Value',[],["DeviceSummary"]}]},
		 %%   {'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],["Device.DeviceInfo.HardwareVersion"]},
		 %%     {'Value',[],["p1"]}]},
		 %%   {'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],["Device.DeviceInfo.SoftwareVersion"]},
		 %%     {'Value',[],["BCR-04-00-BSR-XMIM-06.01"]}]},
		 %%   {'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],
		 %%      ["Device.ManagementServer.ConnectionRequestURL"]},
		 %%     {'Value',[],
		 %%      ["&sn=4321http://127.0.0.1:8095/ConnectionRequest?command=cr"]}]},
		 %%   {'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],["Device.ManagementServer.ParameterKey"]},
		 %%     {'Value',[],["ParameterKey"]}]},
		 %%   {'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],["Device.LAN.IPAddress"]},
		 %%     {'Value',[],["172.0.0.1"]}]},
		 %%   {'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],["Device.LAN.MACAddress"]},
		 %%     {'Value',[],["00-0E-35-D6-24-F7"]}]},
		 %%   {'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],["Device.Services.BSR.1.SDM.1.mimVersion"]},
		 %%     {'Value',[],["BCR-04-00-BSR-XMIM-02.00"]}]},
		 %%   {'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],
		 %%      ["Device.DeviceInfo.AdditionalHardwareVersion"]},
		 %%     {'Value',[],["250mW"]}]},
		 %%   {'cwmp:ParameterValueStruct',[],
		 %%    [{'Name',[],["Device.Services.BSR.1.RFTrace.1.iMSI"]},
		 %%     {'Value',[],["iMSI"]}]}]}
		]
	       }]}]}
	  ],

	xmerl:export_simple(XML, xmerl_xml, [{prolog,[]}]),
    ok.
-endif.
