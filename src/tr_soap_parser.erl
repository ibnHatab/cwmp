
-module(tr_soap_parser).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-import(tr_soap_lib, [get_local_name/2,
		      parse_error/2,
		      
		      parse_anySimpleType/2,
		      xmlElement/1,
		      parse_unsignedInt/1,
		      parse_unsignedInt/2,
		      parse_string/1,
		      parse_boolean/2,
		      parse_int/1,
		      parse_int/2,
		      parse_dateTime/2,
		      parse_base64/2,

		      parse_SetParameterValuesFault/2,
		      parse_ParameterName/2,
		      parse_ParameterList/2,
		      parse_ParameterKey/2,
		      parse_Status/2,
		      parse_ParameterPath/2,
		      parse_NextLevel/2,
		      parse_ObjectName/2,
		      parse_InstanceNumber/2,
		      
		      parse_CommandKey/2,
		      parse_FileType/2,
		      parse_URL/2,
		      parse_Username/2,
		      parse_Password/2,
		      parse_FileSize/2,
		      parse_TargetFileName/2,
		      parse_DelaySeconds/2,
		      parse_SuccessURL/2,
		      parse_FailureURL/2,
		      parse_StartTime/2,
		      parse_CompleteTime/2,
		      parse_OptionName/2,
		      parse_Operations/2,
		      parse_CommandKey/2,
		      parse_DeviceId/2,
		      parse_Event/2,
		      parse_MaxEnvelopes/2,
		      parse_CurrentTime/2,
		      parse_RetryCount/2,
		      parse_FaultStruct/2,
		      parse_AnnounceURL/2,
		      parse_TransferURL/2,
		      parse_IsDownload/2,
		      parse_Command/2,
		      parse_Referer/2,
		      parse_Arg/2,
		      parse_Next/2,
		      parse_NextURL/2,
		      parse_Results/2

]).


    

parse_FaultCode(E, _) -> parse_unsignedInt(E).
parse_FaultString(E, _) -> parse_string(E).

parse_string(E, _) -> parse_string(E).
parse_Name(E, _) -> parse_string(E).
parse_Value(E, S) -> parse_anySimpleType(E, S).

parse_Manufacturer(E, _) -> parse_string(E).
parse_OUI(E, _) -> parse_string(E).
parse_ProductClass(E, _) -> parse_string(E).
parse_SerialNumber(E, _) -> parse_string(E).

    
%% -spec parse_ID(#xmlElement{},#decoder{}) -> #i_d{}.
parse_ID(_, _) -> #id{}.
    
%% -spec parse_HoldRequests(#xmlElement{},#decoder{}) -> #hold_requests{}.
parse_HoldRequests(_, _) -> #hold_requests{}.
    
%% Simple type
parse_CPEFaultCodeType(E,_S) -> parse_unsignedInt(E).
parse_CPEFaultCodeType(E) -> parse_unsignedInt(E).
    
%% Simple type
parse_CPEExtensionFaultCodeType(E,_S) -> parse_unsignedInt(E).
    
%% Simple type
parse_CPEVendorFaultCodeType(E,_S) -> parse_unsignedInt(E).
    
%% Simple type
parse_ACSFaultCodeType(E,_S) -> parse_unsignedInt(E).
    
%% Simple type
parse_ACSVendorFaultCodeType(E,_S) -> parse_unsignedInt(E).
    
%% Simple type
parse_TransferFileType(E,_S) -> parse_string(E).
    
%% Simple type
parse_DownloadFileType(E,_S) -> parse_string(E).
    
%% Simple type
parse_UploadFileType(E,_S) -> parse_string(E).
    
%% Simple type
parse_EventCodeType(E,_S) -> parse_string(E).
    
%% Simple type
parse_TimeWindowModeValueType(E,_S) -> parse_string(E).
    
%% Simple type
parse_TransferCompleteCPEFaultCodeType(E,_S) -> parse_CPEFaultCodeType(E).
    
%% -spec parse_TransferCompleteFaultStruct(#xmlElement{},#decoder{}) -> #transfer_complete_fault_struct{}.
parse_TransferCompleteFaultStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, TransferCompleteFaultStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'FaultCode' ->
                                TransferCompleteFaultStruct#transfer_complete_fault_struct{fault_code = parse_string(Elem, State)};
    
                            'FaultString' ->
                                TransferCompleteFaultStruct#transfer_complete_fault_struct{fault_string = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #transfer_complete_fault_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% Simple type
parse_DeploymentUnitCPEFaultCodeType(E,_S) -> parse_CPEFaultCodeType(E).
    
%% -spec parse_DeploymentUnitFaultStruct(#xmlElement{},#decoder{}) -> #deployment_unit_fault_struct{}.
parse_DeploymentUnitFaultStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, DeploymentUnitFaultStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'FaultCode' ->
                                DeploymentUnitFaultStruct#deployment_unit_fault_struct{fault_code = parse_string(Elem, State)};
    
                            'FaultString' ->
                                DeploymentUnitFaultStruct#deployment_unit_fault_struct{fault_string = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #deployment_unit_fault_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% Simple type
parse_CommandKeyType(E,_S) -> parse_string(E).
    
%% Simple type
parse_ObjectNameType(E,_S) -> parse_string(E).
    
%% Simple type
parse_ParameterKeyType(E,_S) -> parse_string(E).
    
%% -spec parse_ParameterNames(#xmlElement{},#decoder{}) -> #parameter_names{}.
parse_ParameterNames(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ParameterNames) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'string' ->
                                ParameterNames#parameter_names{string = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_names{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_ParameterValueStruct(#xmlElement{},#decoder{}) -> #parameter_value_struct{}.
parse_ParameterValueStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ParameterValueStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Name' ->
                                ParameterValueStruct#parameter_value_struct{name = parse_string(Elem, State)};
    
                            'Value' ->
                                ParameterValueStruct#parameter_value_struct{value = parse_anySimpleType(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_value_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_ParameterValueList(#xmlElement{},#decoder{}) -> #parameter_value_list{}.
parse_ParameterValueList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ParameterValueList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterValueStruct' ->
                                ParameterValueList#parameter_value_list{parameter_value_struct = parse_ParameterValueStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_value_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_MethodList(#xmlElement{},#decoder{}) -> #method_list{}.
parse_MethodList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, MethodList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'string' ->
                                MethodList#method_list{string = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #method_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_DeviceIdStruct(#xmlElement{},#decoder{}) -> #device_id_struct{}.
parse_DeviceIdStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, DeviceIdStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Manufacturer' ->
                                DeviceIdStruct#device_id_struct{manufacturer = parse_string(Elem, State)};
    
                            'OUI' ->
                                DeviceIdStruct#device_id_struct{oui = parse_string(Elem, State)};
    
                            'ProductClass' ->
                                DeviceIdStruct#device_id_struct{product_class = parse_string(Elem, State)};
    
                            'SerialNumber' ->
                                DeviceIdStruct#device_id_struct{serial_number = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #device_id_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_EventStruct(#xmlElement{},#decoder{}) -> #event_struct{}.
parse_EventStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, EventStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'EventCode' ->
                                EventStruct#event_struct{event_code = parse_EventCodeType(Elem, State)};
    
                            'CommandKey' ->
                                EventStruct#event_struct{command_key = parse_CommandKeyType(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #event_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_EventList(#xmlElement{},#decoder{}) -> #event_list{}.
parse_EventList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, EventList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'EventStruct' ->
                                EventList#event_list{event_struct = parse_EventStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #event_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_ParameterInfoStruct(#xmlElement{},#decoder{}) -> #parameter_info_struct{}.
parse_ParameterInfoStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ParameterInfoStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Name' ->
                                ParameterInfoStruct#parameter_info_struct{name = parse_string(Elem, State)};
    
                            'Writable' ->
                                ParameterInfoStruct#parameter_info_struct{writable = parse_boolean(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_info_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_ParameterInfoList(#xmlElement{},#decoder{}) -> #parameter_info_list{}.
parse_ParameterInfoList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ParameterInfoList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterInfoStruct' ->
                                ParameterInfoList#parameter_info_list{parameter_info_struct = parse_ParameterInfoStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_info_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% Simple type
parse_AccessListValueType(E,_S) -> parse_string(E).
    
%% -spec parse_AccessList(#xmlElement{},#decoder{}) -> #access_list{}.
parse_AccessList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, AccessList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'string' ->
                                AccessList#access_list{string = parse_AccessListValueType(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #access_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% Simple type
parse_ParameterAttributeNotificationValueType(E,_S) -> parse_int(E).
    
%% -spec parse_SetParameterAttributesStruct(#xmlElement{},#decoder{}) -> #set_parameter_attributes_struct{}.
parse_SetParameterAttributesStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, SetParameterAttributesStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Name' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{name = parse_string(Elem, State)};
    
                            'NotificationChange' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{notification_change = parse_boolean(Elem, State)};
    
                            'Notification' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{notification = parse_ParameterAttributeNotificationValueType(Elem, State)};
    
                            'AccessListChange' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{access_list_change = parse_boolean(Elem, State)};
    
                            'AccessList' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{access_list = parse_AccessList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_attributes_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_SetParameterAttributesList(#xmlElement{},#decoder{}) -> #set_parameter_attributes_list{}.
parse_SetParameterAttributesList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, SetParameterAttributesList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'SetParameterAttributesStruct' ->
                                SetParameterAttributesList#set_parameter_attributes_list{set_parameter_attributes_struct = parse_SetParameterAttributesStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_attributes_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_ParameterAttributeStruct(#xmlElement{},#decoder{}) -> #parameter_attribute_struct{}.
parse_ParameterAttributeStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ParameterAttributeStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Name' ->
                                ParameterAttributeStruct#parameter_attribute_struct{name = parse_string(Elem, State)};
    
                            'Notification' ->
                                ParameterAttributeStruct#parameter_attribute_struct{notification = parse_ParameterAttributeNotificationValueType(Elem, State)};
    
                            'AccessList' ->
                                ParameterAttributeStruct#parameter_attribute_struct{access_list = parse_AccessList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_attribute_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_ParameterAttributeList(#xmlElement{},#decoder{}) -> #parameter_attribute_list{}.
parse_ParameterAttributeList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ParameterAttributeList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterAttributeStruct' ->
                                ParameterAttributeList#parameter_attribute_list{parameter_attribute_struct = parse_ParameterAttributeStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_attribute_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_TimeWindowStruct(#xmlElement{},#decoder{}) -> #time_window_struct{}.
parse_TimeWindowStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, TimeWindowStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'WindowStart' ->
                                TimeWindowStruct#time_window_struct{window_start = parse_unsignedInt(Elem, State)};
    
                            'WindowEnd' ->
                                TimeWindowStruct#time_window_struct{window_end = parse_unsignedInt(Elem, State)};
    
                            'WindowMode' ->
                                TimeWindowStruct#time_window_struct{window_mode = parse_TimeWindowModeValueType(Elem, State)};
    
                            'UserMessage' ->
                                TimeWindowStruct#time_window_struct{user_message = parse_string(Elem, State)};
    
                            'MaxRetries' ->
                                TimeWindowStruct#time_window_struct{max_retries = parse_int(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #time_window_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_TimeWindowList(#xmlElement{},#decoder{}) -> #time_window_list{}.
parse_TimeWindowList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, TimeWindowList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'TimeWindowStruct' ->
                                TimeWindowList#time_window_list{time_window_struct = parse_TimeWindowStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #time_window_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% Simple type
parse_TransferStateType(E,_S) -> parse_int(E).
    
%% -spec parse_QueuedTransferStruct(#xmlElement{},#decoder{}) -> #queued_transfer_struct{}.
parse_QueuedTransferStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, QueuedTransferStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'CommandKey' ->
                                QueuedTransferStruct#queued_transfer_struct{command_key = parse_CommandKeyType(Elem, State)};
    
                            'State' ->
                                QueuedTransferStruct#queued_transfer_struct{state = parse_TransferStateType(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #queued_transfer_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_TransferList(#xmlElement{},#decoder{}) -> #transfer_list{}.
parse_TransferList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, TransferList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'QueuedTransferStruct' ->
                                TransferList#transfer_list{queued_transfer_struct = parse_QueuedTransferStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #transfer_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_AllQueuedTransferStruct(#xmlElement{},#decoder{}) -> #all_queued_transfer_struct{}.
parse_AllQueuedTransferStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, AllQueuedTransferStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'CommandKey' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{command_key = parse_CommandKeyType(Elem, State)};
    
                            'State' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{state = parse_TransferStateType(Elem, State)};
    
                            'IsDownload' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{is_download = parse_boolean(Elem, State)};
    
                            'FileType' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{file_type = parse_TransferFileType(Elem, State)};
    
                            'FileSize' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{file_size = parse_unsignedInt(Elem, State)};
    
                            'TargetFileName' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{target_file_name = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #all_queued_transfer_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_AllTransferList(#xmlElement{},#decoder{}) -> #all_transfer_list{}.
parse_AllTransferList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, AllTransferList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'AllQueuedTransferStruct' ->
                                AllTransferList#all_transfer_list{all_queued_transfer_struct = parse_AllQueuedTransferStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #all_transfer_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% Simple type
parse_DeploymentUnitUUID(E,_S) -> parse_string(E).
    
%% Simple type
parse_DeploymentUnitState(E,_S) -> parse_string(E).
    
%% Simple type
parse_DefaultDeploymentUnitOperationType(E,_S) -> parse_string(E).
    
%% Simple type
parse_DeploymentUnitOperationType(E,_S) -> parse_string(E).
    
%% -spec parse_OperationStruct(#xmlElement{},#decoder{}) -> #operation_struct{}.
parse_OperationStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, OperationStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #operation_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_InstallOpStruct(#xmlElement{},#decoder{}) -> #install_op_struct{}.
parse_InstallOpStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, InstallOpStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'URL' ->
                                InstallOpStruct#install_op_struct{url = parse_string(Elem, State)};
    
                            'UUID' ->
                                InstallOpStruct#install_op_struct{uuid = parse_DeploymentUnitUUID(Elem, State)};
    
                            'Username' ->
                                InstallOpStruct#install_op_struct{username = parse_string(Elem, State)};
    
                            'Password' ->
                                InstallOpStruct#install_op_struct{password = parse_string(Elem, State)};
    
                            'ExecutionEnvRef' ->
                                InstallOpStruct#install_op_struct{execution_env_ref = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #install_op_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_UpdateOpStruct(#xmlElement{},#decoder{}) -> #update_op_struct{}.
parse_UpdateOpStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, UpdateOpStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'UUID' ->
                                UpdateOpStruct#update_op_struct{uuid = parse_DeploymentUnitUUID(Elem, State)};
    
                            'Version' ->
                                UpdateOpStruct#update_op_struct{version = parse_string(Elem, State)};
    
                            'URL' ->
                                UpdateOpStruct#update_op_struct{url = parse_string(Elem, State)};
    
                            'Username' ->
                                UpdateOpStruct#update_op_struct{username = parse_string(Elem, State)};
    
                            'Password' ->
                                UpdateOpStruct#update_op_struct{password = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #update_op_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_UninstallOpStruct(#xmlElement{},#decoder{}) -> #uninstall_op_struct{}.
parse_UninstallOpStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, UninstallOpStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'UUID' ->
                                UninstallOpStruct#uninstall_op_struct{uuid = parse_DeploymentUnitUUID(Elem, State)};
    
                            'Version' ->
                                UninstallOpStruct#uninstall_op_struct{version = parse_string(Elem, State)};
    
                            'ExecutionEnvRef' ->
                                UninstallOpStruct#uninstall_op_struct{execution_env_ref = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #uninstall_op_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_OpResultStruct(#xmlElement{},#decoder{}) -> #op_result_struct{}.
parse_OpResultStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, OpResultStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'UUID' ->
                                OpResultStruct#op_result_struct{uuid = parse_DeploymentUnitUUID(Elem, State)};
    
                            'DeploymentUnitRef' ->
                                OpResultStruct#op_result_struct{deployment_unit_ref = parse_string(Elem, State)};
    
                            'Version' ->
                                OpResultStruct#op_result_struct{version = parse_string(Elem, State)};
    
                            'CurrentState' ->
                                OpResultStruct#op_result_struct{current_state = parse_DeploymentUnitState(Elem, State)};
    
                            'Resolved' ->
                                OpResultStruct#op_result_struct{resolved = parse_boolean(Elem, State)};
    
                            'ExecutionUnitRefList' ->
                                OpResultStruct#op_result_struct{execution_unit_ref_list = parse_string(Elem, State)};
    
                            'StartTime' ->
                                OpResultStruct#op_result_struct{start_time = parse_dateTime(Elem, State)};
    
                            'CompleteTime' ->
                                OpResultStruct#op_result_struct{complete_time = parse_dateTime(Elem, State)};
    
                            'Fault' ->
                                OpResultStruct#op_result_struct{fault = parse_DeploymentUnitFaultStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #op_result_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_AutonOpResultStruct(#xmlElement{},#decoder{}) -> #auton_op_result_struct{}.
parse_AutonOpResultStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, AutonOpResultStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'OperationPerformed' ->
                                AutonOpResultStruct#auton_op_result_struct{operation_performed = parse_DeploymentUnitOperationType(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #auton_op_result_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_VoucherList(#xmlElement{},#decoder{}) -> #voucher_list{}.
parse_VoucherList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, VoucherList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'base64' ->
                                VoucherList#voucher_list{base64 = parse_base64(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #voucher_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_OptionStruct(#xmlElement{},#decoder{}) -> #option_struct{}.
parse_OptionStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, OptionStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'OptionName' ->
                                OptionStruct#option_struct{option_name = parse_string(Elem, State)};
    
                            'VoucherSN' ->
                                OptionStruct#option_struct{voucher_sn = parse_unsignedInt(Elem, State)};
    
                            'State' ->
                                OptionStruct#option_struct{state = parse_string(Elem, State)};
    
                            'Mode' ->
                                OptionStruct#option_struct{mode = parse_string(Elem, State)};
    
                            'StartDate' ->
                                OptionStruct#option_struct{start_date = parse_dateTime(Elem, State)};
    
                            'ExpirationDate' ->
                                OptionStruct#option_struct{expiration_date = parse_dateTime(Elem, State)};
    
                            'IsTransferable' ->
                                OptionStruct#option_struct{is_transferable = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #option_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_OptionList(#xmlElement{},#decoder{}) -> #option_list{}.
parse_OptionList(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, OptionList) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'OptionStruct' ->
                                OptionList#option_list{option_struct = parse_OptionStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #option_list{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_ArgStruct(#xmlElement{},#decoder{}) -> #arg_struct{}.
parse_ArgStruct(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ArgStruct) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Name' ->
                                ArgStruct#arg_struct{name = parse_string(Elem, State)};
    
                            'Value' ->
                                ArgStruct#arg_struct{value = parse_string(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #arg_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_FileTypeArg(#xmlElement{},#decoder{}) -> #file_type_arg{}.
parse_FileTypeArg(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, FileTypeArg) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ArgStruct' ->
                                FileTypeArg#file_type_arg{arg_struct = parse_ArgStruct(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #file_type_arg{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

%% -spec parse_Fault(#xmlElement{},#decoder{}) -> #fault{}.
parse_Fault(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, Fault) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'FaultCode' ->
                                Fault#fault{fault_code = parse_FaultCode(Elem, State)};
    
                            'FaultString' ->
                                Fault#fault{fault_string = parse_FaultString(Elem, State)};
    
                            'SetParameterValuesFault' ->
                                Fault#fault{set_parameter_values_fault = parse_SetParameterValuesFault(Elem, State)};
    
                            %% 'ParameterName' ->
                            %%     Fault#fault{parameter_name = parse_ParameterName(Elem, State)};
    
                            %% 'FaultCode' ->
                            %%     Fault#fault{fault_code = parse_FaultCode(Elem, State)};
    
                            %% 'FaultString' ->
                            %%     Fault#fault{fault_string = parse_FaultString(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #fault{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_GetRPCMethods(#xmlElement{},#decoder{}) -> #get_rpc_methods{}.
parse_GetRPCMethods(_, _) -> #get_rpc_methods{}.
    
%% -spec parse_GetRPCMethodsResponse(#xmlElement{},#decoder{}) -> #get_rpc_methods_response{}.
parse_GetRPCMethodsResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetRPCMethodsResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'MethodList' ->
                                GetRPCMethodsResponse#get_rpc_methods_response{method_list = parse_MethodList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_rpc_methods_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_SetParameterValues(#xmlElement{},#decoder{}) -> #set_parameter_values{}.
parse_SetParameterValues(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, SetParameterValues) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterList' ->
                                SetParameterValues#set_parameter_values{parameter_list = parse_ParameterList(Elem, State)};
    
                            'ParameterKey' ->
                                SetParameterValues#set_parameter_values{parameter_key = parse_ParameterKey(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_values{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_SetParameterValuesResponse(#xmlElement{},#decoder{}) -> #set_parameter_values_response{}.
parse_SetParameterValuesResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, SetParameterValuesResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Status' ->
                                SetParameterValuesResponse#set_parameter_values_response{status = parse_Status(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_values_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_GetParameterValues(#xmlElement{},#decoder{}) -> #get_parameter_values{}.
parse_GetParameterValues(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetParameterValues) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterNames' ->
                                GetParameterValues#get_parameter_values{parameter_names = parse_ParameterNames(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_values{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_GetParameterValuesResponse(#xmlElement{},#decoder{}) -> #get_parameter_values_response{}.
parse_GetParameterValuesResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetParameterValuesResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterList' ->
                                GetParameterValuesResponse#get_parameter_values_response{parameter_list = parse_ParameterList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_values_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_GetParameterNames(#xmlElement{},#decoder{}) -> #get_parameter_names{}.
parse_GetParameterNames(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetParameterNames) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterPath' ->
                                GetParameterNames#get_parameter_names{parameter_path = parse_ParameterPath(Elem, State)};
    
                            'NextLevel' ->
                                GetParameterNames#get_parameter_names{next_level = parse_NextLevel(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_names{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_GetParameterNamesResponse(#xmlElement{},#decoder{}) -> #get_parameter_names_response{}.
parse_GetParameterNamesResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetParameterNamesResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterList' ->
                                GetParameterNamesResponse#get_parameter_names_response{parameter_list = parse_ParameterList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_names_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_SetParameterAttributes(#xmlElement{},#decoder{}) -> #set_parameter_attributes{}.
parse_SetParameterAttributes(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, SetParameterAttributes) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterList' ->
                                SetParameterAttributes#set_parameter_attributes{parameter_list = parse_ParameterList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_attributes{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_SetParameterAttributesResponse(#xmlElement{},#decoder{}) -> #set_parameter_attributes_response{}.
parse_SetParameterAttributesResponse(_, _) -> #set_parameter_attributes_response{}.
    
%% -spec parse_GetParameterAttributes(#xmlElement{},#decoder{}) -> #get_parameter_attributes{}.
parse_GetParameterAttributes(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetParameterAttributes) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterNames' ->
                                GetParameterAttributes#get_parameter_attributes{parameter_names = parse_ParameterNames(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_attributes{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_GetParameterAttributesResponse(#xmlElement{},#decoder{}) -> #get_parameter_attributes_response{}.
parse_GetParameterAttributesResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetParameterAttributesResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ParameterList' ->
                                GetParameterAttributesResponse#get_parameter_attributes_response{parameter_list = parse_ParameterList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_attributes_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_AddObject(#xmlElement{},#decoder{}) -> #add_object{}.
parse_AddObject(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, AddObject) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ObjectName' ->
                                AddObject#add_object{object_name = parse_ObjectName(Elem, State)};
    
                            'ParameterKey' ->
                                AddObject#add_object{parameter_key = parse_ParameterKey(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #add_object{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_AddObjectResponse(#xmlElement{},#decoder{}) -> #add_object_response{}.
parse_AddObjectResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, AddObjectResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'InstanceNumber' ->
                                AddObjectResponse#add_object_response{instance_number = parse_InstanceNumber(Elem, State)};
    
                            'Status' ->
                                AddObjectResponse#add_object_response{status = parse_Status(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #add_object_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_DeleteObject(#xmlElement{},#decoder{}) -> #delete_object{}.
parse_DeleteObject(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, DeleteObject) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'ObjectName' ->
                                DeleteObject#delete_object{object_name = parse_ObjectName(Elem, State)};
    
                            'ParameterKey' ->
                                DeleteObject#delete_object{parameter_key = parse_ParameterKey(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #delete_object{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_DeleteObjectResponse(#xmlElement{},#decoder{}) -> #delete_object_response{}.
parse_DeleteObjectResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, DeleteObjectResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Status' ->
                                DeleteObjectResponse#delete_object_response{status = parse_Status(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #delete_object_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_Download(#xmlElement{},#decoder{}) -> #download{}.
parse_Download(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, Download) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'CommandKey' ->
                                Download#download{command_key = parse_CommandKey(Elem, State)};
    
                            'FileType' ->
                                Download#download{file_type = parse_FileType(Elem, State)};
    
                            'URL' ->
                                Download#download{url = parse_URL(Elem, State)};
    
                            'Username' ->
                                Download#download{username = parse_Username(Elem, State)};
    
                            'Password' ->
                                Download#download{password = parse_Password(Elem, State)};
    
                            'FileSize' ->
                                Download#download{file_size = parse_FileSize(Elem, State)};
    
                            'TargetFileName' ->
                                Download#download{target_file_name = parse_TargetFileName(Elem, State)};
    
                            'DelaySeconds' ->
                                Download#download{delay_seconds = parse_DelaySeconds(Elem, State)};
    
                            'SuccessURL' ->
                                Download#download{success_url = parse_SuccessURL(Elem, State)};
    
                            'FailureURL' ->
                                Download#download{failure_url = parse_FailureURL(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #download{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_DownloadResponse(#xmlElement{},#decoder{}) -> #download_response{}.
parse_DownloadResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, DownloadResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Status' ->
                                DownloadResponse#download_response{status = parse_Status(Elem, State)};
    
                            'StartTime' ->
                                DownloadResponse#download_response{start_time = parse_StartTime(Elem, State)};
    
                            'CompleteTime' ->
                                DownloadResponse#download_response{complete_time = parse_CompleteTime(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #download_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_Reboot(#xmlElement{},#decoder{}) -> #reboot{}.
parse_Reboot(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, Reboot) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'CommandKey' ->
                                Reboot#reboot{command_key = parse_CommandKey(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #reboot{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_RebootResponse(#xmlElement{},#decoder{}) -> #reboot_response{}.
parse_RebootResponse(_, _) -> #reboot_response{}.
    
%% -spec parse_GetQueuedTransfers(#xmlElement{},#decoder{}) -> #get_queued_transfers{}.
parse_GetQueuedTransfers(_, _) -> #get_queued_transfers{}.
    
%% -spec parse_GetQueuedTransfersResponse(#xmlElement{},#decoder{}) -> #get_queued_transfers_response{}.
parse_GetQueuedTransfersResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetQueuedTransfersResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'TransferList' ->
                                GetQueuedTransfersResponse#get_queued_transfers_response{transfer_list = parse_TransferList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_queued_transfers_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_ScheduleInform(#xmlElement{},#decoder{}) -> #schedule_inform{}.
parse_ScheduleInform(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ScheduleInform) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'DelaySeconds' ->
                                ScheduleInform#schedule_inform{delay_seconds = parse_DelaySeconds(Elem, State)};
    
                            'CommandKey' ->
                                ScheduleInform#schedule_inform{command_key = parse_CommandKey(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #schedule_inform{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_ScheduleInformResponse(#xmlElement{},#decoder{}) -> #schedule_inform_response{}.
parse_ScheduleInformResponse(_, _) -> #schedule_inform_response{}.
    
%% -spec parse_SetVouchers(#xmlElement{},#decoder{}) -> #set_vouchers{}.
parse_SetVouchers(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, SetVouchers) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'VoucherList' ->
                                SetVouchers#set_vouchers{voucher_list = parse_VoucherList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_vouchers{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_SetVouchersResponse(#xmlElement{},#decoder{}) -> #set_vouchers_response{}.
parse_SetVouchersResponse(_, _) -> #set_vouchers_response{}.
    
%% -spec parse_GetOptions(#xmlElement{},#decoder{}) -> #get_options{}.
parse_GetOptions(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetOptions) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'OptionName' ->
                                GetOptions#get_options{option_name = parse_OptionName(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_options{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_GetOptionsResponse(#xmlElement{},#decoder{}) -> #get_options_response{}.
parse_GetOptionsResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetOptionsResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'OptionList' ->
                                GetOptionsResponse#get_options_response{option_list = parse_OptionList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_options_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_Upload(#xmlElement{},#decoder{}) -> #upload{}.
parse_Upload(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, Upload) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'CommandKey' ->
                                Upload#upload{command_key = parse_CommandKey(Elem, State)};
    
                            'FileType' ->
                                Upload#upload{file_type = parse_FileType(Elem, State)};
    
                            'URL' ->
                                Upload#upload{url = parse_URL(Elem, State)};
    
                            'Username' ->
                                Upload#upload{username = parse_Username(Elem, State)};
    
                            'Password' ->
                                Upload#upload{password = parse_Password(Elem, State)};
    
                            'DelaySeconds' ->
                                Upload#upload{delay_seconds = parse_DelaySeconds(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #upload{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_UploadResponse(#xmlElement{},#decoder{}) -> #upload_response{}.
parse_UploadResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, UploadResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Status' ->
                                UploadResponse#upload_response{status = parse_Status(Elem, State)};
    
                            'StartTime' ->
                                UploadResponse#upload_response{start_time = parse_StartTime(Elem, State)};
    
                            'CompleteTime' ->
                                UploadResponse#upload_response{complete_time = parse_CompleteTime(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #upload_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_FactoryReset(#xmlElement{},#decoder{}) -> #factory_reset{}.
parse_FactoryReset(_, _) -> #factory_reset{}.
    
%% -spec parse_FactoryResetResponse(#xmlElement{},#decoder{}) -> #factory_reset_response{}.
parse_FactoryResetResponse(_, _) -> #factory_reset_response{}.
    
%% -spec parse_GetAllQueuedTransfers(#xmlElement{},#decoder{}) -> #get_all_queued_transfers{}.
parse_GetAllQueuedTransfers(_, _) -> #get_all_queued_transfers{}.
    
%% -spec parse_GetAllQueuedTransfersResponse(#xmlElement{},#decoder{}) -> #get_all_queued_transfers_response{}.
parse_GetAllQueuedTransfersResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, GetAllQueuedTransfersResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'TransferList' ->
                                GetAllQueuedTransfersResponse#get_all_queued_transfers_response{transfer_list = parse_TransferList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_all_queued_transfers_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_ScheduleDownload(#xmlElement{},#decoder{}) -> #schedule_download{}.
parse_ScheduleDownload(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ScheduleDownload) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'CommandKey' ->
                                ScheduleDownload#schedule_download{command_key = parse_CommandKey(Elem, State)};
    
                            'FileType' ->
                                ScheduleDownload#schedule_download{file_type = parse_FileType(Elem, State)};
    
                            'URL' ->
                                ScheduleDownload#schedule_download{url = parse_URL(Elem, State)};
    
                            'Username' ->
                                ScheduleDownload#schedule_download{username = parse_Username(Elem, State)};
    
                            'Password' ->
                                ScheduleDownload#schedule_download{password = parse_Password(Elem, State)};
    
                            'FileSize' ->
                                ScheduleDownload#schedule_download{file_size = parse_FileSize(Elem, State)};
    
                            'TargetFileName' ->
                                ScheduleDownload#schedule_download{target_file_name = parse_TargetFileName(Elem, State)};
    
                            'TimeWindowList' ->
                                ScheduleDownload#schedule_download{time_window_list = parse_TimeWindowList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #schedule_download{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_ScheduleDownloadResponse(#xmlElement{},#decoder{}) -> #schedule_download_response{}.
parse_ScheduleDownloadResponse(_, _) -> #schedule_download_response{}.
    
%% -spec parse_CancelTransfer(#xmlElement{},#decoder{}) -> #cancel_transfer{}.
parse_CancelTransfer(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, CancelTransfer) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'CommandKey' ->
                                CancelTransfer#cancel_transfer{command_key = parse_CommandKey(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #cancel_transfer{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_CancelTransferResponse(#xmlElement{},#decoder{}) -> #cancel_transfer_response{}.
parse_CancelTransferResponse(_, _) -> #cancel_transfer_response{}.
    
%% -spec parse_ChangeDUState(#xmlElement{},#decoder{}) -> #change_du_state{}.
parse_ChangeDUState(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, ChangeDUState) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Operations' ->
                                ChangeDUState#change_du_state{operations = parse_Operations(Elem, State)};
    
                            'CommandKey' ->
                                ChangeDUState#change_du_state{command_key = parse_CommandKey(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #change_du_state{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_ChangeDUStateResponse(#xmlElement{},#decoder{}) -> #change_du_state_response{}.
parse_ChangeDUStateResponse(_, _) -> #change_du_state_response{}.
    
%% -spec parse_Inform(#xmlElement{},#decoder{}) -> #inform{}.
parse_Inform(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, Inform) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'DeviceId' ->
                                Inform#inform{device_id = parse_DeviceId(Elem, State)};
    
                            'Event' ->
                                Inform#inform{event = parse_Event(Elem, State)};
    
                            'MaxEnvelopes' ->
                                Inform#inform{max_envelopes = parse_MaxEnvelopes(Elem, State)};
    
                            'CurrentTime' ->
                                Inform#inform{current_time = parse_CurrentTime(Elem, State)};
    
                            'RetryCount' ->
                                Inform#inform{retry_count = parse_RetryCount(Elem, State)};
    
                            'ParameterList' ->
                                Inform#inform{parameter_list = parse_ParameterList(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #inform{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_InformResponse(#xmlElement{},#decoder{}) -> #inform_response{}.
parse_InformResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, InformResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'MaxEnvelopes' ->
                                InformResponse#inform_response{max_envelopes = parse_MaxEnvelopes(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #inform_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_TransferComplete(#xmlElement{},#decoder{}) -> #transfer_complete{}.
parse_TransferComplete(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, TransferComplete) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'CommandKey' ->
                                TransferComplete#transfer_complete{command_key = parse_CommandKey(Elem, State)};
    
                            'FaultStruct' ->
                                TransferComplete#transfer_complete{fault_struct = parse_FaultStruct(Elem, State)};
    
                            'StartTime' ->
                                TransferComplete#transfer_complete{start_time = parse_StartTime(Elem, State)};
    
                            'CompleteTime' ->
                                TransferComplete#transfer_complete{complete_time = parse_CompleteTime(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #transfer_complete{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_TransferCompleteResponse(#xmlElement{},#decoder{}) -> #transfer_complete_response{}.
parse_TransferCompleteResponse(_, _) -> #transfer_complete_response{}.
    
%% -spec parse_AutonomousTransferComplete(#xmlElement{},#decoder{}) -> #autonomous_transfer_complete{}.
parse_AutonomousTransferComplete(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, AutonomousTransferComplete) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'AnnounceURL' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{announce_url = parse_AnnounceURL(Elem, State)};
    
                            'TransferURL' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{transfer_url = parse_TransferURL(Elem, State)};
    
                            'IsDownload' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{is_download = parse_IsDownload(Elem, State)};
    
                            'FileType' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{file_type = parse_FileType(Elem, State)};
    
                            'FileSize' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{file_size = parse_FileSize(Elem, State)};
    
                            'TargetFileName' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{target_file_name = parse_TargetFileName(Elem, State)};
    
                            'FaultStruct' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{fault_struct = parse_FaultStruct(Elem, State)};
    
                            'StartTime' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{start_time = parse_StartTime(Elem, State)};
    
                            'CompleteTime' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{complete_time = parse_CompleteTime(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #autonomous_transfer_complete{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_AutonomousTransferCompleteResponse(#xmlElement{},#decoder{}) -> #autonomous_transfer_complete_response{}.
parse_AutonomousTransferCompleteResponse(_, _) -> #autonomous_transfer_complete_response{}.
    
%% -spec parse_Kicked(#xmlElement{},#decoder{}) -> #kicked{}.
parse_Kicked(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, Kicked) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Command' ->
                                Kicked#kicked{command = parse_Command(Elem, State)};
    
                            'Referer' ->
                                Kicked#kicked{referer = parse_Referer(Elem, State)};
    
                            'Arg' ->
                                Kicked#kicked{arg = parse_Arg(Elem, State)};
    
                            'Next' ->
                                Kicked#kicked{next = parse_Next(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #kicked{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_KickedResponse(#xmlElement{},#decoder{}) -> #kicked_response{}.
parse_KickedResponse(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, KickedResponse) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'NextURL' ->
                                KickedResponse#kicked_response{next_url = parse_NextURL(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #kicked_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_RequestDownload(#xmlElement{},#decoder{}) -> #request_download{}.
parse_RequestDownload(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, RequestDownload) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'FileType' ->
                                RequestDownload#request_download{file_type = parse_FileType(Elem, State)};
    
                            'FileTypeArg' ->
                                RequestDownload#request_download{file_type_arg = parse_FileTypeArg(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #request_download{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_RequestDownloadResponse(#xmlElement{},#decoder{}) -> #request_download_response{}.
parse_RequestDownloadResponse(_, _) -> #request_download_response{}.
    
%% -spec parse_DUStateChangeComplete(#xmlElement{},#decoder{}) -> #du_state_change_complete{}.
parse_DUStateChangeComplete(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, DUStateChangeComplete) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Results' ->
                                DUStateChangeComplete#du_state_change_complete{results = parse_Results(Elem, State)};
    
                            'CommandKey' ->
                                DUStateChangeComplete#du_state_change_complete{command_key = parse_CommandKey(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #du_state_change_complete{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_DUStateChangeCompleteResponse(#xmlElement{},#decoder{}) -> #du_state_change_complete_response{}.
parse_DUStateChangeCompleteResponse(_, _) -> #du_state_change_complete_response{}.
    
%% -spec parse_AutonomousDUStateChangeComplete(#xmlElement{},#decoder{}) -> #autonomous_du_state_change_complete{}.
parse_AutonomousDUStateChangeComplete(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, AutonomousDUStateChangeComplete) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    
                            'Results' ->
                                AutonomousDUStateChangeComplete#autonomous_du_state_change_complete{results = parse_Results(Elem, State)};
    
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #autonomous_du_state_change_complete{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    
%% -spec parse_AutonomousDUStateChangeCompleteResponse(#xmlElement{},#decoder{}) -> #autonomous_du_state_change_complete_response{}.
parse_AutonomousDUStateChangeCompleteResponse(_, _) -> #autonomous_du_state_change_complete_response{}.
    
%% end
    
