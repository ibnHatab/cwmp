%%% File    : tr_soap_parser.erl
%%% Description : SOAP Structure Parser for TR-069 RPC


-module(tr_soap_parser).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-export([parser/1, parse/1]).

-export([main/1]).

-import(tr_soap_lib, [parse_error/2, get_local_name/1, get_QName/2, 
		      match_cwmp_ns_and_version/1, check_namespace/3]).

-import(tr_soap_types, [
			parse_AccessListChange/1,
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
			parse_Writable/1,

			parse_string/1,
			parse_boolean/1,
						%			 parse_int/1,
						%			 parse_dateTime/1,
			parse_base64/1,
			parse_anyURI/1,
			parse_attribete/3,
			parse_anySimpleType/1,
			parse_XS_Array/4
		       ]).


%%%-----------------------------------------------------------------------------
%%%        Local API
%%%-----------------------------------------------------------------------------

parse_parser_options([], State) ->
    State;
parse_parser_options([{version, Version} | Rest], State) ->
    parse_parser_options(Rest, State#parser{version=Version});
parse_parser_options([{object_hook, Hook} | Rest], State) ->
    parse_parser_options(Rest, State#parser{object_hook=Hook}).


%%%-----------------------------------------------------------------------------
%%%        SOAP Parser
%%%-----------------------------------------------------------------------------

%% @doc Create a parser/1 with the given options.
-spec parser([parser_option()]) -> function().
parser(Options) ->
    State = parse_parser_options(Options, #parser{}),
    fun (O) -> parse(O, State) end.

%% @doc Parse the given xmlElement to rpc_data terms.
-spec parse(#xmlElement{}) -> #rpc_data{}.
parse(S) ->
    parse(S, #parser{}).

-spec parse(#xmlElement{}, #parser{}) -> #rpc_data{}.
parse(Doc, S) ->
    try
        parse_Document(Doc, S)
    catch
        error:Error ->
	    Stacktrace = erlang:get_stacktrace(),
	    erlang:raise(error, Error, Stacktrace);
        %%FIXME: Probably thrown from return_error/2:
        throw:{error, {_Line, _Cause}} = Error->
	    Error
    end.

-spec parse_Document(#xmlElement{}, #parser{}) -> #rpc_data{}.
parse_Document(#xmlElement{name=QName, namespace = Namespace} = Doc, State) when is_tuple(Doc) ->
    Nss = parse_Namespace(Namespace),
    RefinedState = State#parser{ns=Nss, state=soap},
    Envelop = case get_local_name(QName) of
		  'Envelope' ->
		      parse_Envelope(Doc, RefinedState);
		  _ -> parse_error(Doc, RefinedState)
              end,
    #rpc_data{data = Envelop}.

-spec parse_Namespace(#xmlNamespace{}) -> #rpc_ns{}.
parse_Namespace(Nss) ->
    match_cwmp_ns_and_version(Nss).

-spec parse_Envelope(#xmlElement{}, #parser{}) -> #envelope{}.
parse_Envelope(#xmlElement{content = Content} = Doc, S) ->
    State = check_namespace('soap-env:Envelope', Doc, S),
    lists:foldl(fun(Elem, Envelop) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'Header' ->
                                Envelop#envelope{header = parse_Header(Elem, State)};
                            'Body' ->
                                Envelop#envelope{body = parse_Body(Elem, State)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #envelope{},
                lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_Header(#xmlElement{},#parser{}) -> #header{}.
parse_Header(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('soap-env:Header', E, S),
    lists:foldl(fun(Elem, Header) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'ID' ->
                                Header#header{id = parse_ID(Elem, State)};
                            'HoldRequests' ->
                                Header#header{hold_requests = parse_HoldRequests(Elem, State)};
                            'NoMoreRequests' ->
                                Header#header{no_more_requests = parse_boolean(Elem)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #header{},
                lists:filter(fun tr_soap_lib:xmlElement/1, Content)).


%%%-----------------------------------------------------------------------------
%%%        SOAP Body
%%%-----------------------------------------------------------------------------
-spec parse_Body(#xmlElement{}, #parser{}) -> [body_type()].
parse_Body(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('soap-env:Body', E, S),
    lists:map(fun(Elem) ->
		      case get_local_name(Elem#xmlElement.name) of
			  'Fault'                                       -> parse_SoapFault(Elem, State);
			  'GetRPCMethods'				-> parse_GetRPCMethods(Elem, State);
			  'GetRPCMethodsResponse'			-> parse_GetRPCMethodsResponse(Elem, State);
			  'SetParameterValues'				-> parse_SetParameterValues(Elem, State);
			  'SetParameterValuesResponse'			-> parse_SetParameterValuesResponse(Elem, State);
			  'GetParameterValues'				-> parse_GetParameterValues(Elem, State);
			  'GetParameterValuesResponse'			-> parse_GetParameterValuesResponse(Elem, State);
			  'GetParameterNames'				-> parse_GetParameterNames(Elem, State);
			  'GetParameterNamesResponse'			-> parse_GetParameterNamesResponse(Elem, State);
			  'SetParameterAttributes'			-> parse_SetParameterAttributes(Elem, State);
			  'SetParameterAttributesResponse'		-> parse_SetParameterAttributesResponse(Elem, State);
			  'GetParameterAttributes'			-> parse_GetParameterAttributes(Elem, State);
			  'GetParameterAttributesResponse'		-> parse_GetParameterAttributesResponse(Elem, State);
			  'AddObject'					-> parse_AddObject(Elem, State);
			  'AddObjectResponse'				-> parse_AddObjectResponse(Elem, State);
			  'DeleteObject'				-> parse_DeleteObject(Elem, State);
			  'DeleteObjectResponse'			-> parse_DeleteObjectResponse(Elem, State);
			  'Download'					-> parse_Download(Elem, State);
			  'DownloadResponse'				-> parse_DownloadResponse(Elem, State);
			  'Reboot'					-> parse_Reboot(Elem, State);
			  'RebootResponse'				-> parse_RebootResponse(Elem, State);
			  'GetQueuedTransfers'				-> parse_GetQueuedTransfers(Elem, State);
			  'GetQueuedTransfersResponse'			-> parse_GetQueuedTransfersResponse(Elem, State);
			  'ScheduleInform'				-> parse_ScheduleInform(Elem, State);
			  'ScheduleInformResponse'			-> parse_ScheduleInformResponse(Elem, State);
			  'SetVouchers'					-> parse_SetVouchers(Elem, State);
			  'SetVouchersResponse'				-> parse_SetVouchersResponse(Elem, State);
			  'GetOptions'					-> parse_GetOptions(Elem, State);
			  'GetOptionsResponse'				-> parse_GetOptionsResponse(Elem, State);
			  'Upload'					-> parse_Upload(Elem, State);
			  'UploadResponse'				-> parse_UploadResponse(Elem, State);
			  'FactoryReset'				-> parse_FactoryReset(Elem, State);
			  'FactoryResetResponse'			-> parse_FactoryResetResponse(Elem, State);
			  'GetAllQueuedTransfers'			-> parse_GetAllQueuedTransfers(Elem, State);
			  'GetAllQueuedTransfersResponse'		-> parse_GetAllQueuedTransfersResponse(Elem, State);
			  'ScheduleDownload'				-> parse_ScheduleDownload(Elem, State);
			  'ScheduleDownloadResponse'			-> parse_ScheduleDownloadResponse(Elem, State);
			  'CancelTransfer'				-> parse_CancelTransfer(Elem, State);
			  'CancelTransferResponse'			-> parse_CancelTransferResponse(Elem, State);
			  'ChangeDUState'				-> parse_ChangeDUState(Elem, State);
			  'ChangeDUStateResponse'			-> parse_ChangeDUStateResponse(Elem, State);
			  'Inform'					-> parse_Inform(Elem, State);
			  'InformResponse'				-> parse_InformResponse(Elem, State);
			  'TransferComplete'				-> parse_TransferComplete(Elem, State);
			  'TransferCompleteResponse'			-> parse_TransferCompleteResponse(Elem, State);
			  'AutonomousTransferComplete'			-> parse_AutonomousTransferComplete(Elem, State);
			  'AutonomousTransferCompleteResponse'		-> parse_AutonomousTransferCompleteResponse(Elem, State);
			  'Kicked'					-> parse_Kicked(Elem, State);
			  'KickedResponse'				-> parse_KickedResponse(Elem, State);
			  'RequestDownload'				-> parse_RequestDownload(Elem, State);
			  'RequestDownloadResponse'			-> parse_RequestDownloadResponse(Elem, State);
			  'DUStateChangeComplete'			-> parse_DUStateChangeComplete(Elem, State);
			  'DUStateChangeCompleteResponse'		-> parse_DUStateChangeCompleteResponse(Elem, State);
			  'AutonomousDUStateChangeComplete'		-> parse_AutonomousDUStateChangeComplete(Elem, State);
			  'AutonomousDUStateChangeCompleteResponse'	-> parse_AutonomousDUStateChangeCompleteResponse(Elem, State);
			  _ ->
			      parse_error(Elem, State)

		      end
	      end,
	      lists:filter(fun tr_soap_lib:xmlElement/1, Content)).


%%%-----------------------------------------------------------------------------
%%   Complex Data
%%%-----------------------------------------------------------------------------

-spec parse_ID(#xmlElement{},#parser{}) -> #id{}.
parse_ID(Elem, #parser{ns=Nss} = _State) ->
    Value = parse_string(Elem),
    QName = get_QName('mustUnderstand', Nss#rpc_ns.ns_envelop),
    MustuNderstand = parse_attribete(Elem, QName, boolean),
    #id{mustUnderstand = MustuNderstand, value = Value}.

-spec parse_HoldRequests(#xmlElement{},#parser{}) -> #hold_requests{}.
parse_HoldRequests(Elem, #parser{ns=Nss} = _State) ->
    Value = parse_boolean(Elem),
    QName = get_QName('mustUnderstand', Nss#rpc_ns.ns_envelop),
    MustuNderstand = parse_attribete(Elem, QName, boolean),
    #hold_requests{mustUnderstand = MustuNderstand, value = Value}.

-spec parse_TransferCompleteFaultStruct(#xmlElement{},#parser{}) -> #transfer_complete_fault_struct{}.
parse_TransferCompleteFaultStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:TransferCompleteFaultStruct', E, S),
    lists:foldl(fun(Elem, TransferCompleteFaultStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

			    'FaultCode' ->
				TransferCompleteFaultStruct#transfer_complete_fault_struct{fault_code = parse_FaultCode(Elem)};

			    'FaultString' ->
				TransferCompleteFaultStruct#transfer_complete_fault_struct{fault_string = parse_FaultString(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #transfer_complete_fault_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_DeploymentUnitFaultStruct(#xmlElement{},#parser{}) -> #deployment_unit_fault_struct{}.
parse_DeploymentUnitFaultStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:DeploymentUnitFaultStruct', E, S),
    lists:foldl(fun(Elem, DeploymentUnitFaultStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

			    'FaultCode' ->
				DeploymentUnitFaultStruct#deployment_unit_fault_struct{fault_code = parse_FaultCode(Elem)};

			    'FaultString' ->
				DeploymentUnitFaultStruct#deployment_unit_fault_struct{fault_string = parse_FaultString(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #deployment_unit_fault_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_ParameterNames(#xmlElement{},#parser{}) -> [string()].
parse_ParameterNames(E, S) ->
    NewState = check_namespace('cwmp:ParameterNames', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'string' ->
				   parse_string(Elem);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:string', NewState).

-spec parse_ParameterValueStruct(#xmlElement{},#parser{}) -> #parameter_value_struct{}.
parse_ParameterValueStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:ParameterValueStruct', E, S),
    lists:foldl(fun(Elem, ParameterValueStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Name' ->
                                ParameterValueStruct#parameter_value_struct{name = parse_Name(Elem)};

                            'Value' ->
                                ParameterValueStruct#parameter_value_struct{value = parse_anySimpleType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_value_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_ParameterValueList(#xmlElement{},#parser{}) -> [#parameter_value_struct{}].
parse_ParameterValueList(E, S) ->
    NewState = check_namespace('cwmp:ParameterValueList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'ParameterValueStruct' ->
				   parse_ParameterValueStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:ParameterValueStruct', NewState).

-spec parse_MethodList(#xmlElement{},#parser{}) -> [string()].
parse_MethodList(E, S) ->
    NewState = check_namespace('cwmp:MethodList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'string' ->
				   parse_string(Elem);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:string', NewState).

-spec parse_DeviceIdStruct(#xmlElement{},#parser{}) -> #device_id_struct{}.
parse_DeviceIdStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:DeviceIdStruct', E, S),
    lists:foldl(fun(Elem, DeviceIdStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Manufacturer' ->
                                DeviceIdStruct#device_id_struct{manufacturer = parse_Manufacturer(Elem)};

                            'OUI' ->
                                DeviceIdStruct#device_id_struct{oui = parse_OUI(Elem)};

                            'ProductClass' ->
                                DeviceIdStruct#device_id_struct{product_class = parse_ProductClass(Elem)};

                            'SerialNumber' ->
                                DeviceIdStruct#device_id_struct{serial_number = parse_SerialNumber(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #device_id_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_EventStruct(#xmlElement{},#parser{}) -> #event_struct{}.
parse_EventStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:EventStruct', E, S),
    lists:foldl(fun(Elem, EventStruct) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'EventCode' ->
                                EventStruct#event_struct{event_code = parse_EventCodeType(Elem, State)};
                            'CommandKey' ->
                                EventStruct#event_struct{command_key = parse_CommandKeyType(Elem)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #event_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_EventList(#xmlElement{},#parser{}) -> [#event_struct{}].
parse_EventList(E, S) ->
    NewState = check_namespace('cwmp:EventList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'EventStruct' ->
				   parse_EventStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:EventStruct', NewState).

-spec parse_ParameterInfoStruct(#xmlElement{},#parser{}) -> #parameter_info_struct{}.
parse_ParameterInfoStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:ParameterInfoStruct', E, S),
    lists:foldl(fun(Elem, ParameterInfoStruct) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'Name' ->
                                ParameterInfoStruct#parameter_info_struct{name = parse_Name(Elem)};

                            'Writable' ->
                                ParameterInfoStruct#parameter_info_struct{writable = parse_Writable(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_info_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_ParameterInfoList(#xmlElement{},#parser{}) -> [#parameter_info_struct{}].
parse_ParameterInfoList(E, S) ->
    NewState = check_namespace('cwmp:ParameterInfoList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'ParameterInfoStruct' ->
				   parse_ParameterInfoStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:ParameterInfoStruct', NewState).

-spec parse_AccessList(#xmlElement{},#parser{}) -> [access_list_value_type()].
parse_AccessList(E, S) ->
    NewState = check_namespace('cwmp:AccessList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'string' ->
				   parse_string(Elem);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:string', NewState).


-spec parse_SetParameterAttributesStruct(#xmlElement{},#parser{}) -> #set_parameter_attributes_struct{}.
parse_SetParameterAttributesStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:SetParameterAttributesStruct', E, S),
    lists:foldl(fun(Elem, SetParameterAttributesStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Name' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{name = parse_Name(Elem)};

                            'NotificationChange' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{notification_change = parse_NotificationChange(Elem)};

                            'Notification' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{notification = parse_Notification(Elem)};

                            'AccessListChange' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{access_list_change = parse_AccessListChange(Elem)};

                            'AccessList' ->
                                SetParameterAttributesStruct#set_parameter_attributes_struct{access_list = parse_AccessList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_attributes_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_SetParameterAttributesList(#xmlElement{},#parser{}) ->  [#set_parameter_attributes_struct{}].
parse_SetParameterAttributesList(E, S) ->
    NewState = check_namespace('cwmp:SetParameterAttributesList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'SetParameterAttributesStruct' ->
				   parse_SetParameterAttributesStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:SetParameterAttributesStruct', NewState).


-spec parse_ParameterAttributeStruct(#xmlElement{},#parser{}) -> #parameter_attribute_struct{}.
parse_ParameterAttributeStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:ParameterAttributeStruct', E, S),
    lists:foldl(fun(Elem, ParameterAttributeStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Name' ->
                                ParameterAttributeStruct#parameter_attribute_struct{name = parse_Name(Elem)};

                            'Notification' ->
                                ParameterAttributeStruct#parameter_attribute_struct{notification = parse_Notification(Elem)};

                            'AccessList' ->
                                ParameterAttributeStruct#parameter_attribute_struct{access_list = parse_AccessList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #parameter_attribute_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_ParameterAttributeList(#xmlElement{},#parser{}) -> [#parameter_attribute_struct{}].
parse_ParameterAttributeList(E, S) ->
    NewState = check_namespace('cwmp:ParameterAttributeList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'ParameterAttributeStruct' ->
				   parse_ParameterAttributeStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:ParameterAttributeStruct', NewState).


-spec parse_TimeWindowStruct(#xmlElement{},#parser{}) -> #time_window_struct{}.
parse_TimeWindowStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:TimeWindowStruct', E, S),
    lists:foldl(fun(Elem, TimeWindowStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'WindowStart' ->
                                TimeWindowStruct#time_window_struct{window_start = parse_WindowStart(Elem)};

                            'WindowEnd' ->
                                TimeWindowStruct#time_window_struct{window_end = parse_WindowEnd(Elem)};

                            'WindowMode' ->
                                TimeWindowStruct#time_window_struct{window_mode = parse_WindowMode(Elem)};

                            'UserMessage' ->
                                TimeWindowStruct#time_window_struct{user_message = parse_UserMessage(Elem)};

                            'MaxRetries' ->
                                TimeWindowStruct#time_window_struct{max_retries = parse_MaxRetries(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #time_window_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_TimeWindowList(#xmlElement{},#parser{}) ->  [#time_window_struct{}].
parse_TimeWindowList(E, S) ->
    NewState = check_namespace('cwmp:TimeWindowList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'TimeWindowStruct' ->
				   parse_TimeWindowStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:TimeWindowStruct', NewState).

-spec parse_QueuedTransferStruct(#xmlElement{},#parser{}) -> #queued_transfer_struct{}.
parse_QueuedTransferStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:QueuedTransferStruct', E, S),
    lists:foldl(fun(Elem, QueuedTransferStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'CommandKey' ->
                                QueuedTransferStruct#queued_transfer_struct{command_key = parse_CommandKeyType(Elem)};

                            'State' ->
                                QueuedTransferStruct#queued_transfer_struct{state = parse_State(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #queued_transfer_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_TransferList(#xmlElement{},#parser{}) ->  [#queued_transfer_struct{}].
parse_TransferList(E, S) ->
    NewState = check_namespace('cwmp:TransferList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'QueuedTransferStruct' ->
				   parse_QueuedTransferStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:QueuedTransferStruct', NewState).


-spec parse_AllQueuedTransferStruct(#xmlElement{},#parser{}) -> #all_queued_transfer_struct{}.
parse_AllQueuedTransferStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:AllQueuedTransferStruct', E, S),
    lists:foldl(fun(Elem, AllQueuedTransferStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'CommandKey' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{command_key = parse_CommandKeyType(Elem)};

                            'State' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{state = parse_State(Elem)};

                            'IsDownload' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{is_download = parse_IsDownload(Elem)};

                            'FileType' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{file_type = parse_FileType('TransferFileType', Elem)};

                            'FileSize' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{file_size = parse_FileSize(Elem)};

                            'TargetFileName' ->
                                AllQueuedTransferStruct#all_queued_transfer_struct{target_file_name = parse_TargetFileName(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #all_queued_transfer_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_AllTransferList(#xmlElement{},#parser{}) -> [#all_queued_transfer_struct{}].
parse_AllTransferList(E, S) ->
    NewState = check_namespace('cwmp:AllTransferList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'AllQueuedTransferStruct' ->
				   parse_AllQueuedTransferStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:AllQueuedTransferStruct', NewState).

-spec parse_OperationStruct(#xmlElement{},#parser{}) -> [operation_struct()].
parse_OperationStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:Operations', E, S),
    [case get_local_name(Elem#xmlElement.name) of
	 'InstallOpStruct' ->
	     parse_InstallOpStruct(Elem, State);
	 'UpdateOpStruct' ->
	     parse_UpdateOpStruct(Elem, State);
	 'UninstallOpStruct' ->
	     parse_UninstallOpStruct(Elem, State);
	 _ ->
	     parse_error(Elem, State)
     end || Elem <- Content, tr_soap_lib:xmlElement(Elem)].

-spec parse_InstallOpStruct(#xmlElement{},#parser{}) -> #install_op_struct{}.
parse_InstallOpStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:InstallOpStruct', E, S),
    lists:foldl(fun(Elem, InstallOpStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'URL' ->
                                InstallOpStruct#install_op_struct{url = parse_URL(Elem)};

                            'UUID' ->
                                InstallOpStruct#install_op_struct{uuid = parse_UUID(Elem)};

                            'Username' ->
                                InstallOpStruct#install_op_struct{username = parse_Username(Elem)};

                            'Password' ->
                                InstallOpStruct#install_op_struct{password = parse_Password(Elem)};

                            'ExecutionEnvRef' ->
                                InstallOpStruct#install_op_struct{execution_env_ref = parse_ExecutionEnvRef(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #install_op_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_UpdateOpStruct(#xmlElement{},#parser{}) -> #update_op_struct{}.
parse_UpdateOpStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:UpdateOpStruct', E, S),
    lists:foldl(fun(Elem, UpdateOpStruct) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'UUID' ->
                                UpdateOpStruct#update_op_struct{uuid = parse_UUID(Elem)};

                            'Version' ->
                                UpdateOpStruct#update_op_struct{version = parse_Version(Elem)};

                            'URL' ->
                                UpdateOpStruct#update_op_struct{url = parse_URL(Elem)};

                            'Username' ->
                                UpdateOpStruct#update_op_struct{username = parse_Username(Elem)};

                            'Password' ->
                                UpdateOpStruct#update_op_struct{password = parse_Password(Elem)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #update_op_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_UninstallOpStruct(#xmlElement{},#parser{}) -> #uninstall_op_struct{}.
parse_UninstallOpStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:UninstallOpStruct', E, S),
    lists:foldl(fun(Elem, UninstallOpStruct) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'UUID' ->
				UninstallOpStruct#uninstall_op_struct{uuid = parse_UUID(Elem)};
                            'Version' ->
                                UninstallOpStruct#uninstall_op_struct{version = parse_Version(Elem)};
                            'ExecutionEnvRef' ->
                                UninstallOpStruct#uninstall_op_struct{execution_env_ref = parse_ExecutionEnvRef(Elem)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #uninstall_op_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_OpResultStruct(#xmlElement{},#parser{}) -> #op_result_struct{}.
parse_OpResultStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:OpResultStruct', E, S),
    lists:foldl(fun(Elem, OpResultStruct) ->
			case get_local_name(Elem#xmlElement.name) of

                            'UUID' ->
                                OpResultStruct#op_result_struct{uuid = parse_UUID(Elem)};

                            'DeploymentUnitRef' ->
                                OpResultStruct#op_result_struct{deployment_unit_ref = parse_DeploymentUnitRef(Elem)};

                            'Version' ->
                                OpResultStruct#op_result_struct{version = parse_Version(Elem)};

                            'CurrentState' ->
                                OpResultStruct#op_result_struct{current_state = parse_DeploymentUnitState(Elem)};

                            'Resolved' ->
                                OpResultStruct#op_result_struct{resolved = parse_Resolved(Elem)};

                            'ExecutionUnitRefList' ->
                                OpResultStruct#op_result_struct{execution_unit_ref_list = parse_ExecutionUnitRefList(Elem)};

                            'StartTime' ->
                                OpResultStruct#op_result_struct{start_time = parse_StartTime(Elem)};

                            'CompleteTime' ->
                                OpResultStruct#op_result_struct{complete_time = parse_CompleteTime(Elem)};

                            'Fault' ->
                                OpResultStruct#op_result_struct{fault = parse_DeploymentUnitFaultStruct(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #op_result_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_AutonOpResultStruct(#xmlElement{},#parser{}) -> #auton_op_result_struct{}.
parse_AutonOpResultStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:AutonOpResultStruct', E, S),
    lists:foldl(fun(Elem, AutonOpResultStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'OperationPerformed' ->
                                AutonOpResultStruct#auton_op_result_struct{operation_performed = parse_DeploymentUnitOperationType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #auton_op_result_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_VoucherList(#xmlElement{},#parser{}) -> [binary()].
parse_VoucherList(E, S) ->
    NewState = check_namespace('cwmp:VoucherList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'base64' ->
				   parse_base64(Elem);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:base64', NewState).

-spec parse_OptionStruct(#xmlElement{},#parser{}) -> #option_struct{}.
parse_OptionStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:OptionStruct', E, S),
    lists:foldl(fun(Elem, OptionStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'OptionName' ->
                                OptionStruct#option_struct{option_name = parse_OptionName(Elem)};

                            'VoucherSN' ->
                                OptionStruct#option_struct{voucher_sn = parse_VoucherSN(Elem)};

                            'State' ->
                                OptionStruct#option_struct{state = parse_State(Elem)};

                            'Mode' ->
                                OptionStruct#option_struct{mode = parse_Mode(Elem)};

                            'StartDate' ->
                                OptionStruct#option_struct{start_date = parse_StartDate(Elem)};

                            'ExpirationDate' ->
                                OptionStruct#option_struct{expiration_date = parse_ExpirationDate(Elem)};

                            'IsTransferable' ->
                                OptionStruct#option_struct{is_transferable = parse_IsTransferable(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #option_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_OptionList(#xmlElement{},#parser{}) -> [#option_struct{}].
parse_OptionList(E, S) ->
    NewState = check_namespace('cwmp:OptionList', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'OptionStruct' ->
				   parse_OptionStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:OptionStruct', NewState).

-spec parse_ArgStruct(#xmlElement{},#parser{}) -> #arg_struct{}.
parse_ArgStruct(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:ArgStruct', E, S),
    lists:foldl(fun(Elem, ArgStruct) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Name' ->
                                ArgStruct#arg_struct{name = parse_Name(Elem)};

                            'Value' ->
                                ArgStruct#arg_struct{value = parse_Value(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #arg_struct{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_FileTypeArg(#xmlElement{},#parser{}) ->  [#arg_struct{}].
parse_FileTypeArg(E, S) ->
    NewState = check_namespace('cwmp:FileTypeArg', E, S),
    parse_XS_Array(fun (Elem, State) ->
			   case get_local_name(Elem#xmlElement.name) of
			       'ArgStruct' ->
				   parse_ArgStruct(Elem, State);
			       _ ->
				   parse_error(Elem, State)
			   end
		   end,
		   E, 'cwmp:string', NewState).

-spec parse_SoapFaultDetail(#xmlElement{},#parser{}) -> #fault{}.
parse_SoapFaultDetail(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('soap-env:detail', E, S),
    case lists:filter(fun tr_soap_lib:xmlElement/1, Content) of
	[] ->
	    undefined;
	[#xmlElement{name=Name} = Elem | _Rest] ->
	    case get_local_name(Name) of
		'Fault' ->
		    parse_Fault(Elem, State);
		_ ->
		    parse_error(Elem, State)
	    end

    end.

-spec parse_SoapFault(#xmlElement{},#parser{}) -> #soap_fault{}.
parse_SoapFault(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('soap-env:Fault', E, S),
    lists:foldl(fun(Elem, Fault) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'faultcode' ->
                                Fault#soap_fault{faultcode = parse_string(Elem)};
                            'faultstring' ->
                                Fault#soap_fault{faultstring = parse_string(Elem)};
                            'faultactor' ->
                                Fault#soap_fault{faultactor = parse_anyURI(Elem)};
                            'detail' ->
                                Fault#soap_fault{detail = parse_SoapFaultDetail(Elem, State)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #soap_fault{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_SetParameterValuesFault(#xmlElement{},#parser{}) -> #set_parameter_values_fault{}.
parse_SetParameterValuesFault(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:SetParameterValuesFault', E, S),
    lists:foldl(fun(Elem, Fault) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'ParameterName' ->
                                Fault#set_parameter_values_fault{parameter_name = parse_string(Elem)};
                            'FaultCode' ->
                                Fault#set_parameter_values_fault{fault_code = parse_FaultCode(Elem)};
                            'FaultString' ->
                                Fault#set_parameter_values_fault{fault_string = parse_FaultString(Elem)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_values_fault{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).



-spec parse_Fault(#xmlElement{},#parser{}) -> #fault{}.
parse_Fault(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:Fault', E, S),
    lists:foldl(fun(Elem, Fault) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'FaultCode' ->
                                Fault#fault{fault_code = parse_FaultCode(Elem)};
			    'FaultString' ->
                                Fault#fault{fault_string = parse_FaultString(Elem)};
			    'SetParameterValuesFault' ->
                                Fault#fault{set_parameter_values_fault = parse_SetParameterValuesFault(Elem, State)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #fault{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_GetRPCMethods(#xmlElement{},#parser{}) -> #get_rpc_methods{}.
parse_GetRPCMethods(_, _) -> #get_rpc_methods{}.

-spec parse_GetRPCMethodsResponse(#xmlElement{},#parser{}) -> #get_rpc_methods_response{}.
parse_GetRPCMethodsResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetRPCMethodsResponse', E, S),
    lists:foldl(fun(Elem, GetRPCMethodsResponse) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'MethodList' ->
                                GetRPCMethodsResponse#get_rpc_methods_response{method_list = parse_MethodList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_rpc_methods_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_SetParameterValues(#xmlElement{},#parser{}) -> #set_parameter_values{}.
parse_SetParameterValues(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:SetParameterValues', E, S),
    lists:foldl(fun(Elem, SetParameterValues) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'ParameterList' ->
                                SetParameterValues#set_parameter_values{parameter_list = parse_ParameterValueList(Elem, State)};

                            'ParameterKey' ->
                                SetParameterValues#set_parameter_values{parameter_key = parse_ParameterKeyType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_values{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_SetParameterValuesResponse(#xmlElement{},#parser{}) -> #set_parameter_values_response{}.
parse_SetParameterValuesResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:SetParameterValuesResponse', E, S),
    lists:foldl(fun(Elem, SetParameterValuesResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Status' ->
                                SetParameterValuesResponse#set_parameter_values_response{status = parse_Status(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_values_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_GetParameterValues(#xmlElement{},#parser{}) -> #get_parameter_values{}.
parse_GetParameterValues(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetParameterValues', E, S),
    lists:foldl(fun(Elem, GetParameterValues) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'ParameterNames' ->
                                GetParameterValues#get_parameter_values{parameter_names = parse_ParameterNames(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_values{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_GetParameterValuesResponse(#xmlElement{},#parser{}) -> #get_parameter_values_response{}.
parse_GetParameterValuesResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetParameterValuesResponse', E, S),
    lists:foldl(fun(Elem, GetParameterValuesResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'ParameterList' ->
                                GetParameterValuesResponse#get_parameter_values_response{parameter_list = parse_ParameterValueList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_values_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_GetParameterNames(#xmlElement{},#parser{}) -> #get_parameter_names{}.
parse_GetParameterNames(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetParameterNames', E, S),
    lists:foldl(fun(Elem, GetParameterNames) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'ParameterPath' ->
                                GetParameterNames#get_parameter_names{parameter_path = parse_ParameterPath(Elem)};

                            'NextLevel' ->
                                GetParameterNames#get_parameter_names{next_level = parse_NextLevel(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_names{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_GetParameterNamesResponse(#xmlElement{},#parser{}) -> #get_parameter_names_response{}.
parse_GetParameterNamesResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetParameterNamesResponse', E, S),
    lists:foldl(fun(Elem, GetParameterNamesResponse) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'ParameterList' ->
                                GetParameterNamesResponse#get_parameter_names_response{parameter_list = parse_ParameterInfoList(Elem, State)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_names_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_SetParameterAttributes(#xmlElement{},#parser{}) -> #set_parameter_attributes{}.
parse_SetParameterAttributes(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:SetParameterAttributes', E, S),
    lists:foldl(fun(Elem, SetParameterAttributes) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'ParameterList' ->
                                SetParameterAttributes#set_parameter_attributes{parameter_list = parse_SetParameterAttributesList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_parameter_attributes{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_SetParameterAttributesResponse(#xmlElement{},#parser{}) -> #set_parameter_attributes_response{}.
parse_SetParameterAttributesResponse(_, _) -> #set_parameter_attributes_response{}.

-spec parse_GetParameterAttributes(#xmlElement{},#parser{}) -> #get_parameter_attributes{}.
parse_GetParameterAttributes(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetParameterAttributes', E, S),
    lists:foldl(fun(Elem, GetParameterAttributes) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'ParameterNames' ->
                                GetParameterAttributes#get_parameter_attributes{parameter_names = parse_ParameterNames(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_attributes{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_GetParameterAttributesResponse(#xmlElement{},#parser{}) -> #get_parameter_attributes_response{}.
parse_GetParameterAttributesResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetParameterAttributesResponse', E, S),
    lists:foldl(fun(Elem, GetParameterAttributesResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'ParameterList' ->
                                GetParameterAttributesResponse#get_parameter_attributes_response{parameter_list = parse_ParameterAttributeList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_parameter_attributes_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_AddObject(#xmlElement{},#parser{}) -> #add_object{}.
parse_AddObject(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:AddObject', E, S),
    lists:foldl(fun(Elem, AddObject) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'ObjectName' ->
                                AddObject#add_object{object_name = parse_ObjectNameType(Elem)};

                            'ParameterKey' ->
                                AddObject#add_object{parameter_key = parse_ParameterKeyType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #add_object{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_AddObjectResponse(#xmlElement{},#parser{}) -> #add_object_response{}.
parse_AddObjectResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:AddObjectResponse', E, S),
    lists:foldl(fun(Elem, AddObjectResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'InstanceNumber' ->
                                AddObjectResponse#add_object_response{instance_number = parse_InstanceNumber(Elem)};

                            'Status' ->
                                AddObjectResponse#add_object_response{status = parse_Status(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #add_object_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_DeleteObject(#xmlElement{},#parser{}) -> #delete_object{}.
parse_DeleteObject(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:DeleteObject', E, S),
    lists:foldl(fun(Elem, DeleteObject) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'ObjectName' ->
                                DeleteObject#delete_object{object_name = parse_ObjectNameType(Elem)};

                            'ParameterKey' ->
                                DeleteObject#delete_object{parameter_key = parse_ParameterKeyType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #delete_object{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_DeleteObjectResponse(#xmlElement{},#parser{}) -> #delete_object_response{}.
parse_DeleteObjectResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:DeleteObjectResponse', E, S),
    lists:foldl(fun(Elem, DeleteObjectResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Status' ->
                                DeleteObjectResponse#delete_object_response{status = parse_Status(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #delete_object_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_Download(#xmlElement{},#parser{}) -> #download{}.
parse_Download(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:Download', E, S),
    lists:foldl(fun(Elem, Download) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'CommandKey' ->
                                Download#download{command_key = parse_CommandKeyType(Elem)};

                            'FileType' ->
                                Download#download{file_type = parse_FileType('DownloadFileType', Elem)};

                            'URL' ->
                                Download#download{url = parse_URL(Elem)};

                            'Username' ->
                                Download#download{username = parse_Username(Elem)};

                            'Password' ->
                                Download#download{password = parse_Password(Elem)};

                            'FileSize' ->
                                Download#download{file_size = parse_FileSize(Elem)};

                            'TargetFileName' ->
                                Download#download{target_file_name = parse_TargetFileName(Elem)};

                            'DelaySeconds' ->
                                Download#download{delay_seconds = parse_DelaySeconds(Elem)};

                            'SuccessURL' ->
                                Download#download{success_url = parse_SuccessURL(Elem)};

                            'FailureURL' ->
                                Download#download{failure_url = parse_FailureURL(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #download{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_DownloadResponse(#xmlElement{},#parser{}) -> #download_response{}.
parse_DownloadResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:DownloadResponse', E, S),
    lists:foldl(fun(Elem, DownloadResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Status' ->
                                DownloadResponse#download_response{status = parse_Status(Elem)};

                            'StartTime' ->
                                DownloadResponse#download_response{start_time = parse_StartTime(Elem)};

                            'CompleteTime' ->
                                DownloadResponse#download_response{complete_time = parse_CompleteTime(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #download_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_Reboot(#xmlElement{},#parser{}) -> #reboot{}.
parse_Reboot(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:Reboot', E, S),
    lists:foldl(fun(Elem, Reboot) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'CommandKey' ->
                                Reboot#reboot{command_key = parse_CommandKeyType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #reboot{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_RebootResponse(#xmlElement{},#parser{}) -> #reboot_response{}.
parse_RebootResponse(_, _) -> #reboot_response{}.

-spec parse_GetQueuedTransfers(#xmlElement{},#parser{}) -> #get_queued_transfers{}.
parse_GetQueuedTransfers(_, _) -> #get_queued_transfers{}.

-spec parse_GetQueuedTransfersResponse(#xmlElement{},#parser{}) -> #get_queued_transfers_response{}.
parse_GetQueuedTransfersResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetQueuedTransfersResponse', E, S),
    lists:foldl(fun(Elem, GetQueuedTransfersResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'TransferList' ->
                                GetQueuedTransfersResponse#get_queued_transfers_response{transfer_list = parse_TransferList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_queued_transfers_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_ScheduleInform(#xmlElement{},#parser{}) -> #schedule_inform{}.
parse_ScheduleInform(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:ScheduleInform', E, S),
    lists:foldl(fun(Elem, ScheduleInform) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'DelaySeconds' ->
                                ScheduleInform#schedule_inform{delay_seconds = parse_DelaySeconds(Elem)};

                            'CommandKey' ->
                                ScheduleInform#schedule_inform{command_key = parse_CommandKeyType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #schedule_inform{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_ScheduleInformResponse(#xmlElement{},#parser{}) -> #schedule_inform_response{}.
parse_ScheduleInformResponse(_, _) -> #schedule_inform_response{}.

-spec parse_SetVouchers(#xmlElement{},#parser{}) -> #set_vouchers{}.
parse_SetVouchers(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:SetVouchers', E, S),
    lists:foldl(fun(Elem, SetVouchers) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'VoucherList' ->
                                SetVouchers#set_vouchers{voucher_list = parse_VoucherList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #set_vouchers{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_SetVouchersResponse(#xmlElement{},#parser{}) -> #set_vouchers_response{}.
parse_SetVouchersResponse(_, _) -> #set_vouchers_response{}.

-spec parse_GetOptions(#xmlElement{},#parser{}) -> #get_options{}.
parse_GetOptions(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetOptions', E, S),
    lists:foldl(fun(Elem, GetOptions) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'OptionName' ->
                                GetOptions#get_options{option_name = parse_OptionName(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_options{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_GetOptionsResponse(#xmlElement{},#parser{}) -> #get_options_response{}.
parse_GetOptionsResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetOptionsResponse', E, S),
    lists:foldl(fun(Elem, GetOptionsResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'OptionList' ->
                                GetOptionsResponse#get_options_response{option_list = parse_OptionList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_options_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_Upload(#xmlElement{},#parser{}) -> #upload{}.
parse_Upload(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:Upload', E, S),
    lists:foldl(fun(Elem, Upload) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'CommandKey' ->
                                Upload#upload{command_key = parse_CommandKeyType(Elem)};

                            'FileType' ->
                                Upload#upload{file_type = parse_FileType('UploadFileType', Elem)};

                            'URL' ->
                                Upload#upload{url = parse_URL(Elem)};

                            'Username' ->
                                Upload#upload{username = parse_Username(Elem)};

                            'Password' ->
                                Upload#upload{password = parse_Password(Elem)};

                            'DelaySeconds' ->
                                Upload#upload{delay_seconds = parse_DelaySeconds(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #upload{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_UploadResponse(#xmlElement{},#parser{}) -> #upload_response{}.
parse_UploadResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:UploadResponse', E, S),
    lists:foldl(fun(Elem, UploadResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Status' ->
                                UploadResponse#upload_response{status = parse_Status(Elem)};

                            'StartTime' ->
                                UploadResponse#upload_response{start_time = parse_StartTime(Elem)};

                            'CompleteTime' ->
                                UploadResponse#upload_response{complete_time = parse_CompleteTime(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #upload_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_FactoryReset(#xmlElement{},#parser{}) -> #factory_reset{}.
parse_FactoryReset(_, _) -> #factory_reset{}.

-spec parse_FactoryResetResponse(#xmlElement{},#parser{}) -> #factory_reset_response{}.
parse_FactoryResetResponse(_, _) -> #factory_reset_response{}.

-spec parse_GetAllQueuedTransfers(#xmlElement{},#parser{}) -> #get_all_queued_transfers{}.
parse_GetAllQueuedTransfers(_, _) -> #get_all_queued_transfers{}.

-spec parse_GetAllQueuedTransfersResponse(#xmlElement{},#parser{}) -> #get_all_queued_transfers_response{}.
parse_GetAllQueuedTransfersResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:GetAllQueuedTransfersResponse', E, S),
    lists:foldl(fun(Elem, GetAllQueuedTransfersResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'TransferList' ->
                                GetAllQueuedTransfersResponse#get_all_queued_transfers_response{transfer_list = parse_AllTransferList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #get_all_queued_transfers_response{},
		lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_ScheduleDownload(#xmlElement{},#parser{}) -> #schedule_download{}.
parse_ScheduleDownload(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:ScheduleDownload', E, S),
    lists:foldl(fun(Elem, ScheduleDownload) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'CommandKey' ->
                                ScheduleDownload#schedule_download{command_key = parse_CommandKeyType(Elem)};

                            'FileType' ->
                                ScheduleDownload#schedule_download{
				  file_type = parse_FileType('DownloadFileType', Elem)};

                            'URL' ->
                                ScheduleDownload#schedule_download{url = parse_URL(Elem)};

                            'Username' ->
                                ScheduleDownload#schedule_download{username = parse_Username(Elem)};

                            'Password' ->
                                ScheduleDownload#schedule_download{password = parse_Password(Elem)};

                            'FileSize' ->
                                ScheduleDownload#schedule_download{file_size = parse_FileSize(Elem)};

                            'TargetFileName' ->
                                ScheduleDownload#schedule_download{target_file_name = parse_TargetFileName(Elem)};

                            'TimeWindowList' ->
                                ScheduleDownload#schedule_download{time_window_list = parse_TimeWindowList(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #schedule_download{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_ScheduleDownloadResponse(#xmlElement{},#parser{}) -> #schedule_download_response{}.
parse_ScheduleDownloadResponse(_, _) -> #schedule_download_response{}.

-spec parse_CancelTransfer(#xmlElement{},#parser{}) -> #cancel_transfer{}.
parse_CancelTransfer(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:CancelTransfer', E, S),
    lists:foldl(fun(Elem, CancelTransfer) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'CommandKey' ->
                                CancelTransfer#cancel_transfer{command_key = parse_CommandKeyType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #cancel_transfer{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_CancelTransferResponse(#xmlElement{},#parser{}) -> #cancel_transfer_response{}.
parse_CancelTransferResponse(_, _) -> #cancel_transfer_response{}.

-spec parse_ChangeDUState(#xmlElement{},#parser{}) -> #change_du_state{}.
parse_ChangeDUState(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:ChangeDUState', E, S),
    lists:foldl(fun(Elem, ChangeDUState) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Operations' ->
                                ChangeDUState#change_du_state{operations = parse_OperationStruct(Elem, State)};

                            'CommandKey' ->
                                ChangeDUState#change_du_state{command_key = parse_CommandKeyType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #change_du_state{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_ChangeDUStateResponse(#xmlElement{},#parser{}) -> #change_du_state_response{}.
parse_ChangeDUStateResponse(_, _) -> #change_du_state_response{}.

-spec parse_Inform(#xmlElement{},#parser{}) -> #inform{}.
parse_Inform(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:Inform', E, S),
    lists:foldl(fun(Elem, Inform) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'DeviceId' ->
                                Inform#inform{device_id = parse_DeviceIdStruct(Elem, State)};
                            'Event' ->
                                Inform#inform{event = parse_EventList(Elem, State)};
                            'MaxEnvelopes' ->
                                Inform#inform{max_envelopes = parse_MaxEnvelopes(Elem)};
                            'CurrentTime' ->
                                Inform#inform{current_time = parse_CurrentTime(Elem)};
                            'RetryCount' ->
                                Inform#inform{retry_count = parse_RetryCount(Elem)};
                            'ParameterList' ->
                                Inform#inform{parameter_list = parse_ParameterValueList(Elem, State)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #inform{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_InformResponse(#xmlElement{},#parser{}) -> #inform_response{}.
parse_InformResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:InformResponse', E, S),
    lists:foldl(fun(Elem, InformResponse) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'MaxEnvelopes' ->
                                InformResponse#inform_response{max_envelopes = parse_MaxEnvelopes(Elem)};
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #inform_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_TransferComplete(#xmlElement{},#parser{}) -> #transfer_complete{}.
parse_TransferComplete(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:TransferComplete', E, S),
    lists:foldl(fun(Elem, TransferComplete) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'CommandKey' ->
                                TransferComplete#transfer_complete{command_key = parse_CommandKeyType(Elem)};

                            'FaultStruct' ->
                                TransferComplete#transfer_complete{fault_struct = parse_TransferCompleteFaultStruct(Elem, State)};

                            'StartTime' ->
                                TransferComplete#transfer_complete{start_time = parse_StartTime(Elem)};

                            'CompleteTime' ->
                                TransferComplete#transfer_complete{complete_time = parse_CompleteTime(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #transfer_complete{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_TransferCompleteResponse(#xmlElement{},#parser{}) -> #transfer_complete_response{}.
parse_TransferCompleteResponse(_, _) -> #transfer_complete_response{}.

-spec parse_AutonomousTransferComplete(#xmlElement{},#parser{}) -> #autonomous_transfer_complete{}.
parse_AutonomousTransferComplete(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:AutonomousTransferComplete', E, S),
    lists:foldl(fun(Elem, AutonomousTransferComplete) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'AnnounceURL' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{announce_url = parse_AnnounceURL(Elem)};

                            'TransferURL' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{transfer_url = parse_TransferURL(Elem)};

                            'IsDownload' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{is_download = parse_IsDownload(Elem)};

                            'FileType' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{file_type = parse_FileType('TransferFileType', Elem)};

                            'FileSize' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{file_size = parse_FileSize(Elem)};

                            'TargetFileName' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{target_file_name = parse_TargetFileName(Elem)};

                            'FaultStruct' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{fault_struct = parse_TransferCompleteFaultStruct(Elem, State)};

                            'StartTime' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{start_time = parse_StartTime(Elem)};

                            'CompleteTime' ->
                                AutonomousTransferComplete#autonomous_transfer_complete{complete_time = parse_CompleteTime(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #autonomous_transfer_complete{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_AutonomousTransferCompleteResponse(#xmlElement{},#parser{}) -> #autonomous_transfer_complete_response{}.
parse_AutonomousTransferCompleteResponse(_, _) -> #autonomous_transfer_complete_response{}.

-spec parse_Kicked(#xmlElement{},#parser{}) -> #kicked{}.
parse_Kicked(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:Kicked', E, S),
    lists:foldl(fun(Elem, Kicked) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Command' ->
                                Kicked#kicked{command = parse_Command(Elem)};

                            'Referer' ->
                                Kicked#kicked{referer = parse_Referer(Elem)};

                            'Arg' ->
                                Kicked#kicked{arg = parse_Arg(Elem)};

                            'Next' ->
                                Kicked#kicked{next = parse_Next(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #kicked{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_KickedResponse(#xmlElement{},#parser{}) -> #kicked_response{}.
parse_KickedResponse(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:KickedResponse', E, S),
    lists:foldl(fun(Elem, KickedResponse) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'NextURL' ->
                                KickedResponse#kicked_response{next_url = parse_NextURL(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #kicked_response{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_RequestDownload(#xmlElement{},#parser{}) -> #request_download{}.
parse_RequestDownload(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:RequestDownload', E, S),
    lists:foldl(fun(Elem, RequestDownload) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'FileType' ->
                                RequestDownload#request_download{file_type = parse_FileType('DownloadFileType', Elem)};

                            'FileTypeArg' ->
                                RequestDownload#request_download{file_type_arg = parse_FileTypeArg(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #request_download{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_RequestDownloadResponse(#xmlElement{},#parser{}) -> #request_download_response{}.
parse_RequestDownloadResponse(_, _) -> #request_download_response{}.

-spec parse_DUStateChangeComplete(#xmlElement{},#parser{}) -> #du_state_change_complete{}.
parse_DUStateChangeComplete(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:DUStateChangeComplete', E, S),
    lists:foldl(fun(Elem, DUStateChangeComplete) ->
                        case get_local_name(Elem#xmlElement.name) of
                            'Results' ->
                                DUStateChangeComplete#du_state_change_complete{results = parse_OpResultStruct(Elem, State)};

                            'CommandKey' ->
                                DUStateChangeComplete#du_state_change_complete{command_key = parse_CommandKeyType(Elem)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #du_state_change_complete{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_DUStateChangeCompleteResponse(#xmlElement{},#parser{}) -> #du_state_change_complete_response{}.
parse_DUStateChangeCompleteResponse(_, _) -> #du_state_change_complete_response{}.

-spec parse_AutonomousDUStateChangeComplete(#xmlElement{},#parser{}) -> #autonomous_du_state_change_complete{}.
parse_AutonomousDUStateChangeComplete(#xmlElement{content = Content} = E, S) ->
    State = check_namespace('cwmp:AutonomousDUStateChangeComplete', E, S),
    lists:foldl(fun(Elem, AutonomousDUStateChangeComplete) ->
                        case get_local_name(Elem#xmlElement.name) of

                            'Results' ->
				AutonomousDUStateChangeComplete#autonomous_du_state_change_complete{results = parse_AutonOpResultStruct(Elem, State)};

                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #autonomous_du_state_change_complete{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).

-spec parse_AutonomousDUStateChangeCompleteResponse(#xmlElement{},#parser{}) -> #autonomous_du_state_change_complete_response{}.
parse_AutonomousDUStateChangeCompleteResponse(_, _) -> #autonomous_du_state_change_complete_response{}.

%% end

%%%-----------------------------------------------------------------------------
%%% Unitary tetsts
%%%-----------------------------------------------------------------------------

main(File) ->
    {Doc, _Rest} = xmerl_scan:file(File),
    Rpc = parse(Doc, #parser{}),
    io:format(">> ~p~n", [Rpc]),
    ok.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

parse_root_test() ->
    T = ["../test/data/GetParameterValues.xml",
	 "../test/data/FaultResponse.xml",
	 "../test/data/Fault.xml",
	 "../test/data/GetParameterAttributes.xml",
	 "../test/data/GetParameterNamesResponse.xml",
	 "../test/data/GetParameterNames.xml",
	 "../test/data/GetParameterValues.xml",
	 "../test/data/GetRPCMethodsResponse.xml",
	 "../test/data/GetRPCMethods.xml",
	 "../test/data/InformResponse.xml",
	 "../test/data/Inform.xml",
	 "../test/data/Reboot.xml",
	 "../test/data/SetParameterValues.xml",
	 "../test/data/Simple.xml"
	],
    A = array:from_list(T),
    F = array:get(7,  A),
    ?DBG(F),
    {Doc, _Rest} = xmerl_scan:file(F),
    Rpc = parse(Doc, #parser{}),
    ?DBG(Rpc),
    ok.

-endif.
