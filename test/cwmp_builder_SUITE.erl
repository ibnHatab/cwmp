%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%% CWMP Builder regression tests
%%% @end
%%% Created : 12 Nov 2012 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(cwmp_builder_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("cwmp.hrl").

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
    [my_test_case
     , build_AddObject_tc
     , build_AddObjectResponse_tc
     , build_AutonomousDUStateChangeComplete_tc
     , build_AutonomousDUStateChangeCompleteResponse_tc
     , build_AutonomousTransferComplete_tc
     , build_AutonomousTransferCompleteResponse_tc
     , build_CancelTransfer_tc
     , build_CancelTransferResponse_tc
     , build_ChangeDUState_tc
     , build_ChangeDUStateResponse_tc     
     , build_DeleteObject_tc             
     , build_DeleteObjectResponse_tc     
     , build_Download_tc
     , build_DownloadResponse_tc
     , build_DUStateChangeComplete_tc
     , build_DUStateChangeCompleteResponse_tc
     , build_FactoryReset_tc
     , build_FactoryResetResponse_tc
     , build_Fault_tc
     , build_GetAllQueuedTransfers_tc
     , build_GetAllQueuedTransfersResponse_tc
     , build_GetOptions_tc
     , build_GetOptionsResponse_tc
     , build_GetParameterAttributes_tc
     %%, build_GetParameterAttributesResponse_tc
     , build_GetParameterNames_tc
     %% , build_GetParameterNamesResponse_tc
     , build_GetParameterValues_tc
     , build_GetParameterValuesResponse_tc
     , build_GetQueuedTransfers_tc
     , build_GetQueuedTransfersResponse_tc
     , build_GetRPCMethods_tc
     , build_GetRPCMethodsResponse_tc		
     , build_Inform_tc
     , build_InformResponse_tc
     , build_Kicked_tc
     , build_KickedResponse_tc
     %%  , build_Reboot_tc
     %%  , build_RebootResponse_tc
     %%  , build_RequestDownload_tc
     %%  , build_RequestDownloadResponse_tc
     %%  , build_ScheduleDownload_tc
     %%  , build_ScheduleDownloadResponse_tc
     %%  , build_ScheduleInform_tc
     %%  , build_ScheduleInformResponse_tc
     %%  , build_SetParameterAttributes_tc
     %%  , build_SetParameterAttributesResponse_tc
     %%  , build_SetParameterValues_tc
     %%  , build_SetParameterValuesResponse_tc
     %% , build_SetVouchers_tc
     %% , build_SetVouchersResponse_tc
     %% , build_TransferComplete_tc
     %% , build_TransferCompleteResponse_tc
     %% , build_Upload_tc
     %% , build_UploadResponse_tc
    ].

%%--------------------------------------------------------------------
%% Utilities
%%--------------------------------------------------------------------
cwmp_obj_BuildCheck(Config, Data, Method) ->
    ct:print("cwmp_obj_BuildCheck ~p~n", [Method]),
    RpcDoc = cwmp_builder:build(Data),
    DataDir = ?config(data_dir, Config),
    SchemaFile = filename:join([DataDir, "../../doc/cwmp-1-2.xsd"]),
    RpcFile = filename:join([DataDir, "cwmp_" ++ Method ++ ".xml"]),
    ct:print(">> ~p~n", [RpcFile]),
    {ok, saved} = savexml(RpcDoc, RpcFile),
    validate_cwmp(RpcFile, SchemaFile).

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

validate_cwmp(RpcFile, SchemaFile) ->
    Command = "xmllint --noout --path doc --schema "
	++ SchemaFile ++ " " ++ RpcFile,
    Result = os:cmd(Command),
    ct:print("Validate: <~p ~n", [Result]),
    ok.

cwmp_print(_Config, Data, Method) ->
    ct:print("cwmp_obj_BuildCheck ~p~n", [Method]),
    RpcDoc = cwmp_builder:build(Data),
    ct:print(">>> ~p ~n",[RpcDoc]).

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case() ->
    "my_test_case".

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
my_test_case(_Config) ->
    ok.

build_Inform_tc() -> "build_Inform".
build_Inform_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{inform,
	       {device_id_struct,"Alcatel","001D4C","9365 BSR Femto",	"4321"},
	       [{event_struct,2,[]}],                       1,
                       {{2012,10,3},{18,16,2}},                       0,
                       [{parameter_value_struct,"Device.DeviceSummary",
			 "DeviceSummary"},
                        {parameter_value_struct,
                            "Device.DeviceInfo.HardwareVersion","p1"},
                        {parameter_value_struct,
                            "Device.DeviceInfo.SoftwareVersion",
                            "BCR-04-00-BSR-XMIM-06.01"},
                        {parameter_value_struct,
                            "Device.ManagementServer.ConnectionRequestURL",
                            "&sn=4321http://127.0.0.1:8095/ConnectionRequest?command=cr"},
                        {parameter_value_struct,
                            "Device.ManagementServer.ParameterKey",
                            "ParameterKey"},
                        {parameter_value_struct,"Device.LAN.IPAddress",
                            "172.0.0.1"},
                        {parameter_value_struct,"Device.LAN.MACAddress",
                            "00-0E-35-D6-24-F7"},
                        {parameter_value_struct,
                            "Device.Services.BSR.1.SDM.1.mimVersion",
                            "BCR-04-00-BSR-XMIM-02.00"},
                        {parameter_value_struct,
                            "Device.DeviceInfo.AdditionalHardwareVersion",
                            "250mW"},
                        {parameter_value_struct,
                            "Device.Services.BSR.1.RFTrace.1.iMSI",
                            "iMSI"}
		       ]}]}},

    ok = cwmp_obj_BuildCheck(Config, Data, "Inform").


build_AddObject_tc() -> "build_AddObject".
build_AddObject_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{add_object,
	      "name","parameter"}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "AddObject").


build_AddObjectResponse_tc() -> "build_AddObjectResponse".
build_AddObjectResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{add_object_response,
	      1,0}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "AddObjectResponse").
    

build_AutonomousDUStateChangeComplete_tc() -> "build_AutonomousDUStateChangeComplete".
build_AutonomousDUStateChangeComplete_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{autonomous_du_state_change_complete,
	       {auton_op_result_struct,
		 {op_result_struct,"uuid","depl_unit","1","on",true,"ExecutionUnitRefList",
		 {{2012,10,3},{18,53,34}},
                 {{2012,10,3},{18,53,44}},
                 {deployment_unit_fault_struct,1,"fault"}
		 },""}}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "AutonomousDUStateChangeComplete").




build_AutonomousDUStateChangeCompleteResponse_tc() -> "build_AutonomousDUStateChangeCompleteResponse".
build_AutonomousDUStateChangeCompleteResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{autonomous_du_state_change_complete_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "AutonomousDUStateChangeCompleteResponse").
    

build_AutonomousTransferComplete_tc() -> "build_AutonomousTransferComplete".
build_AutonomousTransferComplete_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{autonomous_transfer_complete,
	       {http,"announceURL-FwUpgr",80,"/",[]},
	       {http,"transferURL-FwUpgr",80,"/",[]},
	       true,1,12345,
	       "",
	       {transfer_complete_fault_struct,0,[]},
	       {{2012,10,3},{18,53,34}},
	       {{2012,10,3},{18,53,44}}}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "AutonomousTransferComplete").
    

build_AutonomousTransferCompleteResponse_tc() -> "build_AutonomousTransferCompleteResponse".
build_AutonomousTransferCompleteResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{autonomous_transfer_complete_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "AutonomousTransferCompleteResponse").
    

build_CancelTransfer_tc() -> "build_CancelTransfer".
build_CancelTransfer_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{cancel_transfer,
	      "command_key"}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "CancelTransfer").
    

build_CancelTransferResponse_tc() -> "build_CancelTransferResponse".
build_CancelTransferResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{cancel_transfer_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "CancelTransferResponse").
    

build_ChangeDUState_tc() -> "build_ChangeDUState".
build_ChangeDUState_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{change_du_state,
                [{install_op_struct,
                    {http,"announceURL-FwUpgr",80,"/",[]},"","","",""}
	        ],"command_key_type"
             }]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "ChangeDUState").
    

build_ChangeDUStateResponse_tc() -> "build_ChangeDUStateResponse".
build_ChangeDUStateResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{change_du_state_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "ChangeDUStateResponse").
    

build_DeleteObject_tc() -> "build_DeleteObject".
build_DeleteObject_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{delete_object,"name","parameter"}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "DeleteObject").
    

build_DeleteObjectResponse_tc() -> "build_DeleteObjectResponse".
build_DeleteObjectResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{delete_object_response,1}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "DeleteObjectResponse").
    


build_Download_tc() -> "build_Download".
build_Download_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{download,
	       "",
	       1,
	       {http,"announceURL-FwUpgr",80,"/",[]},
	       [],
	       [],
	       1024,
	       "",
	       300,
	       {http,"successURL-FwUpgr",80,"/",[]},
	       {http,"failureURL-FwUpgr",80,"/",[]}
	      }]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "Download").
    

build_DownloadResponse_tc() -> "build_DownloadResponse".
build_DownloadResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{download_response,0,
	       {{2012,10,3},{18,53,34}},
               {{2012,10,3},{18,53,44}}}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "DownloadResponse").


build_DUStateChangeComplete_tc() -> "build_DUStateChangeComplete".
build_DUStateChangeComplete_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{du_state_change_complete,
	       {op_result_struct,
		 "uuid",
		 "depl_unit",
		 "1",
		 "on",
		 true,
		 "ExecutionUnitRefList",
		 {{2012,10,3},{18,53,34}},
                 {{2012,10,3},{18,53,44}},
                 {deployment_unit_fault_struct,
		  9012,
		  "fault"}},
	       ""
	       }]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "DUStateChangeComplete").
    

build_DUStateChangeCompleteResponse_tc() -> "build_DUStateChangeCompleteResponse".
build_DUStateChangeCompleteResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{du_state_change_complete_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "DUStateChangeCompleteResponse").
    

build_FactoryReset_tc() -> "build_FactoryReset".
build_FactoryReset_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{factory_reset}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "FactoryReset").
    

build_FactoryResetResponse_tc() -> "build_FactoryResetResponse".
build_FactoryResetResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{factory_reset_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "FactoryResetResponse").
    

build_Fault_tc() -> "build_Fault".
build_Fault_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{fault,9000,"",
	      {set_parameter_values_fault,"name",9000,""}}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "Fault").
    

build_GetAllQueuedTransfers_tc() -> "build_GetAllQueuedTransfers".
build_GetAllQueuedTransfers_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_all_queued_transfers}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetAllQueuedTransfers").
    

build_GetAllQueuedTransfersResponse_tc() -> "build_GetAllQueuedTransfersResponse".
build_GetAllQueuedTransfersResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_all_queued_transfers_response,
             [{queued_transfer_struct,"",3}]}]}},
     ok = cwmp_obj_BuildCheck(Config, Data, "GetAllQueuedTransfersResponse").
    

build_GetOptions_tc() -> "build_GetOptions".
build_GetOptions_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_options,"op_name"}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetOptions").
    

build_GetOptionsResponse_tc() -> "build_GetOptionsResponse".
build_GetOptionsResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_options_response,[{option_struct,"",1,2,1,
		 {{2012,10,3},{18,53,34}},
                 {{2012,10,3},{18,53,44}},
		  0}]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetOptionsResponse").
    

build_GetParameterAttributes_tc() -> "build_GetParameterAttributes".
build_GetParameterAttributes_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_parameter_attributes,
	      ["",""]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetParameterAttributes").
    

build_GetParameterAttributesResponse_tc() -> "build_GetParameterAttributesResponse".
build_GetParameterAttributesResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_parameter_attributes_response,
	      [{parameter_attribute_struct,"",2,[""]}]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetParameterAttributesResponse").
    
    

build_GetParameterNames_tc() -> "build_GetParameterNames".
build_GetParameterNames_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_parameter_names,"param_path",true}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetParameterNames").
    

build_GetParameterNamesResponse_tc() -> "build_GetParameterNamesResponse".
build_GetParameterNamesResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_parameter_names_response,
	      [{parameter_info_struct,"param_path",true},
	       {parameter_info_struct,"param_path1",true}
	      ]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetParameterNamesResponse").
    

build_GetParameterValues_tc() -> "build_GetParameterValues".
build_GetParameterValues_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_parameter_values,["",""]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetParameterValues").
    

build_GetParameterValuesResponse_tc() -> "build_GetParameterValuesResponse".
build_GetParameterValuesResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_parameter_values_response,
	      [{parameter_value_struct,
                            "Device.DeviceInfo.AdditionalHardwareVersion",
                            "250mW"},
               {parameter_value_struct,
                            "Device.Services.BSR.1.RFTrace.1.iMSI",
                            "iMSI"}
	       ]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetParameterValuesResponse").
    

build_GetQueuedTransfers_tc() -> "build_GetQueuedTransfers".
build_GetQueuedTransfers_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_queued_transfers}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetQueuedTransfers").
    

build_GetQueuedTransfersResponse_tc() -> "build_GetQueuedTransfersResponse".
build_GetQueuedTransfersResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_queued_transfers_response,
	      [{queued_transfer_struct,"command_key",1}]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetQueuedTransfersResponse").
    

build_GetRPCMethods_tc() -> "build_GetRPCMethods".
build_GetRPCMethods_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	    	     [{get_rpc_methods}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetRPCMethods").
    

build_GetRPCMethodsResponse_tc() -> "build_GetRPCMethodsResponse".
build_GetRPCMethodsResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{get_rpc_methods_response,
                      ["GetRPCMethods",
			"Inform",
			"TransferComplete",
                        "AutonomousTransferComplete",
			"DUStateChangeComplete",
                        "AutonomousDUStateChangeComplete"]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "GetRPCMethodsResponse").
    

  

build_InformResponse_tc() -> "build_InformResponse".
build_InformResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{inform_response,1}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "InformResponse").
    

build_Kicked_tc() -> "build_Kicked".
build_Kicked_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{kicked,"command","reffer","arg","next"}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "Kicked").
    

 build_KickedResponse_tc() -> "build_KickedResponse".
 build_KickedResponse_tc(Config) ->
     Data = {cwmp_obj,
 	    {envelope,
 	     {header,{id,true,"1"},undefined,undefined},
 	     [{kicked_response,
              {"",""}}]}},
     ok = cwmp_obj_BuildCheck(Config, Data, "KickedResponse").


build_Reboot_tc() -> "build_Reboot".
build_Reboot_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{reboot,"command_key"}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "Reboot").
    

build_RebootResponse_tc() -> "build_RebootResponse".
build_RebootResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{reboot_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "RebootResponse").
    

build_RequestDownload_tc() -> "build_RequestDownload".
build_RequestDownload_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{request_download,1,
	      [{arg_struct,"Device.DeviceInfo.AdditionalHardwareVersion",
                            "250mW"}]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "RequestDownload").
    

build_RequestDownloadResponse_tc() -> "build_RequestDownloadResponse".
build_RequestDownloadResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{request_download_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "RequestDownloadResponse").
    

build_ScheduleDownload_tc() -> "build_ScheduleDownload".
build_ScheduleDownload_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{schedule_download,"",
	      {http,"announceURL-FwUpgr",80,"/",[]},
	      "","",2010,"",
	      [{time_window_struct,2,3,2,"",2}]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "ScheduleDownload").
    

build_ScheduleDownloadResponse_tc() -> "build_ScheduleDownloadResponse".
build_ScheduleDownloadResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{schedule_download_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "ScheduleDownloadResponse").
    

build_ScheduleInform_tc() -> "build_ScheduleInform".
build_ScheduleInform_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{schedule_inform,1,""}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "ScheduleInform").
    

build_ScheduleInformResponse_tc() -> "build_ScheduleInformResponse".
build_ScheduleInformResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{schedule_inform_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "ScheduleInformResponse").
    

build_SetParameterAttributes_tc() -> "build_SetParameterAttributes".
build_SetParameterAttributes_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{set_parameter_attributes,
	      [{"",true,1,false,["1","2"]}]
	      }]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "SetParameterAttributes").
    

build_SetParameterAttributesResponse_tc() -> "build_SetParameterAttributesResponse".
build_SetParameterAttributesResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{set_parameter_attributes_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "SetParameterAttributesResponse").
    

build_SetParameterValues_tc() -> "build_SetParameterValues".
build_SetParameterValues_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{set_parameter_values,
              [{parameter_value_struct,
                            "Device.DeviceInfo.AdditionalHardwareVersion",
                            "250mW"},
               {parameter_value_struct,
                            "Device.Services.BSR.1.RFTrace.1.iMSI",
                            "iMSI"}
	       ],""}]}},
     ok = cwmp_obj_BuildCheck(Config, Data, "SetParameterValues").
    

build_SetParameterValuesResponse_tc() -> "build_SetParameterValuesResponse".
build_SetParameterValuesResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{set_parameter_values_response,0}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "SetParameterValuesResponse").
    

build_SetVouchers_tc() -> "build_SetVouchers".
build_SetVouchers_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{set_vouchers,
	      [0,1,0,1]}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "SetVouchers").
    

build_SetVouchersResponse_tc() -> "build_SetVouchersResponse".
build_SetVouchersResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{set_vouchers_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "SetVouchersResponse").
    

build_TransferComplete_tc() -> "build_TransferComplete".
build_TransferComplete_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{transfer_complete,"",
	      {transfer_complete_fault_struct,1,"1 BOOT"},
	      {{2012,10,3},{18,53,34}},
                 {{2012,10,3},{18,53,44}}}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "TransferComplete").
    

build_TransferCompleteResponse_tc() -> "build_TransferCompleteResponse".
build_TransferCompleteResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{transfer_complete_response}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "TransferCompleteResponse").
    

build_Upload_tc() -> "build_Upload".
build_Upload_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{upload,"","Device.Services.BSR.1.RFTrace.1.iMSI",
	      {http,"announceURL-FwUpgr",80,"/",[]},
	      "","",300}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "Upload").
    

build_UploadResponse_tc() -> "build_UploadResponse".
build_UploadResponse_tc(Config) ->
    Data = {cwmp_obj,
	    {envelope,
	     {header,{id,true,"1"},undefined,undefined},
	     [{upload_response,1,
	      {{2012,10,3},{18,53,34}},
                 {{2012,10,3},{18,53,44}}}]}},
    ok = cwmp_obj_BuildCheck(Config, Data, "UploadResponse").
    

