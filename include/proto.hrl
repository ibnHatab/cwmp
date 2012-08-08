%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%%-----------------------------------------------------------------------------

-define(SUPPORTED_CPE_METHODS, ['cwmp:GetRPCMethods'
				, 'cwmp:SetParameterValues'
				, 'cwmp:GetParameterValues'
				, 'cwmp:GetParameterNames'
				, 'cwmp:SetParameterAttributes'
				, 'cwmp:GetParameterAttributes'
				, 'cwmp:AddObject'
				, 'cwmp:DeleteObject'
				, 'cwmp:Reboot'
				, 'cwmp:Download'
				, 'cwmp:ScheduleDownload'
				, 'cwmp:Upload'
				, 'cwmp:FactoryReset'
				, 'cwmp:GetQueuedTransfers'
				, 'cwmp:GetAllQueuedTransfers'
				, 'CancelTransfer'
				, 'cwmp:ScheduleInform'
				, 'ChangeDUState'
				, 'cwmp:SetVouchers'
				, 'cwmp:GetOptions'
			       ]).

-define(SUPPORTED_CPE_DEPRECATED_METHODS, ['cwmp:GetQueuedTransfers'
					   , 'cwmp:SetVouchers'
					   , 'cwmp:GetOptions'
					  ]).


-define(SUPPORTED_ACS_METHODS, ['cwmp:GetRPCMethods'
				, 'cwmp:Inform'
				, 'cwmp:TransferComplete'
				, 'cwmp:AutonomousTransferComplete'
				, 'cwmp:DUStateChangeComplete'
				, 'AutonomousDUStateChangeComplete'
				, 'cwmp:RequestDownload'
				, 'cwmp:Kicked'
			       ]).

-define(SUPPORTED_ACS_DEPRECATED_METHODS, ['cwmp:Kicked']).


-type rpc_data_type() :: string | int | unsignedInt | boolean | dateTime | base64 | anySimpleType.

-type dateTime() :: string().

-type id() :: term (). %% ?/
-type hold_requests() :: boolean().


-define(SUPPORTED_CPE_FAULT_CODES,
	[
	 %%        CPE Fault Codes from 9000 to 9799
	   {9000, "Method not supported"}
	 , {9001, "Request denied (no reason specified)"}
	 , {9002, "Internal error"}
	 , {9003, "Invalid arguments"}
	 , {9004, "Resources exceeded"}
	 , {9005, "Invalid parameter name"}
	 , {9006, "Invalid parameter type"}
	 , {9007, "Invalid parameter value"}
	 , {9008, "Attempt to set a non-writable parameter"}
	 , {9009, "Notification request rejected"}
	 , {9010, "File transfer failure"}
	 , {9011, "Upload failure"}
	 , {9012, "File transfer server authentication failure"}
	 , {9013, "Unsupported protocol for file transfer"}
	 , {9014, "File transfer failure: unable to join multicast group"}
	 , {9015, "File transfer failure: unable to contact file server"}
	 , {9016, "File transfer failure: unable to access file"}
	 , {9017, "File transfer failure: unable to complete download"}
	 , {9018, "File transfer failure: file corrupted"}
	 , {9019, "File transfer failure: file authentication failure"}
	 , {9020, "File transfer failure: unable to complete download within specified time windows"}
	 , {9021, "Cancelation of file transfer not permitted in current transfer state"}
	 , {9022, "Invalid UUID Format"}
	 , {9023, "Unknown Execution Environment"}
	 , {9024, "Disabled Execution Environment"}
	 , {9025, "Deployment Unit to Execution Environment Mismatch"}
	 , {9026, "Duplicate Deployment Unit"}
	 , {9027, "System Resources Exceeded"}
	 , {9028, "Unknown Deployment Unit"}
	 , {9029, "Invalid Deployment Unit State"}
	 , {9030, "Invalid Deployement Unit Update – Downgrade not permitted"}
	 , {9031, "Invalid Deployement Unit Update – Version not specified"}
	 , {9032, "Invalid Deployment Unit Update – Version already exists"}
	]).
-type cpe_fault_code_type() :: non_neg_integer().

%% @doc CPEExtensionFaultCodeType
%% Range of CPE Fault Codes from 9033 to 9799 for future extension
-type cpe_extension_fault_code_type() :: cpe_fault_code_type().

%% @doc CPEVendorFaultCodeType
%% Vendor Extension range for CPE Fault Codes from 9800 to 9899
-type cpe_vendor_fault_code_type() :: cpe_fault_code_type().


%% @doc AcsFaultCodeType
%%  ACS Fault Codes from 8000 to 8005
-type acs_fault_code_type() :: non_neg_integer().
-define(SUPPORTED_ACS_FAULT_CODES,
	[   {8000, "Method not supported"}
	  , {8001, "Request denied (no reason specified)"}
	  , {8002, "Internal error"}
	  , {8003, "Invalid arguments"}
	  , {8004, "Resources exceeded"}
	  , {8005, "Retry request"}
	]).

%% @doc Vendor Extension range for ACS Fault Codes from 8800 to 8899
-type acs_vendor_fault_code_type() :: acs_fault_code_type().

%% @doc
%%        This type is used for AllQueuedTransferStruct and AutonomousTransferComplete
%%
-define(SUPPORTED_TRANSFER_FILE_TYPE,
	%% Pattern allows the following File Types:
	[ {1, "Firmware Upgrade, Image"}
	  , {2, "Web, Content"}
	  , {3, "Vendor Configuration, File"}
	  , {4, "Vendor Log, File"}
	  , {4, "Tone, File"}
	  , {5, "Ringer, File"}
	  , {6, "Vendor Configuration File, [1-9]\d*"}
	  , {7, "Vendor Log File, [1-9]\d*"}
	  , {'X', "OUI Vendor speceific, ID"}
	]).
-type transfer_file_type() :: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 'X'.


%% @doc This type is used for Download and RequestDownload
-define(SUPPORTED_DOWNLOAD_FILE_TYPE,
	%% This pattern allows the following File Types:
	[ {1, "Firmware Upgrade, Image"}
	  , {2, "Web, Content"}
	  , {3, "Vendor Configuration, File"}
	  , {4, "Tone, File"}
	  , {5, "Ringer, File"}
	  , {'X', "OUI Vendor speceific, ID"}
	]).
-type download_file_type() :: 1 | 2 | 3 | 4 | 5 | 'X'.

%% @doc This type is used for Upload
-define(SUPPORTED_UPLOAD_FILE_TYPE,
	%% This pattern allows the following File Types:
	[ {1, "Vendor Configuration, File"}
	  , {2, "Vendor Log, File"}
	  , {3, "Vendor Configuration File, [1-9]\d*"}
	  , {4, "Vendor Log File, [1-9]\d*"}
	  , {'X', "OUI Vendor speceific, ID"}
	]).
-type upload_file_type() :: 1 | 2 | 3 | 4 | 'X'.

-define(SUPPORTED_EVENT_CODE_TYPE,
        %%    This pattern allows the following Event Codes:
	[ {0, "BOOTSTRAP"}
	  , {1, "BOOT"}
	  , {2, "PERIODIC"}
	  , {3, "SCHEDULED"}
	  , {4, "VALUE, CHANGE"}
	  , {5, "KICKED"}
	  , {6, "CONNECTION, REQUEST"}
	  , {7, "TRANSFER, COMPLETE"}
	  , {8, "DIAGNOSTICS, COMPLETE"}
	  , {9, "REQUEST, DOWNLOAD"}
	  , {10, "AUTONOMOUS TRANSFER, COMPLETE"}
	  , {11, "DU STATE CHANGE, COMPLETE"}
	  , {12, "AUTONOMOUS DU STATE CHANGE, COMPLETE"}
	  , {Reboot,		"M Reboot"}
	  , {ScheduleInform,	"M ScheduleInform"}
	  , {Download,		"M Download"}
	  , {ScheduleDownload,	"M ScheduleDownload"}
	  , {Upload,		"M Upload"}
	  , {ChangeDUState,	"M ChangeDUState"}
	]).
-type event_code_type() :: 1-12 | atom().

%% @doc
%%    Extendable Time Window Mode Definition
-define(SUPPORTED_TIME_WINDOW_MODE_VALUE_TYPE,
	%%     This pattern allows the following Time Window Modes:
	[ {1,		"At Any, Time"}
	  , {2,		"Immediately"}
	  , {3,		"When, Idle"}
	  , {4,		"Confirmation, Needed"}
	  , {'X',	"OUI Vendor speceific, ID"}
	]).
-type time_window_mode_value_type() :: any().

%% @doc
%%        Restricted subset of CPEFaultCodeType that are specific for the
%%        TransferComplete and AutonomousTransferComplete RPCs
%%
-type transfer_complete_cpe_fault_code_type() :: cpe_fault_code_type().

%% @doc Fault information for TransferComplete and AutonomousTransferComplete
-record (transfer_complete_fault_struct, {
	   fault_code :: transfer_complete_cpe_fault_code_type(),
	   fault_string :: string()
	  }).

%% @doc
%%        Restricted subset of CPEFaultCodeType that are specific for a single operation
%%        in the DUStateChangeComplete and AutonomousDUStateChangeComplete RPCs
%%
-type deployment_unit_cpe_fault_code_type() :: cpe_fault_code_type().

%% @doc Structure used to convey success or failure status of an
%%        operation performed on a Deployment Unit
%%
-record (deployment_unit_fault_struct, {
	   fault_code :: transfer_complete_cpe_fault_code_type(),
	   fault_string :: string()
	  }).

%%   Generic Type Definitions
-type command_key_type() :: string().
-type object_name_type() :: string().
-type parameter_key_type() :: string().

-record(parameter_names, {
	  string :: [string()]
	 }).

-record(parameter_value_struct,	{
	  name  :: string(),
	  value :: any()
	 }).

-record(parameter_value_list,{
	  parameter_value_struct :: [#parameter_value_struct{}]
	 }).

%%    GetRPCMethods Type Definition

-record(method_list,{
	 string :: [string()]
	 }).

-record(device_id_struct,{
	  manufacturer :: string(),
	  oui :: string(),
	  product_class :: string(),
	  serial_number :: string()
	 }).


-record(event_struct,{
	  event_code :: event_code_type(),
	  command_key :: command_key_type()
	 }).

-record(event_list,{
	  event_struct :: [#event_struct{}]
	 }).

%%    Get Parameter Names Type Definitions
-record(parameter_info_struct, {
	  name :: string(),
	  writable :: boolean()
	 }).

-record(parameter_info_list, {
	 parameter_info_struct :: [#parameter_info_struct{}]
	 }).

%%    Get/Set Parameter Attributes Type Definitions

-type access_list_value_type() :: string().

-record(access_list,{
	 string :: [access_list_value_type()]
	 }).


-type parameter_attribute_notification_value_type() :: 1 | 2 | 3 | 4 | 5 | 6.

-record(set_parameter_attributes_struct, {
	  name :: string(),
	  notification_change :: boolean(),
	  notification :: parameter_attribute_notification_value_type(),
	  access_list_change :: boolean(),
	  access_list :: #access_list{}
	 }).

-record(set_parameter_attributes_list, {
	  set_parameter_attributes_struct :: [#set_parameter_attributes_list{}]
	 }).


-record(parameter_attribute_struct, {
	  notification :: parameter_attribute_notification_value_type(),
	  access_list :: access_list
	  }).

-record(parameter_attribute_list, {
	  parameter_attribute_struct :: [#parameter_attribute_struct{}]
	 }).

%%    Schedule Download Time Window Type Definitions

-record(time_window_struct, {
	  window_start :: non_neg_integer(),
	  window_end   :: non_neg_integer(),
	  window_mode  :: time_window_mode_value_type(),
	  user_message :: string(),
	  max_retries  :: integer()
	 }).

-record(time_window_list, {
	  time_window_struct :: [#time_window_struct{}]
	 }).

%%    TransferComplete Type Definitions
-type transfer_state_type() :: 1 % Not yet started
			   | 2 % In progress
			   | 3 % Completed
			     .

-record(queued_transfer_struct, {
	  command_key :: command_key_type(),
	  state :: transfer_state_type()
	 }).

-record(transfer_list, {
	  queued_transfer_struct :: [#queued_transfer_struct{}]
	 }).

-record(all_queued_transfer_struct, {
	  command_key :: command_key_type(),
	  state :: transfer_state_type(),
	  is_download :: boolean(),
	  file_type :: transfer_file_type(),
	  file_size :: non_neg_integer(),
	  target_file_name :: string()
	 }).

-record(all_transfer_list, {
	  all_queued_transfer_struct :: [#all_queued_transfer_struct{}]
	 }).

%% @doc
%%        A unique identifier for a Deployment Unit
%%
-type deployment_unit_uuid() :: string().

%% @doc
%%        The state of a Deployment Unit on the device
%%
-type deployment_unit_state() :: 'Installed' | 'Uninstalled' | 'Failed'.

-type default_deployment_unit_operation_type() :: 'Install' | 'Update' | 'Uninstall'.

-type deployment_unit_operation_type() :: default_deployment_unit_operation_type()
					  | string().

%% @doc
%%        A base type for Deployment Unit operations that can be performed on a device
%%
-record(operation_struct, {
	 }).

%% @doc
%%        An operation indicating a Deployment Unit should be installed
%%
-record(install_op_struct, {
	  url :: string(),
	  uuid :: deployment_unit_uuid(),
	  username :: string(),
	  password :: string(),
	  execution_env_ref :: string()
	 }).

%% @doc
%%        An operation indicating an individual or all Deployment Units should be updated
%%
-record(update_op_struct, {
	  uuid :: deployment_unit_uuid(),
	  version :: string(),
	  url :: string(),
	  username :: string(),
	  password :: string()
	 }).

%% @doc
%%        An operation indicating a Deployment Unit should be un-installed
%%
-record(uninstall_op_struct, {
	  uuid :: deployment_unit_uuid(),
	  version :: string(),
	  execution_env_ref :: string()
	 }).

%% @doc
%%        The result of a Deployment Unit operation performed on the device
%%
-record(op_result_struct, {
	  uuid :: deployment_unit_uuid(),
	  deployment_unit_ref :: string(),
	  version :: string(),
	  current_state :: deployment_unit_state(),
	  resolved :: boolean(),
	  execution_unit_ref_list :: string(),
	  start_time :: dateTime(),
	  complete_time :: dateTime(),
	  fault :: #deployment_unit_fault_struct{}
	 }).

%% @doc
%%        The result of a Deployment Unit operation performed autonomously on the device (i.e., not at the
%%        direct request of the ACS)
%%
-record(auton_op_result_struct, {
	  op_result_struct :: #op_result_struct{},
	  operation_performed :: deployment_unit_operation_type()
	 }).

%%   Voucher and Option Type Definitions
-record(voucher_list, {
	  voucher_list :: [binary()]
	 }).

-record(option_struct, {
	  option_name :: string(),
	  voucher_sn :: non_neg_integer(),
	  state :: 0   % Option is disabled and not setup
		 | 1   % Option is enabled and not setup
		 | 2   % Option is disabled and setup
		 | 3   % Option is enabled and setup
		   ,
	  mode :: 0    % Disabled
		  | 1  % Enabled with expiration
		  | 2  % Enabled without expiration
		  ,
	  start_date :: dateTime(),
	  expiration_date :: dateTime(),
	  is_transferable :: 0 % Non-transferable
			   | 1 % Transferable
	 }).

-record(option_list, {
	  option_struct :: #option_struct{}
	 }).

-record(arg_struct, {
	  name :: string(),
	  value :: string()
	 }).


-record(file_type_arg, {
	  arg_struct :: [#arg_struct{}]
	 }).

-record(rpcData,{}).
