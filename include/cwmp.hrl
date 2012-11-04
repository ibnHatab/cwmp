%%% Use is subject to License terms.
%%%-----------------------------------------------------------------------------
%%%-----------------------------------------------------------------------------

-type cwmp_version() :: 1 | 2.

-type cwmp_obj_type() :: string | int | unsignedInt | boolean | dateTime | base64 | anySimpleType.

-type qName()     :: string().
-type url()       :: tuple().
-type anyURI()    :: tuple().
-type date_time() :: tuple(Date::calendar:date(),
			   Time::calendar:time()).

-type cwmp_method() ::
        'AddObject'                       | 'AddObjectResponse'
      | 'AutonomousDUStateChangeComplete' | 'AutonomousDUStateChangeCompleteResponse'
      | 'AutonomousTransferComplete'      | 'AutonomousTransferCompleteResponse'
      | 'CancelTransfer'                  | 'CancelTransferResponse'
      | 'ChangeDUState'                   | 'ChangeDUStateResponse'
      | 'DeleteObject'                    | 'DeleteObjectResponse'
      | 'Download'                        | 'DownloadResponse'
      | 'DUStateChangeComplete'           | 'DUStateChangeCompleteResponse'
      | 'FactoryReset'                    | 'FactoryResetResponse'
      | 'Fault'
      | 'GetAllQueuedTransfers'           | 'GetAllQueuedTransfersResponse'
      | 'GetOptions'                      | 'GetOptionsResponse'
      | 'GetParameterAttributes'          | 'GetParameterAttributesResponse'
      | 'GetParameterNames'               | 'GetParameterNamesResponse'
      | 'GetParameterValues'              | 'GetParameterValuesResponse'
      | 'GetQueuedTransfers'              | 'GetQueuedTransfersResponse'
      | 'GetRPCMethods'                   | 'GetRPCMethodsResponse'
      | 'Inform'                          | 'InformResponse'
      | 'Kicked'                          | 'KickedResponse'
      | 'Reboot'                          | 'RebootResponse'
      | 'RequestDownload'                 | 'RequestDownloadResponse'
      | 'ScheduleDownload'                | 'ScheduleDownloadResponse'
      | 'ScheduleInform'                  | 'ScheduleInformResponse'
      | 'SetParameterAttributes'          | 'SetParameterAttributesResponse'
      | 'SetParameterValues'              | 'SetParameterValuesResponse'
      | 'SetVouchers'                     | 'SetVouchersResponse'
      | 'TransferComplete'                | 'TransferCompleteResponse'
      | 'Upload'                          | 'UploadResponse'
        .

-define(SUPPORTED_CPE_METHODS, ['GetRPCMethods'
				, 'SetParameterValues'
				, 'GetParameterValues'
				, 'GetParameterNames'
				, 'SetParameterAttributes'
				, 'GetParameterAttributes'
				, 'AddObject'
				, 'DeleteObject'
				, 'Reboot'
				, 'Download'
				, 'ScheduleDownload'
				, 'Upload'
				, 'FactoryReset'
				, 'GetQueuedTransfers'
				, 'GetAllQueuedTransfers'
				, 'CancelTransfer'
				, 'ScheduleInform'
				, 'ChangeDUState'
				, 'SetVouchers'
				, 'GetOptions'
			       ]).

-define(SUPPORTED_CPE_DEPRECATED_METHODS, ['GetQueuedTransfers'
					   , 'SetVouchers'
					   , 'GetOptions'
					  ]).

-define(SUPPORTED_ACS_METHODS, ['GetRPCMethods'
				, 'Inform'
				, 'TransferComplete'
				, 'AutonomousTransferComplete'
				, 'DUStateChangeComplete'
				, 'AutonomousDUStateChangeComplete'
				, 'RequestDownload'
				, 'Kicked'
			       ]).

-define(SUPPORTED_ACS_DEPRECATED_METHODS, ['cwmp:Kicked']).


-define(SUPPORTED_CPE_FAULT_CODES,
	[
	 %%        CPE Fault Codes from 9000 to 9799
	   {0,    "No fault"}
	 , {9000, "9000 - Method not supported"}
	 , {9001, "9001 - Request denied (no reason specified)"}
	 , {9002, "9002 - Internal error"}
	 , {9003, "9003 - Invalid arguments"}
	 , {9004, "9004 - Resources exceeded"}
	 , {9005, "9005 - Invalid parameter name"}
	 , {9006, "9006 - Invalid parameter type"}
	 , {9007, "9007 - Invalid parameter value"}
	 , {9008, "9008 - Attempt to set a non-writable parameter"}
	 , {9009, "9009 - Notification request rejected"}
	 , {9010, "9010 - File transfer failure"}
	 , {9011, "9011 - Upload failure"}
	 , {9012, "9012 - File transfer server authentication failure"}
	 , {9013, "9013 - Unsupported protocol for file transfer"}
	 , {9014, "9014 - File transfer failure: unable to join multicast group"}
	 , {9015, "9015 - File transfer failure: unable to contact file server"}
	 , {9016, "9016 - File transfer failure: unable to access file"}
	 , {9017, "9017 - File transfer failure: unable to complete download"}
	 , {9018, "9018 - File transfer failure: file corrupted"}
	 , {9019, "9019 - File transfer failure: file authentication failure"}
	 , {9020, "9020 - File transfer failure: unable to complete download within specified time windows"}
	 , {9021, "9021 - Cancelation of file transfer not permitted in current transfer state"}
	 , {9022, "9022 - Invalid UUID Format"}
	 , {9023, "9023 - Unknown Execution Environment"}
	 , {9024, "9024 - Disabled Execution Environment"}
	 , {9025, "9025 - Deployment Unit to Execution Environment Mismatch"}
	 , {9026, "9026 - Duplicate Deployment Unit"}
	 , {9027, "9027 - System Resources Exceeded"}
	 , {9028, "9028 - Unknown Deployment Unit"}
	 , {9029, "9029 - Invalid Deployment Unit State"}
	 , {9030, "9030 - Invalid Deployement Unit Update – Downgrade not permitted"}
	 , {9031, "9031 - Invalid Deployement Unit Update – Version not specified"}
	 , {9032, "9032 - Invalid Deployment Unit Update – Version already exists"}
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
%FIXME: check file types
-define(SUPPORTED_TRANSFER_FILE_TYPE,
	%% Pattern allows the following File Types:
	[ {1, "1 Firmware Upgrade, Image"}
	  , {2, "2 Web, Content"}
	  , {3, "3 Vendor Configuration, File"}
	  , {4, "4 Vendor Log, File"}
	  , {4, "5 Tone, File"}
	  , {5, "6 Ringer, File"}
	  , {6, "7 Vendor Configuration File, [1-9]\d*"}
	  , {7, "8 Vendor Log File, [1-9]\d*"}
	  , {'X', "X OUI Vendor speceific, ID"}
	]).
-type transfer_file_type() :: integer() | string().


%% @doc This type is used for Download and RequestDownload
-define(SUPPORTED_DOWNLOAD_FILE_TYPE,
	%% This pattern allows the following File Types:
	[ {1, "1 Firmware Upgrade, Image"}
	  , {2, "2 Web, Content"}
	  , {3, "3 Vendor Configuration, File"}
	  , {4, "4 Tone, File"}
	  , {5, "5 Ringer, File"}
	  , {'X', "X OUI Vendor speceific, ID"}
	]).
-type download_file_type() :: integer() | string().

%% @doc This type is used for Upload
-define(SUPPORTED_UPLOAD_FILE_TYPE,
	%% This pattern allows the following File Types:
	[ {1, "1 Vendor Configuration, File"}
	  , {2, "2 Vendor Log, File"}
	  , {3, "3 Vendor Configuration File, [1-9]\d*"}
	  , {4, "4 Vendor Log File, [1-9]\d*"}
	  , {'X', "X OUI Vendor speceific, ID"}
	]).
-type upload_file_type() :: integer() | string().

-define(SUPPORTED_EVENT_CODE_TYPE,
        %%    This pattern allows the following Event Codes:
	[ {0,  "0 BOOTSTRAP"}
	  , {1,  "1 BOOT"}
	  , {2,  "2 PERIODIC"}
	  , {3,  "3 SCHEDULED"}
	  , {4,  "4 VALUE, CHANGE"}
	  , {5,  "5 KICKED"}
	  , {6,  "6 CONNECTION, REQUEST"}
	  , {7,  "7 TRANSFER, COMPLETE"}
	  , {8,  "8 DIAGNOSTICS, COMPLETE"}
	  , {9,  "9 REQUEST, DOWNLOAD"}
	  , {10, "10 AUTONOMOUS TRANSFER, COMPLETE"}
	  , {11, "11 DU STATE CHANGE, COMPLETE"}
	  , {12, "12 AUTONOMOUS DU STATE CHANGE, COMPLETE"}
	  , {'Reboot',		 "M Reboot"}
	  , {'ScheduleInform',	 "M ScheduleInform"}
	  , {'Download',	 "M Download"}
	  , {'ScheduleDownload', "M ScheduleDownload"}
	  , {'Upload',		 "M Upload"}
	  , {'ChangeDUState',	 "M ChangeDUState"}
	]).
-type event_code_type() :: 1-12 | atom().

%% @doc
%%    Extendable Time Window Mode Definition
-define(SUPPORTED_TIME_WINDOW_MODE_VALUE_TYPE,
	%%     This pattern allows the following Time Window Modes:
	[ {1,     "1 At Any, Time"}
	  , {2,	  "2 Immediately"}
	  , {3,	  "3 When, Idle"}
	  , {4,	  "4 Confirmation, Needed"}
	  , {'X', "OUI Vendor speceific, ID"}
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

-record(parameter_value_struct,	{
	  name  :: string(),
	  value :: any()
	 }).


%%    GetRPCMethods Type Definition
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

%%    Get Parameter Names Type Definitions
-record(parameter_info_struct, {
	  name :: string(),
	  writable :: boolean()
	 }).

%%    Get/Set Parameter Attributes Type Definitions

-type access_list_value_type() :: string().

-type parameter_attribute_notification_value_type() :: 1 | 2 | 3 | 4 | 5 | 6.

-record(set_parameter_attributes_struct, {
	  name :: string(),
	  notification_change :: boolean(),
	  notification :: parameter_attribute_notification_value_type(),
	  access_list_change :: boolean(),
	  access_list :: [access_list_value_type()]
	 }).

-record(parameter_attribute_struct, {
	  name :: string(),
	  notification :: parameter_attribute_notification_value_type(),
	  access_list :: [access_list_value_type()]
	  }).

%%    Schedule Download Time Window Type Definitions

-record(time_window_struct, {
	  window_start :: non_neg_integer(),
	  window_end   :: non_neg_integer(),
	  window_mode  :: time_window_mode_value_type(),
	  user_message :: string(),
	  max_retries  :: integer()
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


-record(all_queued_transfer_struct, {
	  command_key :: command_key_type(),
	  state :: transfer_state_type(),
	  is_download :: boolean(),
	  file_type :: string(),
	  file_size :: non_neg_integer(),
	  target_file_name :: string()
	 }).


%% @doc
%%        A unique identifier for a Deployment Unit
%%
-type deployment_unit_uuid() :: string().

%% @doc
%%        The state of a Deployment Unit on the device
%%
-define(SUPPORTED_DEPLOYMENT_UNIT_STATE,
	['Installed', 'Uninstalled', 'Failed']).

-define(SUPPORTED_DEPLOYMENT_UNIT_OPERATION_TYPE,
	['Install', 'Update', 'Uninstall']).



%% @doc
%%        An operation indicating a Deployment Unit should be installed
%%
-record(install_op_struct, {
	  url :: anyURI(),
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
	  url :: anyURI(),
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
%%        A base type for Deployment Unit operations that can be performed on a device
%%
-type operation_struct() :: #install_op_struct{} | #update_op_struct{} | #uninstall_op_struct{}.


%% @doc
%%        The result of a Deployment Unit operation performed on the device
%%
-record(op_result_struct, {
	  uuid :: deployment_unit_uuid(),
	  deployment_unit_ref :: string(),
	  version :: string(),
	  current_state :: string(),
	  resolved :: boolean(),
	  execution_unit_ref_list :: string(), %FIXME: parse comma separated list: "ExecutionUnitRefList" type="xs:string"
	  start_time :: date_time(),
	  complete_time :: date_time(),
	  fault :: #deployment_unit_fault_struct{}
	 }).

%% @doc
%%        The result of a Deployment Unit operation performed autonomously on the device (i.e., not at the
%%        direct request of the ACS)
%%
-record(auton_op_result_struct, {
	  op_result_struct :: #op_result_struct{},
	  operation_performed :: string()
	 }).

%%   Voucher and Option Type Definitions

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
	  start_date :: date_time(),
	  expiration_date :: date_time(),
	  is_transferable :: 0 % Non-transferable
			   | 1 % Transferable
	 }).

-record(arg_struct, {
	  name :: string(),
	  value :: string()
	 }).


%% -record(file_type_arg, {
%%  	  arg_struct :: [#arg_struct{}]
%%  	 }).




%%%-----------------------------------------------------------------------------
%%% Fault Definition
%%%-----------------------------------------------------------------------------

%% @doc
-record(set_parameter_values_fault, {
	   parameter_name :: string(),
	   fault_code :: cpe_fault_code_type()
		       | cpe_vendor_fault_code_type (),
	   fault_string :: string()
	  }).

-record(fault, {
	   fault_code :: cpe_fault_code_type()
		       | cpe_vendor_fault_code_type ()
		       | acs_fault_code_type()
		       | acs_vendor_fault_code_type(),
	   fault_string :: string(),
	   set_parameter_values_fault :: #set_parameter_values_fault{}
	  }).


%%%-----------------------------------------------------------------------------
%%% Generic RPC Messages - Annex A.3.1
%%%-----------------------------------------------------------------------------

%% @doc GeRPCMethods message - Annex A.3.1.1
-record(get_rpc_methods, {
	   %% TODO: add ref
	  }).

%% @doc GeRPCMethodsResponse message - Annex A.3.1.1
-record(get_rpc_methods_response, {
	   method_list :: [string()]
	  }).


%%%-----------------------------------------------------------------------------
%%% CPE messages - Annex A.3.2
%%%-----------------------------------------------------------------------------

%% @doc SetParameterValues message - Annex A.3.2.1
-record(set_parameter_values, {
	   parameter_list :: [#parameter_value_struct{}],
	   parameter_key :: parameter_key_type()
	  }).

%% @doc SetParameterValuesResponse message - Annex A.3.2.1
-record(set_parameter_values_response, {
	   status :: 0 %% All Parameter changes have been validated and applied
		   | 1
		     %% All Parameter changes have been validated and committed,
		     %% but some or all are not yet applied (for example, if a reboot
		     %% is required before the new values are applied)
	  }).

%% @doc GetParameterValues message - Annex A.3.2.2
-record(get_parameter_values, {
	   parameter_names :: [string()]
	  }).

%% @doc GetParameterValuesResponse message - Annex A.3.2.2
-record(get_parameter_values_response, {
	   parameter_list :: [#parameter_value_struct{}]
	  }).

%% @doc GetParameterNames message - Annex A.3.2.3
-record(get_parameter_names, {
	   parameter_path :: string(),
	   next_level :: boolean()
	  }).

%% @doc GetParameterNamesResponse message - Annex A.3.2.3
-record(get_parameter_names_response, {
	   parameter_list :: [#parameter_info_struct{}]
	  }).

%% @doc SetParameterAttributes message - Annex A.3.2.4
-record(set_parameter_attributes, {
	   parameter_list ::  [#set_parameter_attributes_struct{}]
	  }).

%% @doc SetParameterAttributesResponse message - Annex A.3.2.4
-record(set_parameter_attributes_response, {
	   %% TODO: ref
	  }).

%% @doc GetParameterAttributes message - Annex A.3.2.5
-record(get_parameter_attributes, {
	   parameter_names :: [string()]
	  }).

%% @doc GetParameterAttributesResponse message - Annex A.3.2.5
-record(get_parameter_attributes_response, {
	   parameter_list :: [#parameter_attribute_struct{}]
	  }).

%% @doc AddObject message - Annex A.3.2.6 xmlText
-record(add_object, {
	   object_name :: object_name_type(),
	   parameter_key :: parameter_key_type()
	  }).

%% @doc AddObjectResponse message - Annex A.3.2.6
-record(add_object_response, {
	   instance_number :: non_neg_integer(),
	   status :: 0 | 1
		     %% 0 - The object has been created
		     %% 1 - The object creation has been validated and committed, but not yet applied
	  }).

%% @doc DeleteObject message - Annex A.3.2.7
-record(delete_object, {
	   object_name :: object_name_type(),
	   parameter_key :: parameter_key_type()
	  }).

%% @doc DeleteObjectResponse message - Annex A.3.2.7
-record(delete_object_response, {
	   status :: 0 | 1
		     %% The object has been deleted
		     %% The object deletion has been validated and committed, but not yet applied
	  }).

%% @doc Download message - Annex A.3.2.8
-record(download, {
	   command_key :: command_key_type(),
	   file_type :: download_file_type(),
	   url :: url(),
	   username :: string(),
	   password :: string(),
	   file_size :: non_neg_integer(),
	   target_file_name :: string(),
	   delay_seconds :: non_neg_integer(),
	   success_url :: url(),
	   failure_url :: url()
	  }).

%% @doc DownloadResponse message - Annex A.3.2.8
-record(download_response, {
	   status :: 0 | 1,
	   %% 0 - Download has completed and been applied
	   %% 1 - Download has not yet been completed and applied
	   start_time :: date_time(),
	   complete_time :: date_time()
	  }).

%% @doc Reboot message - Annex A.3.2.9
-record(reboot, {
	   command_key :: command_key_type()
	  }).

%% @doc RebootResponse message - Annex A.3.2.9
-record(reboot_response, {
	  }).

%%%-----------------------------------------------------------------------------
%%%        Optional CPE messages - Annex A.4.1
%%%-----------------------------------------------------------------------------

%% @doc GetQueuedTransfers message - Annex A.4.1.1
-record(get_queued_transfers, {
	  }).

%% @doc GetQueuedTransfersResponse message - Annex A.4.1.1
-record(get_queued_transfers_response, {
	   transfer_list ::  [#queued_transfer_struct{}]
	  }).

%% @doc ScheduleInform message - Annex A.4.1.2
-record(schedule_inform, {
	   delay_seconds :: non_neg_integer(),
	   command_key :: command_key_type()
	  }).

%% @doc ScheduleInformResponse message - Annex A.4.1.2
-record(schedule_inform_response, {
	  }).

%% @doc SetVouchers message - Annex A.4.1.3
-record(set_vouchers, {
	   voucher_list :: [binary()]
	  }).

%% @doc SetVouchersResponse message - Annex A.4.1.3
-record(set_vouchers_response, {
	  }).

%% @doc GetOptions message - Annex A.4.1.4
-record(get_options, {
	   option_name :: string()
	  }).

%% @doc GetOptionsResponse message - Annex A.4.1.4
-record(get_options_response, {
	   option_list :: [#option_struct{}]
	  }).

%% @doc Upload message - Annex A.4.1.5
-record(upload, {
	   command_key :: command_key_type(),
	   file_type :: upload_file_type(),
	   url :: url(),
	   username :: string(),
	   password :: string(),
	   delay_seconds :: non_neg_integer()
	  }).

%% @doc UploadResponse message - Annex A.4.1.5
-record(upload_response, {
	   status :: 0 | 1,
	   %% 0 - Upload has been completed
	   %% 1 - Upload has not yet completed
	   start_time :: date_time(),
	   complete_time :: date_time()
	  }).

%% @doc FactoryReset message - Annex A.4.1.6
-record(factory_reset, {
	  }).

%% @doc FactoryResetResponse message - Annex A.4.1.6
-record(factory_reset_response, {
	  }).

%% @doc GetAllQueuedTransfers message - Annex A.4.1.7
-record(get_all_queued_transfers, {
	  }).

%% @doc GetAllQueuedTransfersResponse message - Annex A.4.1.7
-record(get_all_queued_transfers_response, {
	   transfer_list :: [#all_queued_transfer_struct{}]
	  }).

%% @doc ScheduleDownload message - Annex A.4.1.8
-record(schedule_download, {
	   command_key :: command_key_type(),
	   file_type :: download_file_type(),
	   url :: url(),
	   username :: string(),
	   password :: string(),
	   file_size :: non_neg_integer(),
	   target_file_name :: string(),
	   time_window_list ::  [#time_window_struct{}]
	  }).

%% @doc ScheduleDownloadResponse message - Annex A.4.1.8
-record(schedule_download_response, {

	  }).

%% @doc CancelTransfer message - Annex A.4.1.9
-record(cancel_transfer, {
	   command_key :: command_key_type()
	  }).

%% @doc CancelTransferResponse message - Annex A.4.1.9
-record(cancel_transfer_response, {

	  }).

%% @doc
%% A request to perform an action on a Deployment Unit on the device
-record(change_du_state, {
	   operations :: [operation_struct()],
	   command_key :: command_key_type()
	  }).

%% @doc
%%        Response to a ChangeDUState message
-record(change_du_state_response, {

	  }).

%% @doc Inform message - Annex A.3.3.1
-record(inform, {
	   device_id :: #device_id_struct{},
	   event :: [#event_struct{}],
	   max_envelopes :: non_neg_integer(),
	   current_time :: date_time(),
	   retry_count :: non_neg_integer(),
	   parameter_list :: [#parameter_value_struct{}]
	  }).

%% @doc InformResponse message - Annex A.3.3.1
-record(inform_response, {
	   max_envelopes :: non_neg_integer()
	  }).

%% @doc TransferComplete message - Annex A.3.3.2
-record(transfer_complete, {
	   command_key :: command_key_type(),
	   fault_struct :: #transfer_complete_fault_struct{},
	   start_time :: date_time(),
	   complete_time :: date_time()
	  }).

%% @doc TransferCompleteResponse message - Annex A.3.3.2
-record(transfer_complete_response, {
	   %% TODO: ref
	  }).

%% @doc AutonomousTransferComplete message - Annex A.3.3.3
-record(autonomous_transfer_complete, {
	   announce_url :: url(),
	   transfer_url :: url(),
	   is_download :: boolean(),
	   file_type :: string(),
	   file_size :: non_neg_integer(),
	   target_file_name :: string(),
	   fault_struct :: #transfer_complete_fault_struct{},
	   start_time :: date_time(),
	   complete_time :: date_time()
	  }).

%% @doc AutonomousTransferCompleteResponse message - Annex A.3.3.3
-record(autonomous_transfer_complete_response, {

	  }).

%%%-----------------------------------------------------------------------------
%%%       Optional ACS messages - Annex A.4.2
%%%-----------------------------------------------------------------------------

%% @doc Kicked message - Annex A.4.2.1
-record(kicked, {
	   command :: string(),
	   referer :: string(),
	   arg :: string(),
	   next :: string()
	  }).

%% @doc KickedResponse message - Annex A.4.2.1
-record(kicked_response, {
	   next_url :: url()
	  }).

%% @doc RequestDownload message - Annex A.4.2.2
-record(request_download, {
	  file_type :: download_file_type(),
	  file_type_arg :: [#arg_struct{}]
	 }).

%% @doc RequestDownloadResponse message - Annex A.4.2.2
-record(request_download_response, {
	  }).

%% @doc A message indicating a prior ChangeDUState request to perform
%% an action on a Deployment Unit on the device has completed
-record(du_state_change_complete, {
	   results :: #op_result_struct{},
	   command_key :: command_key_type()
	  }).

%% @doc Response to a DUStateChangeComplete message
-record(du_state_change_complete_response, {
	  }).

%% @doc A message indicating an autonomous action for a Deployment
%%      Unit on the device has completed
-record(autonomous_du_state_change_complete, {
	   results :: #auton_op_result_struct{}
	  }).

%% @doc Response to a AutonomousDUStateChangeComplete message
-record(autonomous_du_state_change_complete_response, {
}).

%%%-----------------------------------------------------------------------------
%%%        SOAP Header Elements
%%%-----------------------------------------------------------------------------

%% @doc
-record(id, {
           mustUnderstand :: boolean(),
	   value :: string()
	  }).

%% @doc
-record(hold_requests, {
           mustUnderstand :: boolean(),
	   value :: boolean()
	  }).

-record(header,{
          id :: #id{},
          hold_requests :: #hold_requests{},
          %% Configurable Amendment 1 Session Termination
          %% NoMoreRequests header element is deprecated
          no_more_requests :: boolean()
         }).

%% @Doc Fault reporting structure
-record(soap_fault,{
	  faultcode   :: qName(),
	  faultstring :: string(),
	  faultactor  :: anyURI(),
	  detail      :: #fault{}
         }).

-record(envelope,{
          header :: #header{},
          body   :: [body_type()]
         }).




-type body_type() ::
        #add_object{}                          | #add_object_response{}
      | #autonomous_du_state_change_complete{} | #autonomous_du_state_change_complete_response{}
      | #autonomous_transfer_complete{}        | #autonomous_transfer_complete_response{}
      | #cancel_transfer{}                     | #cancel_transfer_response{}
      | #change_du_state{}                     | #change_du_state_response{}
      | #delete_object{}                       | #delete_object_response{}
      | #download{}                            | #download_response{}
      | #du_state_change_complete{}            | #du_state_change_complete_response{}
      | #factory_reset{}                       | #factory_reset_response{}
      | #get_all_queued_transfers{}            | #get_all_queued_transfers_response{}
      | #get_options{}                         | #get_options_response{}
      | #get_parameter_attributes{}            | #get_parameter_attributes_response{}
      | #get_parameter_names{}                 | #get_parameter_names_response{}
      | #get_parameter_values{}                | #get_parameter_values_response{}
      | #get_queued_transfers{}                | #get_queued_transfers_response{}
      | #get_rpc_methods{}                     | #get_rpc_methods_response{}
      | #inform{}                              | #inform_response{}
      | #kicked{}                              | #kicked_response{}
      | #reboot{}                              | #reboot_response{}
      | #request_download{}                    | #request_download_response{}
      | #schedule_download{}                   | #schedule_download_response{}
      | #schedule_inform{}                     | #schedule_inform_response{}
      | #set_parameter_attributes{}            | #set_parameter_attributes_response{}
      | #set_parameter_values{}                | #set_parameter_values_response{}
      | #set_vouchers{}                        | #set_vouchers_response{}
      | #transfer_complete{}                   | #transfer_complete_response{}
      | #upload{}                              | #upload_response{}
      | #soap_fault{}
        .


%%%-----------------------------------------------------------------------------
%%%        RPC Message
%%%-----------------------------------------------------------------------------
-record(cwmp_obj, {
%          type :: cwmp_method(),
          data :: #envelope{}
         }).

%%%-----------------------------------------------------------------------------
%%%        Parser context
%%%-----------------------------------------------------------------------------

-record(rpc_ns, {
	  ns_xsd     :: atom(),
	  ns_envelop :: atom(),
	  ns_encoding:: atom(),
	  ns_cwmp    :: atom(),
	  cwmp_version = 1 :: cwmp_version(),
	  inherited  :: atom()
	 }).

-type builder_option() :: {version, cwmp_version()} | {handler, function()} | {namespaces, #rpc_ns{}}.
-type parser_option() :: {version, cwmp_version()} | {object_hook, function()}.

-record(builder, {version = 1  :: cwmp_version(),
                  handler = null,
		  ns :: #rpc_ns{}
		 }).

-record(parser, { version = 1  :: cwmp_version(),
                  object_hook = null,
                  state       = null,
                  ns :: #rpc_ns{}
                 }).

-type export_element() :: { atom(), list(), list()}
			| {atom(), list()}
			| atom()
			| string()
			| #xmlText{}
			| #xmlElement{}
			| #xmlPI{}
			| #xmlComment{}
			| #xmlDecl{}
			  .

