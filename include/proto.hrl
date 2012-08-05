%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%%-----------------------------------------------------------------------------

-type cpe_method () ::
	'cwmp:GetRPCMethods'  
      | 'cwmp:SetParameterValues'  
      | 'cwmp:GetParameterValues'  
      | 'cwmp:GetParameterNames'  
      | 'cwmp:SetParameterAttributes'  
      | 'cwmp:GetParameterAttributes'  
      | 'cwmp:AddObject'  
      | 'cwmp:DeleteObject'  
      | 'cwmp:Reboot'  
      | 'cwmp:Download'
      | 'cwmp:ScheduleDownload'
      | 'cwmp:Upload'  
      | 'cwmp:FactoryReset'  
%% Deprecated         | 'cwmp:GetQueuedTransfers'  
      | 'cwmp:GetAllQueuedTransfers'
      | 'CancelTransfer'
      | 'cwmp:ScheduleInform'
      | 'ChangeDUState'
%% Deprecated      | 'cwmp:SetVouchers'  
%% Deprecated      | 'cwmp:GetOptions' 
	.

-type acs_method () ::
	'cwmp:GetRPCMethods'  
      | 'cwmp:Inform'  
      | 'cwmp:TransferComplete'  
      | 'cwmp:AutonomousTransferComplete'
      | 'cwmp:DUStateChangeComplete'
      | 'AutonomousDUStateChangeComplete'
      | 'cwmp:RequestDownload'  
%% Deprecated      | 'cwmp:Kicked'
	.


-type rpc_data () :: string | int | unsignedInt | boolean | dateTime | base64 | anySimpleType.


-type packet_type() :: st_data | st_fin | st_state | st_reset | st_syn.

-record(connection, {
          pid :: pid(),
          socket :: port(),
          version :: integer(),
          role = equal :: role()
         }).

-type connection() :: #connection{}.

-type role() :: master | slave | equal.

-define(ACK_NO_MASK, 16#FFFF).
-define(REORDER_BUFFER_SIZE, 32).
