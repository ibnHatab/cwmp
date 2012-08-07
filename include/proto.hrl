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




-record(rpcData, {}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type packet_type() :: st_data | st_fin | st_state | st_reset | st_syn.

-record(connection, {
          pid		:: pid(),
          socket	:: port(),
          version	:: integer(),
          role = equal	:: role()
         }).

-type connection() :: #connection{}.

-type role() :: master | slave | equal.

-define(ACK_NO_MASK, 16#FFFF).
-define(REORDER_BUFFER_SIZE, 32).
