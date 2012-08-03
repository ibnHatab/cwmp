%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%%-----------------------------------------------------------------------------

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
