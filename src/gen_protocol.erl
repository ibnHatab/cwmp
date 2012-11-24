%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%   This behavior define Protocol class.
%%%
%%%   X-Kernel Protocol Object behavior
%%%   - create_protocol
%%%   - open -> session
%%%   - open_enable
%%%   - open_done -> session
%%%   
%%% @end
%%% Created : 14 Oct 2012 by vlad <lib.aca55a@gmail.com>

-module(gen_protocol).

-type protocol() :: pid().
-type session() :: pid().

-export_type([protocol/0, session/0]).

%% Protocol Object methods

%% create_protocol
%% @doc This method initialize protocol.
-callback create_protocol(Config :: term()) ->
    {ok, Config :: term()} | {error, Reason :: term()}.

%% open
%% Active open on protocol object
-callback open(InvokingProtocol :: protocol(),
	       ParticipandSet :: list()) ->
    {ok, session()} | {error, Reason :: term()}.

%% open_enable
%% @doc Request for passive open on low-level protocol object
-callback open_enable(InvokingProtocol :: protocol(),
		      ParticipandSet :: list()) ->
    ok | {error, Reason :: term()}.

%% open_done
%% @doc Async response for passive open on high-level protocol object
-callback open_done(InvokingProtocol :: protocol(),
		    ParticipandSet :: list()) ->
    {ok, session()} | {error, Reason :: term()}.
