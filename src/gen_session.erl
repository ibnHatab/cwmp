%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%   This behavior define Session class.
%%%
%%%   Because in erlang any object comes with Mailbox and message demuxer,
%%%   we can implement Session object on spot.
%%%   
%%%   X-Kernel Session Object behavior
%%%   - push
%%%   - pop
%%%
%%% @end
%%% Created : 14 Oct 2012 by vlad <lib.aca55a@gmail.com>

-module(gen_session).


%%% Session Object methods

%% push
%% Invoked by a high-level session to pass messages to some low-level session
-callback push(Message :: term()) ->
    ok | {error, Reason :: term()}.

%% pop
%% Invoked by the 'demux' operation of a protocol to pass a message to one of its sessions
-callback pop(Message :: term()) ->
    ok | {error, Reason :: term()}.

