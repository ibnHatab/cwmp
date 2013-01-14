%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  Core functionality for ACS
%%% @end
%%% Created :  9 Oct 2012 by vlad <lib.aca55a@gmail.com>

-module(cwmp).

-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ok = ensure_started(crypto),
    cwmp_sup:start_link().

%% @spec start() -> ok
%% @doc Start the cwmp server.
start() ->
    ok = ensure_started(crypto),
    ok = application:start(cwmp),
    ok.

%% @spec stop() -> ok
%% @doc Stop the cwmp server.
stop() ->
    ok = application:stop(cwmp),
    ok = application:stop(crypto),
    ok.

