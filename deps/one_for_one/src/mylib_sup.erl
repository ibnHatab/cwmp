%%% File    : mylib_sup.erl
%%% Description : 


-module(mylib_sup).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [],
    MaxRestart = 2,
    RestartTime = 10,
    Settings = {one_for_one, MaxRestart, RestartTime},
    SupervisorSpec = {Settings, Children},
    {ok, SupervisorSpec}.
    
    


