-module(mylib).

-export([start/0]).

						% application start callbacks
-export([start/2]).

%% Supervisor callbacks
-export([start_sup/0, init/1]).


-export([start_task/1, start_task_producer/2, task_producer/2, start_task_consumer/0, task_consumer/0, tasks/0]).


tasks() ->
    [TID || {{task,TID},_,_,_} <- supervisor:which_children(tasks_sup)].

						% Shell start code
start() ->
    AppDesc = {application, mylib, [
				    {mod, { mylib, []}}
				    ,{env, []}
				   ]},
    application:load(AppDesc),
    ok = application:start(mylib).


						% Application start callback  
start(normal, _) ->
    start_sup().


start_sup() ->
    supervisor:start_link({local, mylib_sup}, ?MODULE, []).


start_task(TaskId) ->
    {ok, Sup} = supervisor:start_child(tasks_sup, {
					 {task, TaskId},
					 {supervisor, start_link, [?MODULE, [task]]},
					 temporary, % this is important. It is temporary inside tasks_sup
					 infinity,
					 supervisor,
					 []
					}),


    {ok, Consumer} = supervisor:start_child(Sup, {
					      consumer,
					      {?MODULE, start_task_consumer, []},
					      permanent, % and this should be restarted to kill whole supervisor
					      2000,
					      worker,
					      []
					     }),

    {ok, Producer} = supervisor:start_child(Sup, {
					      producer,
					      {?MODULE, start_task_producer, [Consumer, 100]},
					      permanent,
					      2000,
					      worker,
					      []
					     }),
    {ok, Producer}.

start_task_consumer() ->
    Pid = spawn_link(?MODULE, task_consumer, []),
    {ok, Pid}.

task_consumer() ->
    receive
	{tick, Percent} -> put(percent, Percent)
    end,
    task_consumer().

start_task_producer(Consumer, Percent) ->
    Pid = spawn_link(?MODULE, task_producer, [Consumer, Percent]),
    {ok, Pid}.

task_producer(_Consumer, 0) ->
    ok;

task_producer(Consumer, Percent) ->
    timer:sleep(500),
    Consumer ! {tick, Percent},
    task_producer(Consumer, Percent - 1).


init([task]) ->
    {ok, {{one_for_all, 0, 10}, []}};

init([tasks]) ->
    {ok, {{one_for_one, 5, 10}, []}};

init([]) ->
    Supervisors = [
		   {tasks,
		    {supervisor, start_link, [{local, tasks_sup}, ?MODULE, [tasks]]},
		    permanent,
		    infinity,
		    supervisor,
		    []
		   }
		  ],
    {ok, {{one_for_one, 5, 10}, Supervisors}}.
