-module(mlp_worker_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/5]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER},?MODULE, []).

start_child(InSock, Request, Headers, Host, Worker) ->
	supervisor:start_child(Worker, [InSock, Request, Headers, Host]).


init([]) ->
	Worker = {mlp_worker, {mlp_worker, start_link, []},
		temporary, brutal_kill, worker, [mlp_worker]},
	Procs = [Worker],
	{ok, {{simple_one_for_one, 1000, 3600}, Procs}}.
