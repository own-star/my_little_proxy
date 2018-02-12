-module(my_little_proxy_sup).
-behaviour(supervisor).

-export([start_link/3, start_child/1]).
-export([init/1]).

start_link(IP, Port, UserArgs) ->
	{ok, Pid} = supervisor:start_link(?MODULE, [IP, Port, UserArgs]),
	start_child(Pid),
	{ok, Pid}.

start_child(Server) ->
	supervisor:start_child(Server, []).


%%%% Callbacks %%%%

init([IP, Port, UserArgs]) ->
	BasicSockOpts = [binary,
		{active, false},
		{packet, http_bin},
		{reuseaddr, true}],
	SockOpts = case IP of
		undefined -> BasicSockOpts;
		_ -> [{ip, IP} | BasicSockOpts]
	end,
	{ok, LSock} = gen_tcp:listen(Port, SockOpts),
	Server = {mlp_server, {mlp_server, start_link,
			[LSock, UserArgs]},
		temporary, brutal_kill, worker, [mlp_server]},
	Procs = [Server],
	{ok, {{simple_one_for_one, 1000, 3600}, Procs}}.
