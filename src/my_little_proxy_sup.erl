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
%	BasicSockOpts = [binary,
%		{active, false},
%		{packet, http_bin},
%		{reuseaddr, true}],
%	SockOpts = case IP of
%		undefined -> BasicSockOpts;
%		_ -> [{ip, IP} | BasicSockOpts]
%	end,
%	{ok, LSock} = gen_tcp:listen(Port, SockOpts),
	Server = {mlp_server_sup, {mlp_server_sup, start_link,
			[IP, Port, UserArgs]},
		permanent, 2000, supervisor, [mlp_server_sup]},
	Worker = {mlp_worker_sup, {mlp_worker_sup, start_link, []},
		permanent, 2000, supervisor, [mlp_worker_sup]},
	Procs = [Server, Worker],
	{ok, {{one_for_one, 1, 5}, Procs}}.
