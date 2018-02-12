-module(my_little_proxy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(DEFAULT_PORT, 3128).

start(_Type, _Args) ->
	Port = case application:get_env(tcp_interface, port) of
		{ok, P} -> P;
		undefined ->?DEFAULT_PORT
	end,
	IP = application:get_env(tcp_interface, ipaddr),
	UserArgs = case application:get_env(tcp_interface, args) of
		{ok, A} -> A;
		undefined -> []
	end,
	my_little_proxy_sup:start_link(IP, Port, UserArgs).

stop(_State) ->
	ok.
