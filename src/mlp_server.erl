-module(mlp_server).

-behaviour(gen_server).

-export([start_link/2]).

-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
	]).

-record(state, {ilsock, olsock, socket, request_line, headers = [],
		user_data, parent}).


%%%% API %%%%
start_link(LSock, UserArgs) ->
	gen_server:start(?MODULE, [LSock, UserArgs, self()], []).


%%%% Callbacks %%%%%

init([LSock, UserArgs, Parent]) ->
	io:format("Start new link: ~p, with parent: ~p~n", [self(), Parent]),
	State = #state{ilsock = LSock, user_data = UserArgs, parent = Parent},
	{ok, State, 0}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({tcp, _Sock, Data}, State) ->
	io:format("Data: ~p~n", [Data]),
	{noreply, State};

handle_info({tcp_closed, _Sock}, State) ->
	{stop, normal, State};

handle_info(timeout, #state{ilsock = LSock, parent = Parent} = State) ->
	{ok, Socket} = gen_tcp:accept(LSock),
	my_little_proxy_sup:start_child(Parent),
	inet:setopts(Socket, [{active, once}]),
	{noreply, State#state{socket = Socket}};

handle_info(Msg, State) ->
	io:format("Unknown message: ~p~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
