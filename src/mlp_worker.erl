-module(mlp_worker).

-behaviour(gen_server).

-export([start_link/4]).

-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
	]).

-record(state, {isock, osock, parent}).


%%%% API %%%%
start_link(InSock, Request, Headers, Host) ->
	gen_server:start(?MODULE, [InSock, Request, Headers, Host, self()], []).


%%%% Callbacks %%%%

init([InSock, Request, Headers, {Host, Port}, Parent]) ->
	io:format("Start new worker: ~p parent: ~p~n", [self(), Parent]),
	io:format("Request: ~p~nHeaders: ~p Host: ~p~n", [Request, Headers, Host]),
	case gen_tcp:connect(Host, Port, [{active, true}, {packet, raw}]) of
		{ok, Socket} ->
			io:format("OutPutSocket: ~p~n", [Socket]),
			gen_tcp:send(Socket, Request),
			gen_tcp:send(Socket, Headers),
%			send_list(Socket, Headers),
	%		inet:setopts(Socket, [{packet, raw}]),
	%		inet:setopts(InSock, [{packet, raw}]),
			State =  #state{isock = InSock, osock = Socket, parent = Parent},
			{ok, State};
		{error, Reason} ->
			io:format("Reason: ~p~n", [Reason]),
			{stop, Reason}
	end.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({tcp, OutSock, Data}, #state{isock = InSock, osock = OutSock} = State) ->
	io:format("Data: ~p~n", [Data]),
	gen_tcp:send(InSock, Data),
	{noreply, State};

handle_info({tcp_closed, _Sock}, State) ->
	{stop, normal, State};

handle_info(Msg, State) ->
	io:format("Unknown message: ~p~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%send_list(Socket, [H | T]) ->
%	gen_tcp:send(Socket, H),
%	send_list(Socket, T);
%send_list(_Socket, []) -> ok.
