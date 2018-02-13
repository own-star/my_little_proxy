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
		host, user_data, parent}).


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

handle_info({http, Sock, {http_request, Method, Url, _}=Request}, State) ->
	io:format("Socket: ~p Get HTTP Method: ~p URL: ~p~nRequest: ~p~n", [Sock, Method, Url, Request]),
	inet:setopts(State#state.socket, [{active, once}]),
	{noreply, State#state{request_line = Request}};
handle_info({http, _Sock, {http_header, _, 'Host', _, Value}=Header}, State) ->
	inet:setopts(State#state.socket, [{active, once}]),
	{Host, Port} = get_host(Value),
	io:format("Host: ~p, Port: ~p~n", [Host, Port]),
	io:format("Get HTTP Header: ~p:~p  Full: ~p~n", ['Host', Value, Header]),
	{noreply, State#state{headers = [Header | State#state.headers], host = {Host, Port}}};
handle_info({http, _Sock, {http_header, _, Name, _, Value}=Header}, State) ->
	inet:setopts(State#state.socket, [{active, once}]),
	io:format("Get HTTP Header: ~p:~p  Full: ~p~n", [Name, Value, Header]),
	{noreply, State#state{headers = [Header | State#state.headers]}};
handle_info({http, _Sock, http_eoh}, State) ->
	io:format("End of Headers~n"),
	{stop, normal, mlp_worker:start_link(State#state.socket, State#state.request_line, State#state.headers, State#state.host)};

handle_info({tcp, _Sock, Data}, State) ->
	io:format("Data: ~p~n", [Data]),
	{noreply, State};

handle_info({tcp_closed, _Sock}, State) ->
	{stop, normal, State};

handle_info(timeout, #state{ilsock = LSock, parent = Parent} = State) ->
	{ok, Socket} = gen_tcp:accept(LSock),
	io:format("New Socket: ~p~n", [Socket]),
	mlp_server_sup:start_child(Parent),
%	my_little_proxy_sup:start_child(Parent),
	inet:setopts(Socket, [{active, once}]),
	{noreply, State#state{socket = Socket}};

handle_info(Msg, State) ->
	io:format("Unknown message: ~p~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%% Internal %%%%

get_host(HostBin) ->
	get_host(HostBin, <<>>, []).

get_host(<<$., Rest/binary>>, Acc, Parts) ->
	get_host(Rest, <<>>, [Acc | Parts]);
get_host(<<$:, Port/binary>>, Acc, Parts) ->
	eval_host([Acc | Parts], Port);
get_host(<<X, Rest/binary>>, Acc, Parts) ->
	get_host(Rest, <<Acc/binary, X>>, Parts);
get_host(<<>>, Acc, Parts) ->
	eval_host([Acc | Parts], <<"80">>).

eval_host(Parts, Port) ->
	case length(Parts) of
		4 -> get_ip(Parts, Port);
		_ -> get_fqdn(Parts, Port)
	end.

get_fqdn(Fqdn, Port) ->
	get_fqdn(Fqdn, Port, <<>>).

get_fqdn([H], Port, Acc) ->
	{binary_to_list(<<H/binary, Acc/binary>>), binary_to_integer(Port)};
get_fqdn([H | T], Port, Acc) ->
	get_fqdn(T, Port, <<".", H/binary, Acc/binary>>).

get_ip(Parts, Port) ->
	get_ip(Parts, Port, Parts, []).

get_ip([H | T], Port, Parts, Acc) ->
	try	binary_to_integer(H) of
		Q  ->	get_ip(T, Port, Parts, [Q | Acc])
	catch
		_:_ -> get_fqdn(Parts, Port)
	end;
get_ip([], Port, _, Acc) ->
	reverse_ip(Acc, Port).

reverse_ip([Q1, Q2, Q3, Q4], Port) ->
	{{Q1, Q2, Q3, Q4}, binary_to_integer(Port)}.
