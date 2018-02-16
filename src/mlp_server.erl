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

-record(state, {lsock, osock, socket, request_line, method, headers = [],
		host, user_data, parent}).


%%%% API %%%%
start_link(LSock, UserArgs) ->
	gen_server:start(?MODULE, [LSock, UserArgs, self()], []).


%%%% Callbacks %%%%%

init([LSock, UserArgs, Parent]) ->
	io:format("Start new link: ~p, with parent: ~p~n", [self(), Parent]),
	State = #state{lsock = LSock, user_data = UserArgs, parent = Parent},
	{ok, State, 0}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({http, Sock, {http_request, Method, Url, _}=Request}, State) ->
	io:format("Socket: ~p Get HTTP Method: ~p URL: ~p~nRequest: ~p~n", [Sock, Method, Url, Request]),
	inet:setopts(State#state.socket, [{active, once}]),
	Req = handle_request(Request),
	{noreply, State#state{request_line = Req, method = Method}};
handle_info({http, _Sock, {http_header, _, 'Host', _, Value}}, State) ->
	inet:setopts(State#state.socket, [{active, once}]),
	{Host, Port} = get_host(Value),
	io:format("Host: ~p, Port: ~p~n", [Host, Port]),
	io:format("Get HTTP Header: ~p:~p~n", ['Host', Value]),
	{noreply, State#state{headers = [{'Host', Value} | State#state.headers], host = {Host, Port}}};
handle_info({http, _Sock, {http_header, _, Name, _, Value}}, State) ->
	inet:setopts(State#state.socket, [{active, once}]),
	io:format("Get HTTP Header: ~p:~p~n", [Name, Value]),
	{noreply, State#state{headers = [{Name, Value} | State#state.headers]}};
handle_info({http, _Sock, http_eoh}, #state{request_line = Request, host = {Host, Port}} = State) ->
	io:format("End of Headers~n"),
	inet:setopts(State#state.socket, [{active, true}, {packet, raw}]),
	case gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, raw}, {keepalive, true}, {reuseaddr, true}]) of
		{ok, Socket} ->
			io:format("OutPutSocket: ~p, Request: ~p ~n", [Socket, Request]),
			Headers = headers(State#state.headers),
			case State#state.method of
				<<"CONNECT">> ->
%					Reply = io_lib:format("~s\r\n~s\r\nContent-Length: ~w\r\n\r\n", [
%							<<"HTTP/1.0 200 Connection Established">>,
%							<<"Proxy-agent: MLP-Proxy/0.1">>,0]),
%					io:format("Connect: ~p~n", [Reply]),
%					gen_tcp:send(State#state.socket, Reply);
					gen_tcp:send(State#state.socket, <<"HTTP/1.0 200 Connection established",$\r,$\n,$\r,$\n>>);
%					gen_tcp:send(State#state.socket, <<"HTTP/1.1 200 Connection Established",$\r,$\n,$\r,$\n>>);
				_ -> 
					gen_tcp:send(Socket, Request),
					gen_tcp:send(Socket, Headers),
					gen_tcp:send(Socket, <<$\r, $\n>>)
			end,
%			headers(State#state.headers),
			{noreply, State#state{osock = Socket, request_line = undefined, headers = undefined}};
		{error, Reason} ->
			io:format("Reason: ~p~n", [Reason]),
			{stop, normal, Reason}
	end;	
handle_info({http, InSock, {http_error, Data}}, #state{osock = OutSock, socket = InSock} = State) ->
	io:format("Non HTTP fom browser: ~p~n", [Data]),
	Res = gen_tcp:send(OutSock, Data),
	inet:setopts(State#state.socket, [{active, once}]),
	io:format("Res to site: ~p~n", [Res]),
	{noreply, State};

handle_info({tcp, OutSock, Data}, #state{osock = OutSock, socket = InSock} = State) ->
	io:format("Data from ~p: ~p~n", [OutSock, Data]),
	Res = gen_tcp:send(InSock, Data),
	io:format("Res to browser: ~p~n", [Res]),
	{noreply, State};

handle_info({tcp, InSock, Data}, #state{osock = OutSock, socket = InSock} = State) ->
	io:format("Data from ~p: ~p~n", [InSock, Data]),
	Res = gen_tcp:send(OutSock, Data),
	io:format("Res to site: ~p~n", [Res]),
	{noreply, State};

handle_info({tcp, _Sock, Data}, State) ->
	io:format("Data: ~p~n", [Data]),
	{noreply, State};

handle_info({tcp_closed, _Sock}, State) ->
	{stop, normal, State};

handle_info(timeout, #state{lsock = LSock, parent = Parent} = State) ->
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

handle_request({_, Method, {absoluteURI, _Prot, _Host, undefined, Path}, {V0,V1}}) ->
	Req = io_lib:format("~s ~s HTTP/~w.~w\r\n", [Method, Path, V0, V1]),
%	Req = io_lib:format("~s ~s://~s~s HTTP/~w.~w\r\n", [Method, Prot, Host, Path, V0, V1]),
	io:format("Req: ~p~n", [Req]),
	Req;
handle_request({_, Method, {absoluteURI, _Prot, _Host, _Port, Path}, {V0,V1}}) ->
	Req = io_lib:format("~s ~s HTTP/~w.~w\r\n", [Method, Path, V0, V1]),
%	Req = io_lib:format("~s ~s://~s:~s~s HTTP/~w.~w\r\n", [Method, Prot, Host, Port, Path, V0, V1]),
	io:format("Req: ~p~n", [Req]),
	Req;
handle_request({_, Method, {scheme, Host, Port}, {V0,V1}}) ->
	Req = io_lib:format("~s ~s:~s HTTP/~w.~w\r\n\r\n", [Method, Host, Port, V0, V1]),
	io:format("Req: ~p~n", [Req]),
	Req.

headers([{Header, Text} | Hs]) ->
	[io_lib:format("~s: ~s\r\n", [Header, Text]) | headers(Hs)];
headers([undefined | Hs]) ->
	headers(Hs);
headers([]) ->
	[].
