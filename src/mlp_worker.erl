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

init([InSock, Request, Headers, Host, Parent]) ->
	io:format("Start new worker: ~p parent: ~p~n", [self(), Parent]),
	io:format("Request: ~p~nHeaders: ~p Host: ~p~n", [Request, Headers, Host]),
	State =  #state{isock = InSock, parent = Parent},
	{ok, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(Msg, State) ->
	io:format("Unknown message: ~p~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
