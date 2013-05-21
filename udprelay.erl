
-module(udprelay).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").

-record(context, {
	socket,
	recipients=[]
}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init(Opts) ->
	Port = proplists:get_value(port,Opts),
	?DEBUG("udprelay:init :: opening udp socket on ~p/~p port~n", [Port, erlang:is_integer(Port)]),

	{Ret, State} = case gen_udp:open(Port, [binary,{active,true},{reuseaddr,true}]) of
		{ok, Socket}    -> {ok, #context{socket=Socket}};
		{error, Reason} -> {stop, Reason};
		_ -> {foo,bar}
	end,

	?DEBUG("udprelay: init(port= ~p, state= ~p)~n", [Port, State]),
	{Ret, State}.


% sync request
handle_call(_Req, _From, State) ->
	?DEBUG("udprelay: handle_call ~p~n", [_Req]),
	{ok, State}.

% async request
handle_cast(_Req, State) ->
	?DEBUG("udprelay: handle_cast ~p~n", [_Req]),
	{noreply, State}.

% timeout, system msg
handle_info(_Info, State) ->
	?DEBUG("udprelay: handle_info ~p~n", [_Info]),
	{noreply, State}.
	
terminate(Reason, #context{socket=Socket}) ->
	?DEBUG("udprelay: terminating (socket ~p)~n", [Socket]),
	gen_udp:close(Socket).

code_change(OldSvn, State, _) -> {ok, State}.
