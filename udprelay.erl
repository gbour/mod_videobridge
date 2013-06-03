
-module(udprelay).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([addpeer/2, forward/2]).

-include("ejabberd.hrl").

-record(context, {
	controller,
	confid,
	port,
	rtpsock,
	rtcpsock,
	rtpsrc=undef,
	rtcpsrc=undef,
	recipients=[]
}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init(Opts) ->
	Port = proplists:get_value(port,Opts),
	Ctrl = proplists:get_value(controller,Opts),
	ConfId = proplists:get_value(confid, Opts),
	?DEBUG("udprelay:init :: opening udp socket on ~p port. Controller= ~p, conf=~p~n", [Port, Ctrl, ConfId]),

	% open RTP port
	{Ret, State} = case gen_udp:open(Port, [binary,{active,true},{reuseaddr,true}]) of
		{ok, RtpSock}   -> 
			case gen_udp:open(Port+1, [binary,{active,true},{reuseaddr,true}]) of
				{ok, RtcpSock} ->
					{ok, #context{controller=Ctrl, confid=ConfId, port=Port, 
							rtpsock=RtpSock, rtcpsock=RtcpSock}
					};
				{error, Reason} ->
					gen_udp:close(RtpSock),
					{stop, Reason}
			end;
		{error, Reason} -> {stop, Reason}
	end,

	?DEBUG("udprelay: init(port= ~p, state= ~p)~n", [Port, State]),
	{Ret, State}.

addpeer(Self, Pid) ->
	gen_server:call(Self, {addpeer,Pid}).

forward(Pid, Packet) ->
	gen_server:cast(Pid, {forward, Packet}).
%rtpport(Self) ->
	
	

% sync request
handle_call({addpeer, Pid}, _, State=#context{recipients=R}) ->
	?DEBUG("udprelay: add recipient ~p~n",[Pid]),
	{reply, ok, State#context{recipients=[Pid|R]}};
handle_call(_Req, _From, State) ->
	?DEBUG("udprelay: handle_call ~p~n", [_Req]),
	{noreply, State}.

% async request
handle_cast({forward, {rtp, Packet}}, State=#context{rtpsock=Sock,rtpsrc={Ip,Port}}) ->
	?DEBUG("udprelay: relay to ~p:~p~n", [Ip, Port]),
	case gen_udp:send(Sock, Ip, Port, Packet) of
		{error,Reason} ->
			?DEBUG("udprelay:forward= fail (~p)~n", [Reason]);
		_ -> ok
	end,
	{noreply, State};

handle_cast(_Req, State) ->
	?DEBUG("udprelay: handle_cast ~p (state= ~p)~n", [_Req, State]),
	{noreply, State}.

% timeout, system msg
handle_info(Req={udp,Sock,SrcIP,SrcPort,Packet},
			State=#context{controller=Ctrl,port=Port,rtpsock=Sock,rtpsrc=undef}) ->
	?DEBUG("urlrelay: RTP initiate latching ~p:~p~n", [SrcIP,SrcPort]),

	% decoding RTP packet
	<<Version:2, _X1:7, Pt:7, Seq:16, Stamp:32, Ssrc:32, _X2/binary>> = Packet,
	?DEBUG("vers= ~p, payload=~p, seq=~p, stamp=~p, ssrc=~p~n", [Version,Pt,Seq,Stamp,Ssrc]),

	% async: notifying conference initiator
	mod_videobridge:notify(Ctrl, {latching, Port, Ssrc}),

	State2 = State#context{rtpsrc={SrcIP,SrcPort}},
	handle_info(Req, State2);
handle_info({udp,Sock,SrcIP,SrcPort,Packet}, State=#context{rtpsock=Sock,rtpsrc={SrcIP,SrcPort},recipients=Rcps})  ->
	?DEBUG("udprelay: RTP packet received~n",[]),
%	gen_udp:send(RtpSock,?,?,Packet),
	[ udprelay:forward(R, {rtp, Packet}) || R <- Rcps ],

	{noreply, State};
% not matching sender
handle_info({udp,Sock,SrcIP,SrcPort,Packet}, State=#context{rtpsock=Sock})  ->
	?DEBUG("udprelay: received rogue RTP packet from ~p:~p~n~p~n",[SrcIP,SrcPort,Packet]),
	{noreply, State};

handle_info(Req={udp,Sock,SrcIP,SrcPort,Packet}, State=#context{rtcpsock=Sock,rtcpsrc=undef}) ->
	?DEBUG("urlrelay: RTCP initiate latching ~p:~p~n", [SrcIP,SrcPort]),
	handle_info(Req, State#context{rtcpsrc={SrcIP,SrcPort}});
handle_info({udp,Sock,SrcIP,SrcPort,Packet}, State=#context{rtcpsock=Sock,rtcpsrc={SrcIP,SrcPort},recipients=Rcps}) ->
	?DEBUG("udprelay: RTCP packet received~n",[]),
	[ udprelay:forward(R, {rtp, Packet}) || R <- Rcps ],
	{noreply, State};
handle_info({udp,Sock,SrcIP,SrcPort,Packet}, State=#context{rtcpsock=Sock}) ->
	?DEBUG("udprelay: received roghe RTCP packet from ~p:~p~n~p~n",[SrcIP,SrcPort,Packet]),
	{noreply, State};

handle_info(_Info, State) ->
	?DEBUG("udprelay: handle_info ~p (state= ~p)~n", [_Info, State]),
	{noreply, State}.
	
terminate(Reason, #context{rtpsock=Socket}) ->
	?DEBUG("udprelay: terminating (socket ~p)~n", [Socket]),
	gen_udp:close(Socket).

code_change(OldSvn, State, _) -> {ok, State}.
