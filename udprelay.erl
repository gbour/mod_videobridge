
-module(udprelay).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([addpeer/2, delpeer/2, forward/2, stats/1, shutdown/1]).

-include("ejabberd.hrl").

-record(stat, {
	count  = 0,
	len    = 0,
	gcount = 0,
	glen   = 0
}).

-record(stats, {
	rtp_recv  = #stat{},
	rtp_send  = #stat{},
	rtcp_recv = #stat{},
	rtcp_send = #stat{}
}).

-record(context, {
	controller,
	confid,
	chanid,
	baseport,
	rtpsock,
	rtcpsock,
	rtpsrc=undef,
	rtcpsrc=undef,
	recipients=[],

	expiry=6000,
	extimer,
	evtclb,

	% stats: RTP, RTCP: total count, total len, step count, step len
	stats=#stats{}
}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init(Opts) ->
	Port = proplists:get_value(port,Opts),
	Ctrl = proplists:get_value(controller,Opts),
	ConfId = proplists:get_value(confid, Opts),
	ChanId = proplists:get_value(chanid, Opts),
	EventClb = proplists:get_value(event_callback, Opts), % {Mod, Fun}
	?DEBUG("udprelay:init :: opening udp socket on ~p port. Controller= ~p, conf=~p~n", [Port, Ctrl, ConfId]),

	% open RTP port
	{Ret, State} = case gen_udp:open(Port, [binary,{active,true},{reuseaddr,true}]) of
		{ok, RtpSock}   -> 
			case gen_udp:open(Port+1, [binary,{active,true},{reuseaddr,true}]) of
				{ok, RtcpSock} ->
					{ok, #context{controller=Ctrl, confid=ConfId, chanid=ChanId, 
							baseport=Port, evtclb=EventClb,
							rtpsock=RtpSock, rtcpsock=RtcpSock}
					};
				{error, Reason} ->
					gen_udp:close(RtpSock),
					{stop, Reason}
			end;
		{error, Reason} -> {stop, Reason}
	end,

	Timer = erlang:start_timer(State#context.expiry, self(), expiry),
	?DEBUG("udprelay: init(port= ~p, state= ~p)~n", [Port, State#context{extimer=Timer}]),
	{Ret, State}.

addpeer(Self, Pid) ->
	gen_server:call(Self, {addpeer,Pid}).
delpeer(Pid, Peer) ->
	gen_server:call(Pid, {delpeer, Peer}).

forward(Pid, Packet) ->
	gen_server:cast(Pid, {forward, Packet}).

stats(Pid) ->
	gen_server:call(Pid, stats).

shutdown(Pid) ->
	gen_server:call(Pid, shutdown).
%rtpport(Self) ->
	
	

% sync request
handle_call(shutdown, _, State) ->
	{stop, normal, ok, State};
handle_call({addpeer, Pid}, _, State=#context{recipients=R}) ->
	?DEBUG("udprelay: add recipient ~p~n",[Pid]),
	{reply, ok, State#context{recipients=[Pid|R]}};
handle_call({delpeer, Pid}, _, State=#context{recipients=R}) ->
	?DEBUG("udprelay: del recipient ~p~n", [Pid]),
	{reply, ok, State#context{recipients=lists:delete(Pid, R)}};
handle_call(stats, _, State) ->
	#context{chanid=ChanId, baseport=Port, rtpsrc=RtpSrc, rtcpsrc=RtcpSrc, stats=Stats} = State,

	#stats{rtp_recv=RtpRecv, rtp_send=RtpSend, rtcp_recv=RtcpRecv, rtcp_send=RtcpSend} = Stats,

	Clear = fun({stat, Cnt,Len,GCnt,GLen}) -> {stat,0,0,GCnt+Cnt,GLen+Len} end,
	Stats2 = #stats{
		rtp_recv =Clear(RtpRecv),
		rtp_send =Clear(RtpSend),
		rtcp_recv=Clear(RtcpRecv),
		rtcp_send=Clear(RtcpSend)
	},
	
	{reply, [
		{chanid  , ChanId},
		{baseport, Port},
		{rtpsrc  , RtpSrc},
		{rtcpsrc , RtcpSrc},
		{stats   , Stats}], State#context{stats=Stats2}};

handle_call(_Req, _From, State) ->
	?DEBUG("udprelay: handle_call ~p~n", [_Req]),
	{noreply, State}.

% async request
% forward RTP/RTCP packet to peers
forward(Sock, {Ip,Port}, Packet, {stat,Cnt,Len,GCnt,GLen}) ->
	case gen_udp:send(Sock, Ip, Port, Packet) of
		{error,Reason} ->
			?DEBUG("udprelay:forward= fail (~p)~n", [Reason]);
		_ -> ok
	end,

	{stat,Cnt+1,Len+byte_size(Packet),GCnt,GLen}.
handle_cast({forward, {rtp, Packet}}, State=#context{rtpsock=Sock,rtpsrc={Ip,Port},stats=Stats}) ->
	%?DEBUG("udprelay: relay to ~p:~p~n", [Ip, Port]),
	RtpStats = forward(Sock, {Ip,Port}, Packet, Stats#stats.rtp_send),
	Stats2 = Stats#stats{rtp_send=RtpStats},

	{noreply, State#context{stats=Stats2}};
handle_cast({forward, {rtcp, Packet}}, State=#context{rtcpsock=Sock,rtcpsrc={Ip,Port},stats=Stats}) ->
	%?DEBUG("udprelay: relay to ~p:~p~n", [Ip, Port]),
	RtcpStats = forward(Sock, {Ip,Port}, Packet, Stats#stats.rtcp_send),
	Stats2    = Stats#stats{rtcp_send=RtcpStats},

	{noreply, State#context{stats=Stats2}};

handle_cast(_Req, State) ->
	?DEBUG("udprelay: handle_cast ~p (state= ~p)~n", [_Req, State]),
	{noreply, State}.

% timeout, system msg
handle_info({timeout,_,expiry}, State=#context{controller=Ctrl,baseport=RtpPort,evtclb={Mod,Fun}}) ->
	?DEBUG("udprelay: socket #~p timeout. closing~n", [RtpPort]),
	%TODO: send RTCP BYE packet
	Mod:Fun(Ctrl, {rtpexpiry, RtpPort}),

	{stop, normal, State};

handle_info(Req={udp,Sock,SrcIP,SrcPort,Packet},
			State=#context{controller=Ctrl,baseport=Port,rtpsock=Sock,rtpsrc=undef}) ->
	?DEBUG("urlrelay: RTP initiate latching ~p:~p~n", [SrcIP,SrcPort]),

	% decoding RTP packet
	State3 = case Packet of
		<<Version:2, _X1:7, Pt:7, Seq:16, Stamp:32, Ssrc:32, _X2/binary>> ->
			?DEBUG("vers= ~p, payload=~p, seq=~p, stamp=~p, ssrc=~p~n", [Version,Pt,Seq,Stamp,Ssrc]),

			% async: notifying conference initiator
			mod_videobridge:notify(Ctrl, {latching, Port, Ssrc}),
			{noreply, State2} = handle_info(Req, State#context{rtpsrc={SrcIP,SrcPort}}),
			State2;

		% empty packet - set latching, but don't forward
		<<>> ->
			?DEBUG("udprelay: receive empty packet~n",[]),
			State#context{rtpsrc={SrcIP,SrcPort}};

		% any other value
		_    ->
			?DEBUG("udprelay: invalid RTP packet. IGNORING~n", []),
			State
	end,

	{noreply, State3};
handle_info({udp,Sock,SrcIP,SrcPort,Packet},
            State=#context{rtpsock=Sock,rtpsrc={SrcIP,SrcPort},recipients=Rcps,expiry=Expiry,extimer=Timer,
                           stats=Stats})  ->
	erlang:cancel_timer(Timer),
	?DEBUG("udprelay: RTP packet received~n",[]),
%	gen_udp:send(RtpSock,?,?,Packet),
	[ udprelay:forward(R, {rtp, Packet}) || R <- Rcps ],

	% counting received packets and summing pkt len
	{stat, Cnt,Len,GCnt,GLen} = Stats#stats.rtp_recv,
	Stats2 = Stats#stats{rtp_recv={stat,Cnt+1,Len+byte_size(Packet),GCnt,GLen}},

	Timer2 = erlang:start_timer(Expiry, self(), expiry),
	{noreply, State#context{stats=Stats2,extimer=Timer2}};
% not matching sender
handle_info({udp,Sock,SrcIP,SrcPort,Packet}, State=#context{rtpsock=Sock})  ->
	?DEBUG("udprelay: received rogue RTP packet from ~p:~p~n~p~n",[SrcIP,SrcPort,Packet]),
	{noreply, State};

handle_info(Req={udp,Sock,SrcIP,SrcPort,Packet}, State=#context{rtcpsock=Sock,rtcpsrc=undef}) ->
	?DEBUG("urlrelay: RTCP initiate latching ~p:~p~n", [SrcIP,SrcPort]),
	handle_info(Req, State#context{rtcpsrc={SrcIP,SrcPort}});
handle_info({udp,Sock,SrcIP,SrcPort,Packet}, 
            State=#context{rtcpsock=Sock,rtcpsrc={SrcIP,SrcPort},recipients=Rcps,stats=Stats,
                           expiry=Expiry,extimer=Timer}) ->
	erlang:cancel_timer(Timer),
	?DEBUG("udprelay: RTCP packet received~n",[]),
	[ udprelay:forward(R, {rtcp, Packet}) || R <- Rcps ],

	% counting received packets and summing pkt len
	{stat, Cnt,Len,GCnt,GLen} = Stats#stats.rtcp_recv,
	Stats2 = Stats#stats{rtcp_recv={stat,Cnt+1,Len+byte_size(Packet),GCnt,GLen}},

	Timer2 = erlang:start_timer(Expiry, self(), expiry),
	{noreply, State#context{stats=Stats2,extimer=Timer2}};
handle_info({udp,Sock,SrcIP,SrcPort,Packet}, State=#context{rtcpsock=Sock}) ->
	?DEBUG("udprelay: received rogue RTCP packet from ~p:~p~n~p~n",[SrcIP,SrcPort,Packet]),
	{noreply, State};

handle_info(_Info, State) ->
	?DEBUG("udprelay: handle_info ~p (state= ~p)~n", [_Info, State]),
	{noreply, State}.
	
terminate(Reason, #context{rtpsock=Sock,rtcpsock=Sock2}) ->
	?DEBUG("udprelay: terminating (socket ~p)~n", [Sock]),
	gen_udp:close(Sock),
	gen_udp:close(Sock2),
	ok.

code_change(OldSvn, State, _) -> {ok, State}.
