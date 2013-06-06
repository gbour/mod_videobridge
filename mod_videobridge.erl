
-module(mod_videobridge).
-author('guillaume@bour.cc').

-behaviour(gen_server).
-behaviour(gen_mod).

%% Exports
%%  - API
-export([start_link/2]).
%%  - gen_mod callbacks
-export([start/2, stop/1]).
%%  - gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).
%%  - Hook callbacks
-export([disco_items/5, disco_identity/5, disco_features/5, notify/2, stats/0]).
% DEBUG
-export([allocate_port/2, uuid/1, uuid/0]).


-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").

-define(SUPERVISOR, ejabberd_sup).
-define(SUBDOMAIN, <<"jitsi-videobridge">>).
-define(NS_COLIBRI, <<"http://jitsi.org/protocol/colibri">>).
-record(state, {
  host        = <<"">>,
  public_ip   = <<"127.0.0.1">>,
  rtp_range   = {},
  alloc_ports = [],
  udprelays   = dict:new()
}).

-record(channel, {
    id,
	confid,
	type,
	host,
	rtpport,
	rtcpport
  }).
%-record(conference, {
%    id
%  }).


start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    PingSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		transient, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, PingSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),

    supervisor:terminate_child(?SUPERVISOR, Proc),
    supervisor:delete_child(?SUPERVISOR, Proc).

% initiate module
% - register as a service module
% - return default context
init([Host, Opts]) ->
	Fqdn = fqdn(Host),
	Ip   = bin(proplists:get_value(public_ip, Opts)),
	{MinPort, MaxPort} = proplists:get_value(rtp_range, Opts),

	?DEBUG("videobridge:init:: ~p, ~p, ~p~n", [Fqdn, Ip, {MinPort,MaxPort}]),

	%
	random:seed(),
	ets:new(videobridge_confs, [set,named_table,public]),
	ets:new(videobridge_ports, [ordered_set,named_table,public]),

	% discovery hooks
	%mod_disco:register_feature(Host, ?NS_COLIBRI),
	%mod_disco:register_feature(Fqdn, ?NS_COLIBRI),
	ejabberd_hooks:add(disco_local_items, Host, ?MODULE, disco_items, 42),
	%ejabberd_hooks:add(disco_local_features, Host, ?MODULE, disco_features, 42),
	%ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, disco_identity, 42),
	ejabberd_hooks:add(disco_local_features, Fqdn, ?MODULE, disco_features, 42),
	ejabberd_hooks:add(disco_local_identity, Fqdn, ?MODULE, disco_identity, 42),
	%ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE, disco_identity, 42),

	ejabberd_router:register_route(Fqdn),

	% stats timer
	%erlang:start_timer(5000, self(), stats),
	ejabberd_commands:register_commands(commands()),
	{ok, #state{host=Host, public_ip=Ip, rtp_range={MinPort,MaxPort}}}.

fqdn(Host) -> <<?SUBDOMAIN/binary, ".", Host/binary>>.
bin(Item) when is_binary(Item) ->
	Item;
bin(Item) when is_list(Item) ->
	%io_lib:printable_list(Item) ->
	erlang:list_to_binary(Item);
bin(Item) when is_integer(Item) ->
	erlang:list_to_binary(erlang:integer_to_list(Item)).

% items discovery hook: add handler
%
disco_items(empty, From, To, Node, Lang) ->
	disco_items({result, []}, From, To, Node, Lang);
disco_items(Acc  , From, To, Node, Lang) -> 
	?DEBUG("disco items: ~p ~p ~p~n", [From, To, Node]),
	Fqdn = fqdn(To#jid.lserver),
	{result, Items} = Acc,

	Item = #xmlel{name= <<"item">>, attrs=[{"jid", Fqdn}]},
	{result, Items ++ [Item]}.

disco_identity(Acc, From, To, Node, Lang) ->
	Id = #xmlel{name= <<"identity">>, attrs=[
		{"category", <<"component">>},
		{"type"    , <<"conference">>},
		{"name"    , <<"EjabberdVideoBridge">>}
	]},
	[Id|Acc].

disco_features(empty, From, To, Node, Lang) ->
	{result, [?NS_DISCO_INFO, ?NS_COLIBRI]}.

commands() ->
	[#ejabberd_commands{name = videobridge_stats,
		tags = [stats],
		desc = 
			"Rtprelay statistics for mod_videobridge",
		module = ?MODULE, function = stats,
		args = [], result = {stats, string}}].


%
% Convert bytes to the highest smallest unit
% . 1024b -> 1kb
%
unit(X) when X >= 1048576 ->
	{X div 1048576, mbytes};
unit(X) when X >= 1024 ->
	{X div 1024, kbytes};
unit(X) ->
	{X, bytes}.

stats() ->
	iolist_to_binary(stats([], ets:first(videobridge_confs))).

stats({peer, {Ip,Port}}) ->
	[" connected to ",inet_parse:ntoa(Ip),":",bin(Port)];
stats({peer, _}) ->
	" -not connected-";

stats({stat,Cnt,Len,GCnt,GLen}) ->
	{XLen , XUnit}  = unit(Len),
	{XGLen, XGUnit} = unit(GLen+Len),

	[bin(Cnt)     , " pkts, ",bin(XLen), " ",atom_to_list(XUnit)," / ",
	 bin(GCnt+Cnt), " pkts, ",bin(XGLen)," ",atom_to_list(XGUnit)].

stats([], '$end_of_table') ->
	" -*- no active conferences -*-";
stats(Acc, '$end_of_table') ->
	Acc;
stats(Acc, Key={ConfId, ContentType}) ->
	Hdr = ["- conference (id=",ConfId,", type=", ContentType,"):"],

	[{_,_,Procs}] = ets:lookup(videobridge_confs, Key),
	Ctnt = lists:map(fun(P) ->
			Stats = udprelay:stats(P),
			Port  = proplists:get_value(baseport,Stats),

			RtpConn  = stats({peer, proplists:get_value(rtpsrc,Stats)}),
			RtcpConn = stats({peer, proplists:get_value(rtcpsrc,Stats)}),

			{stats,Rtprcv,Rtpsnd,Rtcprcv,Rtcpsnd} = proplists:get_value(stats,Stats),
				
			["  . channel #", proplists:get_value(chanid,Stats),":",$\n,
			 "    - rtp  port#",bin(Port),RtpConn,$\n,
			 "      . recv: ", stats(Rtprcv),$\n,
			 "      . send: ", stats(Rtpsnd),$\n,
			 "    - rtcp port#",bin(Port+1),RtcpConn,$\n
			]
		end, Procs
	),

	stats([Acc,$\n,Hdr,$\n,Ctnt], ets:next(videobridge_confs, Key)).


% handle received messages
handle_info({route, From, To, Packet}, State) ->
	?DEBUG("videobridge: from ~p to ~p received ~p~n", [From, To, Packet]),

	Res = do_route(From, To, jlib:iq_query_info(Packet), State),
	ejabberd_router:route(To, From, jlib:iq_to_xml(Res)),

	{noreply, State}.

% emulate disco 
do_route(From, To, IQ=#iq{type=get, xmlns=?NS_DISCO_INFO}, _) ->
	#iq{lang=Lang, sub_el = SubEl} = IQ,
	Host = To#jid.lserver,
	Node = xml:get_tag_attr_s(<<"node">>, SubEl),

	Res = ejabberd_hooks:run_fold(disco_local_features, Host,
		empty, [From, To, Node, Lang]),
	?DEBUG("disco: host= ~p, node= ~p, res= ~p~n", [Host,Node,Res]),

	mod_disco:process_local_iq_info(From, To, IQ);

do_route(From, To, #iq{id=Id, xmlns=?NS_COLIBRI, sub_el=El}, State) when
		El#xmlel.name =:= <<"conference">> ->
%		sub_el=#xmlel{name=<<"conference">>, attrs=Attrs, children=Elts}}) ->
	?DEBUG("videobridge: conference query~n",[]),
	%NOTE: if no id, ConfId == <<"">>
	{Op, ConfId}  = case xml:get_tag_attr_s(<<"id">>, El) of
		<<"">> -> {create, bin(uuid())};
		_Id    -> {append, _Id}
	end,
	%TODO: case no content
	%ontent = xml:get_subtag(El, <<"content">>),
	Content = El#xmlel.children,
	Res = init_content([], Content, From, ConfId, State),
	?DEBUG("content= ~p ~p~n", [Content, Res]),

	#iq{type=result, id=Id, sub_el=[
		#xmlel{name= <<"conference">>, 
			attrs=[{<<"xmlns">>, ?NS_COLIBRI}, {<<"id">>, ConfId}],
			children=Res
		}
	]};

do_route(_,_,IQ,_) ->
	?DEBUG("videobridge: unknown message ~p~n", [IQ]),
	#iq{type=error, sub_el=[jlib:make_error_element(<<"404">>, <<"unknown msg">>)]}.


init_content(Acc, [], _, _, _) ->
	Acc;
init_content(Acc, [Content=#xmlel{name= <<"content">>, children=Chans}|T], 
             From, ConfId, State) ->
	ContentType = xml:get_tag_attr_s(<<"name">>, Content),
	?DEBUG("content= ~p, from=~p~n", [ContentType, From]),
	{Procs2, Xmls} = do_channels([], [], Chans, {ConfId, ContentType}, State),
	
	% NOTE: if the conference still exists, we merge udprelay procs
	Procs1 = case ets:lookup(videobridge_confs, {ConfId,ContentType}) of
		[{_,_,P}] -> P;
		_         -> []
	end,
	Procs = lists:merge(Procs1,Procs2),
	ets:insert(videobridge_confs, {{ConfId, ContentType}, From, Procs}),

	% add udprelays each others as recipients
	% All "old" procs must relay to new ones
	lists:foreach(fun(P) ->
		[udprelay:addpeer(P,R) || R <- Procs2]
	end, Procs1),
	% All "new" procs must relay to both olds and news
	lists:foreach(fun(P) ->
		% all but P
		Recipients = lists:filter(fun(X) -> X =/= P end, Procs),
		[udprelay:addpeer(P, R) || R <- Recipients]
	end, Procs2),

	init_content([Content#xmlel{children=Xmls}|Acc], T, From, ConfId, State);
init_content(Acc, [_|T], From, ConfId, State) ->
	init_content(Acc, T, From, ConfId, State).

do_channels(Procs, Xmls, [],_,_) ->
	{Procs, Xmls};
do_channels(Procs, Xmls, [El=#xmlel{name= <<"channel">>}|T], Opts, State) ->
	{Action, ChanId} = case xml:get_tag_attr_s(<<"id">>, El) of
		<<"">> -> {allocate, undef};
		Id     ->
			case xml:get_tag_attr_s(<<"expire">>, El) of
				<<"0">> -> {free, Id};
				_       -> {update, Id}
			end
	end,
	{Proc, Xml} = channel(Action, ChanId, Opts, State),
	Xml2 = Xml#xmlel{children=El#xmlel.children},
	?DEBUG("channel proc= ~p~n", [Proc]),
	do_channels([Proc|Procs], [Xml2|Xmls], T, Opts, State);
do_channels(Procs, Xmls,[_|T],Opts,State) ->
	do_channels(Procs, Xmls, T, Opts,State).

channel(allocate, undef, {ConfId, ContentType}, #state{public_ip=PublicIp,rtp_range={Min,Max}}) ->
	ChanId=uuid(),
	%TODO: handle false value (no more available ports)
	RtpPort=allocate_port(Min,Max),
	?DEBUG("channel: alloc(~p), port=~p~n", [ChanId,RtpPort]),

	% channel udprelay proc is stored as a local gproc index 
	{ok, Proc} = udprelay:start_link([
		{port,RtpPort},
		{controller,self()},
		{confid,ConfId},
		{chanid,ChanId}
	]),
	ets:insert(videobridge_ports, {RtpPort, Proc, ChanId, ConfId, ContentType}),

	?DEBUG("videobridge: udprelay proc= ~p~n", [Proc]),
	?DEBUG("key= {~p,~p}~n", [ConfId,ContentType]),
	
	{
		Proc,
		#xmlel{name= <<"channel">>, attrs=[
			{<<"id">>      , bin(ChanId)},
			{<<"host">>    , PublicIp},
			{<<"rtpport">> , jlib:integer_to_binary(RtpPort)},
			{<<"rtcpport">>, jlib:integer_to_binary(RtpPort+1)},
			{<<"expire">>  , <<"60">>}
		]}
	};

channel(free    , ChanId, _, _)              ->
	?DEBUG("channel: free(~p)~n", [ChanId]),
	case ets:lookup(videobridge_channel, ChanId) of 
		[{ChanId, Chan}] -> 
			ets:delete(videobridge_channel, ChanId);
		_                -> 
			?DEBUG("channel(free): ERROR, ~p channel not found~n", [ChanId])
	end,
	{xmlcdata,<<"">>};

channel(update  , ChanId, _, _)              ->
	?DEBUG("channel: update(~p)~n", [ChanId]),
	update.



notify(Pid, {latching, RtpPort, Ssrc}) ->
	gen_server:cast(Pid, {latching, RtpPort, Ssrc}).


%
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({latching, RtpPort, Ssrc}, State=#state{host=Host,public_ip=Ip}) ->
	case ets:lookup(videobridge_ports, RtpPort) of
		[{RtpPort,Proc,ChanId,ConfId,ContentType}] ->
			[{_, To,_}] = ets:lookup(videobridge_confs, {ConfId,ContentType}),
			From = jlib:string_to_jid(fqdn(Host)),

			Msg = #iq{type=set, id= <<"videobridge-",(bin(uuid(5)))/binary>>, sub_el=[
				#xmlel{name= <<"conference">>, attrs=[
						{<<"xmlns">>, ?NS_COLIBRI},
						{<<"id">>   , bin(ConfId)}
					],
					children=[#xmlel{name= <<"content">>, attrs=[{<<"name">>, ContentType}], 
						children=[#xmlel{name= <<"channel">>, attrs=[
								{<<"id">>      , bin(ChanId)},
								{<<"host">>    , Ip},
								{<<"rtpport">> , jlib:integer_to_binary(RtpPort)},
								{<<"rtcpport">>, jlib:integer_to_binary(RtpPort+1)},
								{<<"expire">>  , <<"60">>}
							],
							children=[#xmlel{name= <<"ssrc">>, children=[{xmlcdata, bin(Ssrc)}]}]
				}]}]}]},

			ejabberd_router:route(From, To, jlib:iq_to_xml(Msg));
		_ -> fail
	end,
	
	{noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, #state{host=Host}) ->
	Fqdn = fqdn(Host),

	ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, disco_items, 42),
	ejabberd_hooks:delete(disco_local_features, Fqdn, ?MODULE, disco_features, 42),
	ejabberd_hooks:delete(disco_local_identity, Fqdn, ?MODULE, disco_identity, 42),
	ejabberd_router:unregister_route(Fqdn).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%
%%% UTILS
%%%

% allocate free RTP/RTCP ports
%   - RTCP port is always following RTP port (RTP is even, RTCP=RTP+1 is odd)
%   - successive allocated ports are random, not linear (help prevent attacks/mim)
% 
% The algorithm is 
%   - choose a random even port, 
%   - we get the close upper free port (looping from end to start of ports range)

% REMARKS:
%   - allocate_port/3 is internal
%
% @args
%   - Min: range start - included (int)
%   - Max: range end - included   (int)
%
% @return
%   - false if no available free port
%   - allocated port number (RTP) else
allocate_port(Base, Cur, Base, _) when Base =< Cur+2 ->
	false;
allocate_port(Base, Cur, '$end_of_table', {Min,Max}) when Max > Cur+2 ->
	Cur+2;
allocate_port(Base, Cur, '$end_of_table', {Min,Max}) ->
	allocate_port(Base,Min-2, ets:first(videobridge_ports), {Min,Max});
allocate_port(Base, Cur, Next, Bounds) when Next > Cur+2 ->
	Cur+2;
allocate_port(Base, Cur, Next, Bounds) ->
	allocate_port(Base,Next, ets:next(videobridge_ports, Next), Bounds).

allocate_port(Min, Max) ->
	Base = Min + (random:uniform((Max-Min) div 2) - 1) * 2,
	% we get the next free port following Base
	Port = case ets:lookup(videobridge_ports, Base) of
		% not found : port is free
		[] -> Base;
		_  -> allocate_port(Base,Base,ets:next(videobridge_ports, Base),{Min,Max})
	end,

	case Port of
		false -> false;
		Any   -> ets:insert(videobridge_ports, {Any,true})
	end,
	?DEBUG("freeport: ~p/~p~n", [Base, Port]),
	Port.

uuid(Len) ->
	lists:map(fun(X) ->
			B = random:uniform(16)-1,
			if B < 10 -> B+48;
        	   true   -> B+87
			end
		end, 
		lists:seq(1, Len)
	).
uuid() ->
	uuid(16).
