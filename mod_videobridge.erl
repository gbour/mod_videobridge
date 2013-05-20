
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
-export([disco_items/5, disco_identity/5, disco_features/5]).


-include("ejabberd.hrl").
-include("jlib.hrl").

-define(SUPERVISOR, ejabberd_sup).
-define(SUBDOMAIN, <<"jitsi-videobridge">>).
-define(NS_COLIBRI, <<"http://jitsi.org/protocol/colibri">>).
-record(state, {
		host
	}).

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
	?DEBUG("videobridge:init:: ~p, ~p~n", [Fqdn, Opts]),

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
	
	{ok, #state{host=Host}}.

fqdn(Host) -> <<?SUBDOMAIN/binary, ".", Host/binary>>.

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

% handle received messages
handle_info({route, From, To, Packet}, State) ->
	?DEBUG("videobridge: from ~p to ~p received ~p~n", [From, To, Packet]),

	Res = do_route(From, To, jlib:iq_query_info(Packet)),
	ejabberd_router:route(To, From, jlib:iq_to_xml(Res)),

	{noreply, State}.

% emulate disco 
do_route(From, To, IQ=#iq{type=get, xmlns=?NS_DISCO_INFO}) ->
	#iq{lang=Lang, sub_el = SubEl} = IQ,
	Host = To#jid.lserver,
	Node = xml:get_tag_attr_s(<<"node">>, SubEl),

	Res = ejabberd_hooks:run_fold(disco_local_features, Host,
		empty, [From, To, Node, Lang]),
	?DEBUG("disco: host= ~p, node= ~p, res= ~p~n", [Host,Node,Res]),

	mod_disco:process_local_iq_info(From, To, IQ).

%
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, #state{host=Host}) ->
	Fqdn = fqdn(Host),

	ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, disco_items, 42),
	ejabberd_hooks:delete(disco_local_features, Fqdn, ?MODULE, disco_features, 42),
	ejabberd_hooks:delete(disco_local_identity, Fqdn, ?MODULE, disco_identity, 42),
	ejabberd_router:unregister_route(Fqdn).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
