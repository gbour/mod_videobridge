
-module(videobridge_SUITE).
-compile(export_all).

%-include_lib("escalus.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, messages}].

groups() ->
    [{messages, [sequence], [capabilities]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).
	%TODO: wait for welcome message

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

capabilities(Config) ->
	escalus:story(Config, [1,1], fun(Alice, Bob) ->
		% TOO BAD: Alice jid is a binary string
		% so we have to extract domain from Config
		Domain = proplists:get_value(server,
			proplists:get_value(alice, 
				proplists:get_value(escalus_users, Config))),

		R = escalus:send_and_wait(Alice, escalus_stanza:disco_items(Domain)),
		io:format("R= ~p~n~p~n", [Domain, R]),

		escalus:assert(has_item, [<<"jitsi-videobridge.", Domain/binary>>], R)
	end).
