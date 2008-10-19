% The client is a running process that will run on the master node
% and spawn requests to the pm_nodes and compile the responses
% for use within the poolparty network

-module (pm_master).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Client function definitions
-export ([get_load/2]).

% pm_master:get_load("0", "cpu").
get_load(Index, Type) ->
	Pid = get_pid_from_index(Index),
	gen_server:call(Pid, get_load_for_type, [Type]).

% Private methods
get_pid_from_index(Index) ->
	String = lists:append(["pm_node", Index]),
	io:format("Looking for ~p~n", [String]),
	whereis(str).

% Tests
basic_test_() ->
	?_assert(get_pid_from_index == 0).