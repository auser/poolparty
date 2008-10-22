% The client is a running process that will run on the master node
% and spawn requests to the pm_nodes and compile the responses
% for use within the poolparty network

-module (pm_master).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Client function definitions
-export ([get_load/1]).

% pm_master:get_load("0", "cpu").
get_load(Type) ->
	Nodes = get_live_nodes(),
	{Loads, _} = rpc:multicall(Nodes, pm_node, get_load_for_type, [Type]),
	{utils:convert_responses_to_int_list(Loads)}.

% Private methods
get_live_nodes() ->
	nodes().
