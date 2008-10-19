% The client is a running process that will run on the master node
% and spawn requests to the pm_servers and compile the responses
% for use within the poolparty network

-module (pm_client).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Client function definitions
-export ([get_load/1]).

get_load(Type) ->
	Pid = whereis(pm_server),
	load_server:get_load_for_type(Pid, Type).

-ifdef(EUNIT).
	basic_test_() ->
		?_assert(1 + 1 == 2).    
-endif.