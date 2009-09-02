-module (mapreduce_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

-export ([test_athens_response_function/1]).

setup() ->
  test_helper:start_n_nodes(30).

teardown(_Servers) ->
  test_helper:teardown_all_nodes(),
  ok.
  
all_test_() ->
  {setup, fun setup/0, fun teardown/1,
    {timeout, 300,
      fun() ->
        NodeList = test_helper:test_nodes(),
        % O = athens_srv:submit_ballot(whereis(node1), ?MODULE, test_athens_response_function, [1]),
        % O = mapreduce:submit(?MODULE, F, 1, test_nodes()),
        ?assertEqual(1/2, athens_srv:call_election(?MODULE, test_athens_response_function, [2], 2, NodeList))
        ,?assertEqual(1/3, athens_srv:call_election(?MODULE, test_athens_response_function, [2], 2, [node1, node2, node3]))
        ,?assertEqual(1.0, athens_srv:call_election(?MODULE, test_athens_response_function, [2], 2, [node2, node4, node6]))
        ,?assertEqual(1/4, athens_srv:call_election(?MODULE, test_athens_response_function, [2], 2, [node1, node3, node5, node8]))
        ,?assertEqual(0.0, athens_srv:call_election(?MODULE, test_athens_response_function, [4], 2, [node1, node3, node5]))
        ,?assertEqual(1/30,athens_srv:call_election(?MODULE, test_athens_response_function, [30],30, NodeList))
      end
    }
  }.


test_athens_response_function([Num]) ->
  ProcInfo = erlang:process_info(self()),
  Regname = erlang:atom_to_list(proplists:get_value(registered_name, ProcInfo)),
  [Nodenum] = string:tokens(Regname, "node"),
  
  PortNum = erlang:list_to_integer(Nodenum),
  % ?TRACE("in test_athens_response_function", [ Num, Regname, Nodenum, PortNum, PortNum rem Num ]),
  
  case PortNum rem Num of
    0 -> Num;
    _ -> 0
  end.
    
%%====================================================================
%% PRIVATE
%%====================================================================
