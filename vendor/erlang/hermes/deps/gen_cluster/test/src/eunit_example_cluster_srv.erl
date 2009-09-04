-module(eunit_example_cluster_srv).
-include_lib("eunit/include/eunit.hrl").
-define(TRACE(X, M),  io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M])).

setup() ->
    ?TRACE("seed servers", self()),
    {ok, Node1Pid} = example_cluster_srv:start_named(node1, {seed, undefined}),
    {ok, Node2Pid} = example_cluster_srv:start_named(node2, {seed, Node1Pid}),
    {ok, Node3Pid} = example_cluster_srv:start_named(node3, {seed, Node1Pid}),
    [node1, node2, node3].

teardown(Servers) ->
    io:format(user, "teardown: ~p ~n", [Servers]),
    lists:map(fun(Pname) -> 
        Pid = whereis(Pname),
        io:format(user, "takedown: ~p ~p ~n", [Pname, Pid]),
        gen_cluster:cast(Pid, stop), 
        try unregister(Pname)
        catch _:_ -> ok
        end
     end, Servers),
    ok.

node_state_test_not() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         ?assert(true =:= true),
         {ok, Plist} = gen_cluster:plist(node1),
         ?assertEqual(3, length(Plist)),
         {ok, State1} = gen_cluster:call(node1, {state}),
         % ?assert(is_record(State1, state) =:= true),
         % ?assertEqual(testnode1, gen_cluster:call(testnode1, {registered_name})),
         {ok}
      end
  }.

node_join_test_not() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         ?assert(true =:= true),
         {ok}
      end
  }.

node_global_takeover_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         Node1Pid = whereis(node1),
         {ok, Name1} = gen_cluster:call(Node1Pid, {'$gen_cluster', globally_registered_name}),

         GlobalPid1 = global:whereis_name(Name1),
         ?assert(is_process_alive(GlobalPid1)),

         gen_cluster:cast(Node1Pid, stop),
         timer:sleep(500),

         GlobalPid2 = global:whereis_name(Name1),
         ?assert(GlobalPid1 =/= GlobalPid2),
         ?assert(is_process_alive(GlobalPid2)),

         {ok, Node4Pid} = example_cluster_srv:start_named(node4, {seed, GlobalPid2}),

         {ok}
      end
  }.

% node_leave_test_not() ->
%   {
%       setup, fun setup/0,
%       fun () ->
%          ?assert(true =:= true),
%          {ok}
%       end
%   }.

