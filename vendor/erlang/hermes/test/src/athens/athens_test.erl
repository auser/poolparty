-module (athens_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  ambassador:start_link(),
  test_helper:start_n_nodes(1).

teardown(_Servers) ->
  ambassador:stop([]),
  % test_helper:teardown_all_nodes(),
  ok.
  
all_test_() ->
  {setup, fun setup/0, fun teardown/1,
    {timeout, 300,
      fun() ->
        test_that_athens_srv_gets_called_()
      end
    }
  }.

test_that_athens_srv_gets_called_() ->
  % {ok, Mock} = gen_server_mock:new(),

  % gen_server_mock:expect_call(Mock, fun(one,  _From, _State)            -> ok end),
  % ok.
  ?assertEqual(0.0, athens:call_ambassador_election_on(cpu, "elect", [node1])),
  ok.