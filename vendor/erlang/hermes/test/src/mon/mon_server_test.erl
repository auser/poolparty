-module (mon_server_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  erlrrd_sup:start_link(),
  mon_server:start_link([]).

teardown(_Servers) ->
  erlrrd_sup:stop(),
  mon_server:stop(),
  ok.
  
all_test_() ->
  {setup, fun setup/0, fun teardown/1,
    {timeout, 300,
      fun() ->
        testing:create_fixture_rrds(["0.1", "0.3"]), % create some dummy data
        test_get_monitors(),
        test_is_known_monitor(),
        test_get_related_monitors(),
        test_list_all_monitor_files(),
        test_get_latest_average_for()
      end
    }
  }.
  
%%====================================================================
%% FIXTURES 
%% test/fixtures/supermonitors/mons
%%====================================================================

test_get_monitors() ->
  ExpectedMonitors = [cpu, memory, disk],
  M = mon_server:list_monitors(),
  ?assertEqual(ExpectedMonitors, proplists:get_keys(M)).
  
test_get_related_monitors() ->
  ?assertEqual(['cpu-free', 'cpu-idle', 'cpu-used'], mon_server:list_related_monitors(cpu))
  ,?assertEqual([], mon_server:list_related_monitors(nothing_here))
  ,?assertEqual(['memory-free', 'memory-idle', 'memory-used'], mon_server:list_related_monitors(memory))
  ,?assertEqual(['cpu-idle'], mon_server:list_related_monitors(cpu, idle))
  .
  
test_is_known_monitor() ->
  ?assert(mon_server:is_known_monitor('memory-idle'))
  ,?assert(mon_server:is_known_monitor('memory-idle'))
  ,?assertNot(mon_server:is_known_monitor('nothing-idle'))
  .
  
test_list_all_monitor_files() ->
  ?assertEqual(['cpu-free', 'cpu-idle', 'cpu-used', 
                'memory-free', 'memory-idle', 'memory-used',
                'disk-free', 'disk-idle', 'disk-used'], mon_server:list_all_monitor_files())
  .
  
test_get_latest_average_for() ->
  testing:create_fixture_rrds(["0.1", "0.3"]),
  % ?assertEqual(0.2, mon_server:get_latest_average_for('cpu-idle'))
  ok
  .