-module (utils_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

format_ip_test_() ->
  [
    ?_assertEqual("1.2.3.4", utils:format_ip({1,2,3,4})),
    ?_assertEqual("99.2.3.2", utils:format_ip([99,2,3,2]))
  ].

% get_rrd_location_test_() ->
%   [
%     ?_assertEqual("hi/hello.rrd", lists:flatten([utils:get_rrd_location("hello", "hi")]))
%   ].
  
delete_test_() ->
  [
    ?_assertEqual([{port,"90"}], utils:delete(a, [{port, "90"}, {a, "danger"}])),
    ?_assertEqual([{port, "90"}, {a, "danger"}], utils:delete(box, [{port, "90"}, {a, "danger"}]))
  ].
  
append_test_() ->
  [
    ?_assertEqual([{a, "a"},{b, "b"}], utils:append([{a, "a"}], [{b, "b"}]))
  ].
  
turn_binary_test_() ->
  [
    ?_assertEqual(<<"a">>, utils:turn_binary(a)),
    ?_assertEqual(<<"a">>, utils:turn_binary(<<"a">>)),
    ?_assertEqual(<<"true">>, utils:turn_binary(true)),
    ?_assertEqual(<<1,2>>, utils:turn_binary([1, 2])),
    ?_assertEqual(term_to_binary({a, "a"}), utils:turn_binary({a, "a"})),
    ?_assertEqual(<<"1234">>, utils:turn_binary(1234))
  ].
  
turn_to_atom_test_() ->
  [
    ?_assertEqual(a, utils:turn_to_atom("a")),
    ?_assertEqual('1234', utils:turn_to_atom(1234)),
    ?_assertEqual(a, utils:turn_to_atom(a))
  ].
  
turn_to_list_test_() ->
  [
    ?_assertEqual("a", utils:turn_to_list(a)),
    ?_assertEqual("a", utils:turn_to_list(<<"a">>))
  ].
  
turn_to_float_test_() ->
  [    
    ?_assertEqual(0.3, utils:turn_to_float("0.3")),
    ?_assertEqual(0.4, utils:turn_to_float(0.4)),
    ?_assertEqual(0.0, utils:turn_to_float("nan")),
    ?_assertEqual([0.0, 0.0], utils:turn_to_float("nan nan")),
    ?_assertEqual([0.3, 0.2, 0.9], utils:turn_to_float("0.3 0.2 0.9"))
  ].