-module (config_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

test_info_msg_test_() ->
  Config = config:read(),
  [
    ?_assertEqual({port, 8642}, hd(Config)),
    ?_assertEqual({ok, 8642}, config:get(port)),
    ?_assertEqual({error, not_found}, config:get(boxes)),
    ?_assertEqual({ok, 9999}, config:get(port, [{law, "and order"},{port, 9999}, {a, "apples"}]))
  ].