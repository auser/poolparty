-module (hermes_logger_test).
-include_lib("eunit/include/eunit.hrl").

-define(TRACE(X, M),  io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M])).

setup() ->
  % {ok, Pid} = hermes_logger:start_link([{tty, false}, {log_path, "logs/hermes.log"}]),
  % [Pid].
  ok.

teardown(_S) ->
  hermes_logger:stop([]),
  ok.

test_info_msg_test_() ->
  {
    setup, 
    fun setup/0, 
    fun teardown/1,
    [
      ?_assert(true)
    ]
  }.
