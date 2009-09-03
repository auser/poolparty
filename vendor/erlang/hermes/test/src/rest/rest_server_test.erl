-module (rest_server_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  application:start(inets),
  http:set_options([{verbose, true}]),
  case rest_server:start_link([{port, 9999}]) of
    {ok, Pid}   -> Pid;
    {error, _}  -> ok
  end.

teardown(_S) ->
  rest_server:stop([]),
  error_logger:tty(false), % turn off the logging output
  application:stop(inets),
  ok.

mochiweb_start_test_() ->
  {inorder,
    {setup, 
      fun setup/0,
      fun teardown/1,
      fun() ->
        check_for_root()
        ,monitors_route()
      end
    }
  }.
  
%%====================================================================
%% TESTS
%%====================================================================
check_for_root() ->
  Resp = get_request(""),
  ?assertEqual(200, proplists:get_value(code, Resp)),
  HeaderType = proplists:get_value("content-type", proplists:get_value(headers, Resp)),
  ?assertEqual("text/html", HeaderType),
  ?assertEqual("OK", proplists:get_value(msg, Resp)).


monitors_route() ->
  % Resp = get_request("/monitors"),
  % % ?TRACE("code:~p~n", [Resp, proplists:get_value(code, Resp)]),
  % ?assertEqual(200, proplists:get_value(code, Resp)),
  % HeaderType = proplists:get_value("content-type", proplists:get_value(headers, Resp)),
  % ?assertEqual("text/html", HeaderType),
  % ?assertEqual("OK", proplists:get_value(msg, Resp)).
  ok.

get_request(Path) ->
  Url = lists:flatten(lists:append([["http://localhost:9999"], [Path]])),
  O = http:request(get, {Url, [{"User-Agent", "Mozilla"}]}, [], []),
  case O of
    {ok, {{_HttpVer, Code, Msg}, Headers, Body}} ->
      [ 
        {msg, Msg},
        {code, Code}, 
        {headers, Headers}, 
        {body, Body}
      ];
    Else -> Else
  end.