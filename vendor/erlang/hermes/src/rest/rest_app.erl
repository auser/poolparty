-module (rest_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
  rest_server_sup:start_link(Args).

stop(State) -> 
  rest_server:stop(State),
  ok.
