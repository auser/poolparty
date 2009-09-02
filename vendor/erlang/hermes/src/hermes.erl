-module (hermes).
-include ("hermes.hrl").
-behaviour (application).

-export([start/0, start/2, stop/1, stop/0, start_phase/3]).

start() ->
    start(normal, []).

start(Type, Args) ->  
    hermes_sup:start(Type, Args).

start_phase(go, normal, _Args) ->
    ok.

stop() -> stop([]).
  
stop(State) -> 
  hermes_sup:stop(State),
  ok.