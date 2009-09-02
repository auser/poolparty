-module (nag_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
  nag_sup:start_link(Args).

stop(State) -> 
  nag_sup:stop(State),
  ok.
