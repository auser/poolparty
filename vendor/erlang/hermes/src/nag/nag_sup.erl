-module (nag_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_Args) ->
    {ok, {{one_for_one, 2, 10}, [
        {nag, {nag, start_link, []}, permanent, 2000, worker, [nag]}
    ]}}.
