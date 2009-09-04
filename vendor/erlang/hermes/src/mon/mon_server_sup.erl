-module (mon_server_sup).
-behaviour(supervisor).

-export([start_link/1, init/1, stop/1]).

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
  {ok, {{one_for_one, 2, 10}, [
      {the_mon_server, {mon_server, start_link, [Args]}, permanent, 2000, worker, [mon_server]}
  ]}}.

stop(_Args) ->
  supervisor:terminate_child(?MODULE, the_mon_server).  