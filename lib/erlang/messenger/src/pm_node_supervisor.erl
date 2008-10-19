% Supervisor for the poolparty_messenger server
% 
% Ari Lerner
% CitrusByte

-module (pm_node_supervisor).
-behaviour (supervisor).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define (SERVER, ?MODULE).

-export ([start_link/0, init/1]).

start_link() ->
	io:format("Starting load_supervisor...~n"),
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 3, % 1000
	MaxTimeBetRestarts = 30, % 3600
	
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
  
	LoadServers = [
		{load_server, {load_server, start_link, []}, permanent, 5000, worker, [load_server, load_client]}
	],
							
	{ok, {SupFlags, LoadServers}}.
							