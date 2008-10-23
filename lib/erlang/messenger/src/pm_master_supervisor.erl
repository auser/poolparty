% This supervisor is responsible for monitoring the 
% client service

-module (pm_master_supervisor).
-behaviour(supervisor).

-export([start/0, start_in_shell_for_testing/0, start_link/1, init/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

start() ->
	spawn(fun() ->
			supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
		end).

start_in_shell_for_testing() ->
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
	unlink(Pid).

start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).	

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 3, % 1000
	MaxTimeBetRestarts = 30, % 3600

	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

	LoadServers = [
		{pm_master1, 
			{pm_master, start_link, []}, 
			permanent, 5000, worker, 
			[pm_master]
		}
	],

	{ok, {SupFlags, LoadServers}}.
