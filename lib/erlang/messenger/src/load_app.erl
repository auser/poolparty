-module (load_app).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour (application).

-export ([start/0, start/2, stop/1]).

start() ->
	io:format("application:start/1 starting...~n"),
	application:start(?MODULE).
	
start(_Type, _) -> 
	io:format("Starting load_app~n"),
	case load_supervisor:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

stop(State) ->
	io:format("Error: ~p~n", State),
	exit(whereis(load_supervisor), shutdown).