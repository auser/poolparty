% Handles generic events
-module (pm_event_handler).
-export ([make/0, add_handler/1, event/1]).

% Make a new event handler
make() ->
	register(?MODULE, spawn(fun() -> handle_events(fun no_op/1) end )).

add_handler(Fun) ->
	whereis(?MODULE) ! {add, Fun}.

event(X) -> whereis(?MODULE) ! {event, X}.

handle_events(Fun) ->
	receive
		{event, Any} ->
			(catch Fun(Any)),
			handle_events(Fun)
	end.

no_op(_) -> void.