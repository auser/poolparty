% Includes file for the PoolParty Messenger

-define(debug, true).

-ifdef(debug).

-define (MASTER_LOCATION, erlang:list_to_atom(lists:append("master@", element(2, inet:gethostname()))) ).
-define (TRACE(X, M), io:format("TRACE ~p:~p ~p ~p~n" ,[?MODULE, ?LINE, X, M])).
-define (UPDATE_TIME, 2000).

-else.

-define (MASTER_LOCATION, master@master).
-define (TRACE(X, M), void).
-define (UPDATE_TIME, 10000).

-endif.

-define (MASTER_NODE_NAME, master).
-define (MASTER_SERVER, global:whereis_name(pm_master)).

-define(NUM_LOADS_TO_STORE, 5).
-define(DICT, dict).

-record (node, {
				load
				}).