% Includes file for the PoolParty Messenger

% -define(debug, true).

-ifdef(debug).

-define (MASTER_LOCATION, erlang:list_to_atom(lists:append("master@", element(2, inet:gethostname()))) ).
-define (TRACE(X, M), io:format("TRACE ~p:~p ~p ~p~n" ,[?MODULE, ?LINE, X, M])).

-else.

-define (MASTER_LOCATION, master@master).
-define (TRACE(X, M), void).

-endif.

-define (MASTER_NODE_NAME, master).
-define (MASTER_SERVER, global:whereis_name(pm_master)).

-define(DICT, dict).
-record (node, 
					{load}).