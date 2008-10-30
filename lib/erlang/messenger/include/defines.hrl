% Includes file for the PoolParty Messenger

-ifndef (MASTER).

-define (MASTER_LOCATION, master@master).
-define (MASTER_NODE_NAME, master).
-define (MASTER_SERVER, global:whereis_name(?MASTER_NODE_NAME)).

-endif.