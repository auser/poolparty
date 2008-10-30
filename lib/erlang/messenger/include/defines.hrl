% Includes file for the PoolParty Messenger

-ifndef (MASTER).

-define (MASTER, master).
-define (MASTER_SERVER, global:whereis_name(pm_master)).
-define (MASTER_NODE_NAME, pp@master).

-endif.