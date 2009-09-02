%%%-------------------------------------------------------------------
%%% File    : athens.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Aug 19 18:50:53 PDT 2009
%%%-------------------------------------------------------------------

-module (athens).
-include ("hermes.hrl").

-export ([  call_election/2,
            call_ambassador_election/2,
            call_ambassador_election_on/3,
            nodes/0,
            ambassador_check/1
         ]).

nodes() ->  athens_srv:nodes().

% Call an election
call_ambassador_election(Name, VoteValue) ->            athens_srv:call_election(?MODULE, ambassador_check, Name, VoteValue).
call_ambassador_election_on(Name, VoteValue, Nodes) ->  athens_srv:call_election(?MODULE, ambassador_check, Name, VoteValue, Nodes).
  
call_election(MFA, Value) ->
  athens_srv:call_election(MFA, Value).

%%====================================================================
%% PRIVATE
%%====================================================================
ambassador_check(Mon) ->
  LatestAverage = mon_server:get_latest_average_for(Mon),
  ambassador:ask("run_monitor", [ erlang:atom_to_list(Mon), erlang:float_to_list(LatestAverage) ]).