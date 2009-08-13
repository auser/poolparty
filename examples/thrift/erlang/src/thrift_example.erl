%%====================================================================
%% thrift_example 
%%  TODO: COMPLETE
%% RUN:
%%  make deps
%%  make 
%%  make run
%%====================================================================

-module (thrift_example).
-include ("poolparty_types.hrl").
-include ("commandInterface_thrift.hrl").
-include ("poolparty_constants.hrl").

-compile (export_all).

run() ->
  {ok, P} = thrift_client:start_link("localhost", 11223, commandInterface_thrift),
  O1 = cloud_query(P, "app", "name"),
  io:format("Name: ~p~n", [O1]),
  O2 = cloud_query(P, "app", "maximum_instances"),
  io:format("maximum_instances: ~p~n", [O2]),
  O3 = cloud_query(P, "app", "take_args", ["a", "b"]),
  io:format("cloud_provider: ~p~n", [O3]).

cloud_query(P, Name, Meth) ->
  cloud_query(P, Name, Meth, "").
  
cloud_query(P, Name, Meth, Args) ->
  Query = #cloudQuery{name=Name},
  io:format("Query: ~p, ~p, ~p~n", [Query, Meth, Args]),
  thrift_client:call(P, run_command, [Query, Meth, Args]).