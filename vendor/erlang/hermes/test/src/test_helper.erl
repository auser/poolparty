-module (test_helper).

-compile (export_all).

start_n_nodes(N) ->
  OrigPid = start_node(1, undefined),
  O = case N > 1 of
    true ->
      lists:map(fun(Name) -> 
        Pid = start_node(Name, OrigPid),
        {Name, Pid}
      end, lists:seq(2, N) );
    false ->
      []
  end,
  lists:flatten([{node1, OrigPid},O]).

start_node(Integer, Seed) ->  
  Name = construct_node_name(Integer),
  case athens_srv:start_named(Name, {seed, Seed}) of
    {ok, OP} -> OP;
    _ -> whereis(Name)
  end.

construct_node_name(Integer) ->
  erlang:list_to_atom(lists:flatten([
                                      ["node"], [erlang:integer_to_list(Integer)]%, ["@local"]%, [Hostname]
                                    ])).

teardown_all_nodes() -> lists:map(fun(Pid) -> gen_cluster:cast(Pid, stop) end, test_nodes()).

test_nodes() ->
  {ok, N} = gen_cluster:call(node1, {'$gen_cluster', plist}),
  N.