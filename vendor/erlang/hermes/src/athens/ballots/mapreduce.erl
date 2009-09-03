-module (mapreduce).
-include ("hermes.hrl").
-export ([submit/4, submit/5]).

submit(M, F, A, Value) ->
  ?TRACE("Submit", [M,F,A,Value]),
  Nodes = athens:nodes(),
  submit(M, F, A, Value, Nodes).
  
submit(M, F, A, Comparison, NodeList) ->
  S = self(),
  Acc = 0.0,
  Nodes = ensure_are_nodes(NodeList),
  Pid = spawn(fun() -> 
    reduce(S, M, F, A, Comparison, Acc, Nodes)
  end)
  ,receive
    {Pid, R} -> 
      R
  end.
  
reduce(From, M, F, A, ComparisonValue, Acc0, Nodes) ->
  process_flag(trap_exit, true),
  ReducePid = self(),
  
  lists:foreach(fun(Node) ->
      spawn_link(fun() -> run_fun(ReducePid, [M,F, [A]], Node) end)
    end, Nodes),
  
  TotalNumNodes = length(Nodes),
  Dict0 = dict:new(),
  % Collect the map reduce
  Dict1 = collect_reductions(TotalNumNodes, Dict0),
  % Reduce the values
  Acc = dict:fold(fun(_Node, V, FoldAcc) ->
      Value = strip_extraneous_values(V),
      O = case Value of
        ComparisonValue -> 1.0;
        _Else -> 0.0
      end,
      FoldAcc + O
    end, Acc0, Dict1),
  % Compute the average over the nodes
  TotalAverage = Acc / TotalNumNodes,
  From ! {self(), TotalAverage}.

%%====================================================================
%% REDUCTION
%%====================================================================
%%--------------------------------------------------------------------
%% Function: collect_reductions (C, T, Val, Acc) -> Avg
%% Description: Collect the reductions across the nodes
%%  If the total number of nodes respond, then return the average of
%%    the responses.
%%  If the response comes back that is equal to that of the value
%%    when the election was called, then let's add 1.0 to the acc
%%    value.
%%  If the response comes back differently than the value called 
%%    with the election, then don't add it to the list of responses
%%  If the known nodes send an exit response when calling the Fun
%%    then don't expect a response back from the node
%%--------------------------------------------------------------------
collect_reductions(0, Dict) -> Dict; 
collect_reductions(N, Dict) -> 
  receive
    {Node, Val} -> 
      case dict:is_key(Node, Dict) of 
        true -> 
          Dict1 = dict:append(Node, Val, Dict), 
          collect_reductions(N, Dict1); 
        false -> 
          Dict1 = dict:store(Node, Val, Dict), 
          collect_reductions(N, Dict1) 
      end; 
    {'EXIT', _, _Why} ->
      collect_reductions(N-1, Dict) 
  end.

run_fun(From, MFA, Node) ->
  [M,F,A] = MFA,
  Output = gen_server:call(Node, {ballot, M, F, A}),
  % {ok, Output} = rpc:call(Node,M,F,A),
  From ! {self(), Output}.
  
% Avg = mon_server:get_latest_average_for(Monitor)
% 
% Out = ambassador:ask(Fun, Args)

%%====================================================================
%% PRIVATE
%%====================================================================
ensure_are_nodes(List) ->
  [ ensure_is_node(Node) || Node <- List ].

% ensure_is_node(Node) when is_pid(Node) -> erlang:node(Node);
ensure_is_node(Node) when is_atom(Node) -> ensure_is_node(whereis(Node));
ensure_is_node(Node) -> Node.

% strip
strip_extraneous_values(Resp) ->
  Response = case Resp of
    {ok, [V]} -> V;
    F -> F
  end,
  % ?INFO("Response: ~p (from ~p)~n", [Response, Resp]),
  case string:tokens(Response, ":") of
    ["vote_for", Action]  -> Action;
    [Action]              -> Action;
    Else                  -> Else
  end.