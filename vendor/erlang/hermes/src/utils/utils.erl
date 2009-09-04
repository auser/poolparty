-module (utils).
-include ("hermes.hrl").
-compile (export_all).
-define(JSON_ENCODE(V), mochijson2:encode(V)).

%%--------------------------------------------------------------------
%% Function: get_rrd_location (Module) -> {ok, Filename}
%% Description: Get the location of an rrd file
%%--------------------------------------------------------------------
get_rrd_location() ->
  case os:getenv("HERMES_RRD_DIRECTORY") of
      false -> ?RRD_DIRECTORY;
      Dir -> Dir
  end.


is_process_alive(Pid) when is_pid(Pid) ->
	rpc:call(node(Pid), erlang, is_process_alive, [Pid]);
is_process_alive(_Pid) -> false.


% utils:delete(a, [{port, "90"}, {a, "danger"}]). => [{port,"90"}]
% utils:delete(a, [{port, "90"}, {ab, "danger"}]). => [{port,"90"},{ab,"danger"}]
delete(Key, Config) ->
	[ T || T <- Config, element(1, T) =/= Key].
	
% utils:append([{port, 90}], [{port, 12345}, {name, "converse"}]).
% [{port,12345},{name,"converse"},{friends,"whisper"}]
append([H|T], L) -> 
  {Key, _Value} = H,
  NewL = delete(Key, L),
  [H|append(T, NewL)];
append([], L) -> L.

turn_binary(Arg) when is_atom(Arg) -> erlang:list_to_binary(erlang:atom_to_list(Arg));
turn_binary(Arg) when is_binary(Arg) -> Arg;
turn_binary(Arg) when is_boolean(Arg) -> erlang:list_to_binary(erlang:atom_to_list(Arg));
turn_binary(Arg) when is_tuple(Arg) -> erlang:term_to_binary(Arg);
turn_binary(Arg) when is_integer(Arg) -> erlang:list_to_binary(erlang:integer_to_list(Arg));
turn_binary(Arg) when is_float(Arg) -> erlang:list_to_binary(erlang:float_to_list(Arg));
turn_binary(Arg) -> 
  case catch erlang:list_to_binary(Arg) of
    {'EXIT',{badarg, _Reason}} -> turn_list_to_binary(Arg, []);
    F -> F
  end.

turn_list_to_binary([], Acc) -> lists:reverse(Acc);
turn_list_to_binary([H|T], Acc) -> turn_list_to_binary(T, [turn_binary(H)|Acc]).

turn_to_atom(Arg) when is_atom(Arg) -> Arg;
turn_to_atom(Arg) when is_integer(Arg) -> erlang:list_to_atom(erlang:integer_to_list(Arg));
turn_to_atom(Arg) when is_list(Arg) -> erlang:list_to_atom(Arg).

turn_to_list(Arg) when is_list(Arg) -> [ turn_to_list(A) || A <- Arg ];
turn_to_list(Bin) when is_binary(Bin) -> erlang:binary_to_list(Bin);
turn_to_list(Arg) -> erlang:atom_to_list(Arg).

turn_to_float("nan") -> 0.0;
turn_to_float(Arg) when is_list(Arg) -> 
  case catch erlang:list_to_float(Arg) of
    {'EXIT',{badarg, Reason}} ->
      case regexp:match(Arg, " ") of
        {match, _, _} ->           
          {ok, Floats} = regexp:split(Arg, " "),
          turn_to_float_from_list(Floats, []);
        _ -> ?TRACE("Uh oh. Error with float", [Arg, Reason])
      end;
    F -> F
  end;
turn_to_float(Arg) -> Arg.

% Turn list
turn_to_float_from_list([], Acc)    -> lists:reverse(Acc);
turn_to_float_from_list([H|T], Acc) -> turn_to_float_from_list(T, [turn_to_float(H)|Acc]).

% Gross
format_ip({A,B,C,D}) -> integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ integer_to_list(C) ++ "." ++ integer_to_list(D);
format_ip([A,B,C,D]) -> integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ integer_to_list(C) ++ "." ++ integer_to_list(D).

jsonify(Body) when is_atom(Body) ->
  [ ?JSON_ENCODE({
        struct, [
          Body
        ]
    })
  ];

jsonify(Body) ->
  [ ?JSON_ENCODE({ 
      Body
    })
  ].


change_to_float("nan")  -> 0;
change_to_float(Int)      -> erlang:float_to_list(Int).


%%====================================================================
%% LISTS
%%====================================================================
list_delete(Key, Config) ->
	[ T || T <- Config, element(1, T) =/= Key].
 
list_append(Config, Other) ->
	DFun = fun(Key, Value, Array) -> lists:append(delete(Key, Array), [{Key, Value}]) end,
	[NewConfig] = [ DFun(Key, Value, Config) || {Key, Value} <- Other ],
	NewConfig.