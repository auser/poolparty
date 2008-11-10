-module (utils).
-compile(export_all).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Turn a list from
% [{"0.66"}, {"0.32"}, []] -> [0.66, 0.32]
convert_responses_to_int_list(L) ->
	Sum = lists:foldr( fun(Int, Sum) -> Int + Sum end, 0, [erlang:list_to_float(F) || {F} <- L] ),
	average_for_list(Sum, L).

% Start a timer to fire off Fun after Time number of milliseconds
start_timer(Time, Fun) -> 
	register(?MODULE, spawn(fun() -> tick_timer(Time, Fun) end)). 

stop_timer() -> ?MODULE ! stop. 

tick_timer(Time, Fun) -> 
	receive 
		stop -> 
			void 
	after Time -> 
		Fun(), 
		tick_timer(Time, Fun) 
end. 

average_of_list(L) ->
	Sum = lists:foldr( fun(Int, Sum) -> erlang:list_to_float(Int) + Sum end, 0, [F || F <- L] ),
	average_for_list(Sum, L).

% Get the average of the list
average_for_list(Num, L) ->
	case length(L) of
		0 ->
			-1;
		_ ->
			Num / length(L)
	end.



% Provisioning utils
distribute_modules_to(Modules, Nodes) ->
	% transfer the modules to all the nodes
	io:format("Sending ~p to ~p~n", [Modules, Nodes]),
	lists:foreach(fun(Node) ->
			transfer_modules(Node, Modules)
	end, Nodes).

% Transfer modules of code to this node
transfer_modules(Node, Modules) ->
	[transfer_module(Node, M) || M <- Modules].

% Transfer one module to the Node
transfer_module(Node, Module) ->
	{_Module, Binary, Filename} = code:get_object_code(Module),
	rpc:call(Node, code, load_binary, [Module, Filename, Binary]).