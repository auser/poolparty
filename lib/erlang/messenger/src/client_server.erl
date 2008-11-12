-module (client_server).
-include_lib("../include/defines.hrl").
-export([loop/1, connect_to_master/0]).
-export ([start/0]).

-define (RECONNECT_TIMEOUT, 10000).
-define (CAST_MESSAGES, [force_reconfig]).

start() -> 	
	utils:start_timer(client_timer, ?UPDATE_TIME, fun() -> client_server:connect_to_master() end),
	pm_client:start(?MODULE, 7050, {?MODULE, loop}).
	
master_server() -> global:whereis_name(pm_master).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->						
						% Args = [Item || K <- string:tokens(erlang:binary_to_list(Data), " "), Item <- erlang:list_to_atom(K)],
						[Meth|Args] = string:tokens(erlang:binary_to_list(Data), " "),
						case lists:member(erlang:list_to_atom(Meth), ?CAST_MESSAGES) of
							true ->
								?TRACE("Received cast message", [Meth]);
							_ ->
								Output = gen_server:call(master_server(), {erlang:list_to_atom(Meth), Args}),
								send_back_appropriate_response(Socket, Output),
								?TRACE("posted", [Output])
						end,
            loop(Socket);
        {error, closed} ->
            ok
    end.

% send_back_appropriate_response(Socket, Output) when is_float(Output) -> gen_tcp:send(Socket, erlang:float_to_list(Output));
% Figure out how to do this the best... damnit
send_back_appropriate_response(Socket, Output) -> 
	[H|_T] = Output,
	case erlang:is_atom(H) of
		true ->
			?TRACE("NewOut", [Output]),
			NewOut = pm_strings:string_join( [erlang:atom_to_list(K) || K <- Output], " ");
		_ ->
			ListOfFloats = pm_strings:string_join( [erlang:float_to_list(V) || V <- Output], " "),
			NewOut = [ListOfFloats]
	end,
	?TRACE("NewOut", [NewOut]),
	gen_tcp:send(Socket, NewOut).
	

connect_to_master() ->
	case net_adm:ping(?MASTER_LOCATION) of
		pong ->			
			global:sync(),
			ok;
		_ ->
			receive 
				stop -> void 
			after ?RECONNECT_TIMEOUT -> 
				connect_to_master()
			end
	end.