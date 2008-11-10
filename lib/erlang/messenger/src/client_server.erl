-module (client_server).
-include_lib("../include/defines.hrl").
-export([start/0, loop/1]).

% echo_server specific code
start() -> 
	?TRACE("MASTER_LOCATION", [?MASTER_LOCATION]),
	pong = net_adm:ping(?MASTER_LOCATION),
	% global:sync(),
	pm_client:start(?MODULE, 7049, {?MODULE, loop}).
master_server() -> global:whereis_name(pm_master).
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
						?TRACE("received", [master_server(), erlang:binary_to_list(Data)]),
						% Args = [Item || K <- string:tokens(erlang:binary_to_list(Data), " "), Item <- erlang:list_to_atom(K)],
						[Meth|Args] = string:tokens(erlang:binary_to_list(Data), " "),
						?TRACE("received", [Meth, Args]),
						Output = gen_server:call(master_server(), {erlang:list_to_atom(Meth), Args}),
						?TRACE("received from gen_server", [Output]),
            gen_tcp:send(Socket, erlang:float_to_list(Output)),
						io:format("~p~n", [Output]),
						?TRACE("posted", [Output]),
            loop(Socket);
        {error, closed} ->
            ok
    end.