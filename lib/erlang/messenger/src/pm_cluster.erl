-module(pm_cluster).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("../include/defines.hrl").

-export ([slaves/1, slaves/0]).
-export ([get_live_nodes/0]).
-export ([send_call/2, master/0]).

-export ([any_new_servers/0]).

send_call(Type, Args) ->
	Nodes = get_live_nodes(),
	rpc:multicall(Nodes, pm_node, Type, [Args]).

master() ->
	{erlang:node()}.

slaves([]) ->
	ok;

slaves([Host|Hosts]) ->
	Args = erl_system_args(),
	NodeName = "pp",
	{ok, Node} = slave:start_link(Host, NodeName, Args),
	io:format("Erlang node started = [~p]~n", [Node]),
	slaves(Hosts).

slaves() ->
	get_live_nodes().

erl_system_args()->
	Shared = case init:get_argument(shared) of
		error -> " ";
		{ok,[[]]} -> " -shared "
	end,
	lists:append(
		["-rsh ssh -setcookie", 
		atom_to_list(erlang:get_cookie()),
		Shared, " +Mea r10b "
	]).

any_new_servers() ->
	String = ". /etc/profile && server-list-active -c name",
	Nodes = lists:map(fun
		(No) ->
			erlang:list_to_atom(lists:append([No, "@", No]))
	end,string:tokens(os:cmd(String), "\n\t")),
	NewServers = Nodes -- get_live_nodes(),
	NewServers.
	
% Get the live nodes
get_live_nodes() ->
	nodes().
%% Do not forget to start erlang with a command like:
%% erl -rsh ssh -sname clustmaster