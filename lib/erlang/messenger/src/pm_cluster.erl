-module(cluster).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([slaves/1]).
	%% Argument:
	%% Hosts: List of hostname (string)
slaves([]) ->
	ok;

slaves([Host|Hosts]) ->
	Args = erl_system_args(),
	NodeName = "cluster",
	{ok, Node} = slave:start_link(Host, NodeName, Args),
	io:format("Erlang node started = [~p]~n", [Node]),
	slaves(Hosts).

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
%% Do not forget to start erlang with a command like:
%% erl -rsh ssh -sname clustmaster