%% @author Ari Lerner <ari.lerner@citrusbyte.com>
%% @copyright 2008 Ari Lerner.

%% @doc PoolParty panel

-module (cloudpanel_web).
-author('Ari Lerner <ari.lerner@citrusbyte.com>').

-export([start/1, stop/0, loop/2]).

-define(TIMEOUT, 20000).
-define (PORT, 8001).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
						?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{max, 1000000}, {name, ?MODULE}, {loop, Loop}, {port, PORT} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
	ok.

subst(Template, Values) when is_list(Values) ->
	erlang:list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
	case string:str(Path, "?") of
		0 ->
			Path;
		N ->
			string:substr(Path, 1, string:len(Path) - (N + 1))
	end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.