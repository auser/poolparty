%% @author Ari Lerner <ari.lerner@citrusbyte.com>
%% @copyright 2008 Ari Lerner.

%% @doc PoolParty panel

-module (cloudpanel_web).
-author('Ari Lerner <ari.lerner@citrusbyte.com>').

-import(random).
-export([start/1, stop/0, loop/2]).
-include ("cloudpanel.hrl").

%% External API

start(Options) ->
    {DocRoot, Options1} = utils:get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
		Path = Req:get(path),
		case {Path, Req:get(method)} of
			{"/tail", 'GET'} ->
				Body = tailor:go(Req:parse_qs()),
				Req:ok({"text/html", [], Body});
			{"/", 'GET'} ->
				Body = views:wrap_page("index"),
				Req:ok({"text/html", [], Body});
			{_, 'GET'} ->
				Body = views:wrap_page(Path),
				Req:ok({"text/html", [], Body});
       _ ->
        Req:respond({501, [], []})
    end.