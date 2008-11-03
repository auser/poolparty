%% @author Ari Lerner <ari.lerner@citrusbyte.com>
%% @copyright 2008 Ari Lerner.

%% @doc PoolParty panel

-module (cloudpanel_web).
-author('Ari Lerner <ari.lerner@citrusbyte.com>').

-import(random).
-export([start/1, stop/0, loop/2]).

-record(doc, {id, name, body}).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                _ ->
									wrap(read_page("index"))
                    % Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

wrap(Content) ->
	Header = read_partial("header"),
	Menu = read_partial("menu"),
	Footer = read_partial("footer"),
	Out = erlang:list_to_binary([Header, Menu, Content, Footer]),
	erlang:binary_to_list(Out).

read_page(Name) ->
	{_, Cont} = file:read_file("priv/www/pages/"++Name++".html"),
	Cont.
read_partial(Name) ->
	{_, Cont} = file:read_file("priv/www/partials/"++Name++".html"),
	Cont.
	
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
