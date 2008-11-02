-module(webservice).
-export([start/0, start/2, loop/2, stop/0, test/0]).
-export([dolog/2]).
-define(PORT, 8080).

start() ->
 start("/home/rolphin/Devel/Web", ?PORT).

start(Wwwroot, Port) ->
 Loop = fun (Req) ->
  ?MODULE:loop(Req, Wwwroot)
 end,
 mochiweb_http:start([{loop, Loop}, {name, ?MODULE}, {port, Port}]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
 log(Req),
 case string:tokens(Req:get(path), "/") of
  [ "dump" ] ->
   Req:ok({"text/plain",
    io_lib:format("~p~n", [Req:dump()])});

  [ "favicon.ico" ] ->
   Req:respond({404, [], ""});

  [ "codepath" ] ->
   Req:ok({"text/plain",
    io_lib:format("codepath: ~p~n", [code:get_path()])});

  [ "codepath", "json" ] ->
   Req:ok({"text/plain",
    mochijson:encode({array, code:get_path()})});

  [ Path, Fun | Elems ] ->
   % Every module name should begin with 'w'
   dispatch(Req, DocRoot, list_to_atom("w" ++ Path), Fun, Elems);

  [] ->
   launch(Req, DocRoot, wdefault, do, []);

  _ ->
   Req:respond({502, [], []})
   
 end.

dispatch(Req, DocRoot, Module, Fun, Elems) ->
 M = Req:get(method),
 case M of
  'GET' ->
   launch(Req, DocRoot, Module, Fun, Elems);
  'POST' ->
   launch(Req, DocRoot, Module, Fun, Elems);
  'PUT' ->
   launch(Req, DocRoot, Module, Fun, Elems);
  'DELETE' ->
   launch(Req, DocRoot, Module, Fun, Elems);
  'HEAD' ->
   launch(Req, DocRoot, Module, Fun, Elems);
  _Any ->
   launch(Req, DocRoot, wdefault, get, [])
 end.

launch(Req, DocRoot, wcontent, Fun, Args) ->
 case catch wcontent:default(Req, DocRoot, [ Fun | Args] ) of
  {'EXIT', {Type, _Error}} ->
   Req:ok({"text/plain",  
    io_lib:format("GET Error: '~p' for '~p' ~p ~p~n~p~n", [Type, wcontent, Fun, Args, _Error])});
  _ ->
   ok
 end;
 
launch(Req, DocRoot, Module, Fun, Args) ->
 F = list_to_atom(Fun),
 case catch Module:F(Req, DocRoot, Args) of
  {'EXIT', {Type, _Error}} ->
   Req:ok({"text/plain",  
    io_lib:format("~p Error: '~p' for ~p ~p ~p~n~p~n", [Req:get(method), Type, Module, Fun, Args, _Error])});
  _ ->
   ok
 end.

log(Req) ->
 Ip = Req:get(peer),
 spawn(?MODULE, dolog, [Req, Ip]).

dolog(Req, Ip) ->
 stat_logger:log("~p ~p", [Ip, Req:get(path)]).
