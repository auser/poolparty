%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(cloudpanel).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the cloudpanel server.
start() ->
    cloudpanel_deps:ensure(),
    ensure_started(crypto),
    application:start(cloudpanel).

%% @spec stop() -> ok
%% @doc Stop the cloudpanel server.
stop() ->
    Res = application:stop(cloudpanel),
    application:stop(crypto),
    Res.
