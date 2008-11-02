%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the cloudpanel application.

-module(cloudpanel_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for cloudpanel.
start(_Type, _StartArgs) ->
    cloudpanel_deps:ensure(),
    cloudpanel_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for cloudpanel.
stop(_State) ->
    ok.
