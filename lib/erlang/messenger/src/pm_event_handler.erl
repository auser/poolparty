-module(pm_event_manager).

%% API
-export([start_link/0, add_handler/1, notify/1]).
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error}
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
  gen_event:start_link({global, ?SERVER}).

%%--------------------------------------------------------------------
%% Function: add_handler(Module) -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(Module) ->
  gen_event:add_handler(?SERVER, Module, []).

%%--------------------------------------------------------------------
%% Function: notify(Event) -> ok | {error, Reason}
%% Description: Sends the Event through the event manager.
%%--------------------------------------------------------------------
notify(Event) ->
  gen_event:notify(?SERVER, Event).