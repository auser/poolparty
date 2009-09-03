-module (athens_sup).

-export([start_link/1, stop/1]).

-behavior(supervisor).

-export([init/1]).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%% @end 
%%--------------------------------------------------------------------
start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end 
%%--------------------------------------------------------------------
init([Args]) ->
  {ok,
    {{one_for_one, 5, 10 }, [ { athens_srv, { athens_srv, start_link, [Args] }, permanent, 3000, worker, [ athens_srv ] }]}
  }.

%%====================================================================
%% Internal functions
%%====================================================================
stop(Args) ->
  io:format("Stopping athens_srv~n"),
  athens_srv:stop(Args),
  supervisor:terminate_child(?MODULE, athens_srv),
  ok.