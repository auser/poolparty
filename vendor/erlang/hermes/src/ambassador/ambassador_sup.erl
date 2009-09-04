-module (ambassador_sup).
-include ("hermes.hrl").

-export([start_link/0, stop/1]).

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
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->
  {ok,
    {{one_for_one, 5, 10 }, [ { ambassador, { ambassador, start_link, [] }, permanent, 3000, worker, [ ambassador ] }]}
  }.

%%====================================================================
%% Internal functions
%%====================================================================
stop(Args) ->
  ?INFO("Stopping ambassador~n", []),
  ambassador:stop(Args),
  supervisor:terminate_child(?MODULE, ambassador),
  ok.