%%%-------------------------------------------------------------------
%%% File    : athens_srv.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Aug 19 18:46:00 PDT 2009
%%%-------------------------------------------------------------------

-module (athens_srv).
-behaviour(gen_cluster).

%% API
-export([start/0, start_link/1, start_named/2]).

% TODO: REFINE
-export ([  
            call_election/4,
            call_election/5
         ]).
         
% -export ([  
%             submit_ballot/4
%          ]).

-export ([  nodes/0,
            nodes/1
         ]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% gen_cluster callback
-export([handle_join/3, handle_node_joined/3, handle_leave/4]).

-define(TRACE(X, M),  io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M])).
-define(SERVER, ?MODULE).
-define (BALLOT, mapreduce).

-record(state, {
        
        }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Alias for start_link
%%--------------------------------------------------------------------
start() -> start_link([]). 

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
  gen_cluster:start_link({local, ?MODULE}, ?SERVER, [Config], []).

start_named(Name, Config) ->
  gen_cluster:start_link({local, Name}, ?MODULE, [Config], []).

call_election(M, F, A, Comparison) ->
  {ok, NodePids} = gen_cluster:plist(?SERVER),
  ?BALLOT:submit(M, F, A, Comparison, NodePids).

call_election(M, F, A, Comparison, Nodes) ->
  ?BALLOT:submit(M, F, A, Comparison, Nodes).
  
% submit_ballot(On, M,F,A) ->
%   gen_cluster:call(On, {ballot, M,F,A}).

nodes() ->
  ?MODULE:nodes(?SERVER).
  
nodes(Name) ->
  {ok, NodePids} = gen_cluster:plist(Name),
  [ node(Pid) || Pid <- NodePids ].

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_A) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({election, M, F, A, Nodes}, _From, State) ->
  O = ?BALLOT:submit(M, F, A, Nodes),
  {reply, O, State};
  
handle_call({ballot, M, F, A}, _From, State) ->
  O = apply(M, F, A),
  {reply, O, State};
  
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
  ?TRACE("handle_info", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%====================================================================
%% gen_cluster
%%====================================================================

%%--------------------------------------------------------------------
%% Function: handle_join(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via this node
%% directly. JoiningPid is the node that joined. Note that JoiningPid may
%% join more than once. Pidlist contains all known pids. Pidlist includes
%% JoiningPid.
%%--------------------------------------------------------------------
handle_join(_JoiningPid, _Pidlist, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_node_joined(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via another node and
%%     the joining node is simply announcing its presence.
%%--------------------------------------------------------------------

handle_node_joined(_JoiningPid, _Pidlist, State) ->
    {ok, State}.

handle_leave(_LeavingPid, _Pidlist, _Info, State) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
