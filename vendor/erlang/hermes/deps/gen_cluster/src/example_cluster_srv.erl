%%%-------------------------------------------------------------------
%%% File    : example_cluster_srv.erl
%%% Author  : nmurray@attinteractive.com
%%% Description : desc
%%% Created     : 2009-08-03
%%%-------------------------------------------------------------------
-module(example_cluster_srv).
-behaviour(gen_cluster).

-compile(export_all).

-export([start/0, start_link/1, start_named/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% gen_cluster callback
-export([handle_join/3, handle_node_joined/3, handle_leave/4]).

-define(TRACE(X, M),  io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M])).
-record(state, {name, pid, timestamp}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Alias for start_link
%%--------------------------------------------------------------------
start() ->
    start_link([]). 

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_cluster:start_link({local, ?MODULE}, ?MODULE, [Config], []).

start_named(Name, Config) ->
    gen_cluster:start_link({local, Name}, ?MODULE, [Config], []).

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

init(Args) -> 
    io:format(user, "TRACE ~p:~p called example init~n", [?MODULE, ?LINE]),
    InitialState = #state{name=example_cluster_srv, pid=self(), timestamp=0},
    {ok, InitialState}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({ohai}, _From, State) ->
    T = State#state.timestamp + 1,
    NewState = State#state{timestamp=T},
    {reply, hello, NewState};

handle_call({state}, _From, State) -> 
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) -> 
    {reply, todo_reply, State}.

% e.g.
% handle_call({create_ring}, _From, State) ->
%     {Reply, NewState} = handle_create_ring(State),
%     {reply, Reply, NewState};
%
% handle_call({join, OtherNode}, _From, State) ->
%     {Reply, NewState} = handle_join(OtherNode, State),
%     {reply, Reply, NewState};
% ...
% etc.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    ?TRACE("got stop cast", []),
    {stop, normal, State};
handle_cast(_Msg, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> 
    ?TRACE("terminating", []),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_join(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via this node
%% directly. JoiningPid is the node that joined. Note that JoiningPid may
%% join more than once. Pidlist contains all known pids. Pidlist includes
%% JoiningPid.
%%--------------------------------------------------------------------
handle_join(JoiningPid, Pidlist, State) ->
    io:format(user, "~p:~p handle join called: ~p Pidlist: ~p~n", [?MODULE, ?LINE, JoiningPid, Pidlist]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_node_joined(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via another node and
%%     the joining node is simply announcing its presence.
%%--------------------------------------------------------------------

handle_node_joined(JoiningPid, Pidlist, State) ->
    io:format(user, "~p:~p handle node_joined called: ~p Pidlist: ~p~n", [?MODULE, ?LINE, JoiningPid, Pidlist]),
    {ok, State}.

handle_leave(LeavingPid, Pidlist, Info, State) ->
    io:format(user, "~p:~p handle leave called: ~p, Info: ~p Pidlist: ~p~n", [?MODULE, ?LINE, LeavingPid, Info, Pidlist]),
    {ok, State}.

