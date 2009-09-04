%%%-------------------------------------------------------------------
%%% File    : stoplight_srv.erl
%%% Author  : nmurray@attinteractive.com, alerner@attinteractive.com
%%% Description : Cascading gen_server behavior that implements process clustering.  
%%% See: http://wiki.trapexit.org/index.php/Cascading_Behaviours
%%% Created     : 2009-08-03
%%%
%%% NOTES:
%%% * uses distributed erlang (today)
%%% * registers one global pid in the format of "gen_cluster_" ++
%%%   atom_to_list(Mod) where Mod is the module using gen_cluster. This allows
%%%   for one "rallying point" for each new node that joins the cluster.
%%% * If the node holding the rally point fails, a new node needs to take over the registered name
%%%-------------------------------------------------------------------
-module(gen_cluster).
-include_lib("../include/gen_cluster.hrl").

%% Define this module as a gen_server callback module.
-behaviour(gen_server).

%% Export the same API as gen_server.
-export([start/3, start/4,
    start_link/3, start_link/4,
    call/2, call/3,
    cast/2, reply/2,
    abcast/2, abcast/3,
    multi_call/2, multi_call/3, multi_call/4,
    enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/5]).

%% Export the callbacks that gen_server expects
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Define the behaviour's required callbacks.
-export([behaviour_info/1]).

%% Helper functions
-export([plist/1]).

behaviour_info(callbacks) ->
    [
    % gen_cluster
      {handle_join, 3}, {handle_node_joined, 3}, {handle_leave, 4},
    % gen_server      
      {init,1}, {handle_call,3},{handle_cast,2},{handle_info,2}, {terminate,2},{code_change,3}
   ];

behaviour_info(_) ->
    undefined.

%% State data record.
-record(state, {module, state, data, plist, seed}).

%% debugging helper
-define (DEBUG, false).
-define (TRACE(X, M), case ?DEBUG of
  true -> io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M]);
  false -> ok
end).

%% Users will use these start functions instead of gen_server's.
%% We add the user's module name to the arguments and call
%% server's start function with our module name instead.
start(Mod, Args, Options) ->
    gen_server:start(?MODULE, [Mod, Args], Options).
start(Name, Mod, Args, Options) ->
    gen_server:start(Name, ?MODULE, [Mod, Args], Options).
start_link(Mod, Args, Options) ->
    gen_server:start(?MODULE, [Mod, Args], Options).
start_link(Name, Mod, Args, Options) ->
    gen_server:start(Name, ?MODULE, [Mod, Args], Options).

%% Delegate the rest of the reqests to gen_server
call(Name, Request) ->
    gen_server:call(Name, Request).
call(Name, Request, Timeout) ->
   gen_server:call(Name, Request, Timeout).
cast(Name, Request) ->
    gen_server:cast(Name, Request).
reply(To, Reply) ->
    gen_server:reply(To, Reply).
abcast(Name, Request) ->
    gen_server:abcast(Name, Request).
abcast(Nodes, Name, Request) ->
    gen_server:abcast(Nodes, Name, Request).
multi_call(Name, Req) ->
    gen_server:multi_call(Name, Req).
multi_call(Nodes, Name, Req)  ->
    gen_server:multi_call(Nodes, Name, Req).
multi_call(Nodes, Name, Req, Timeout)  ->
    gen_server:multi_call(Nodes, Name, Req, Timeout).
enter_loop(Mod, Options, State) ->
    gen_server:enter_loop(Mod, Options, State).
enter_loop(Mod, Options, State, Timeout) ->
    gen_server:enter_loop(Mod, Options, State, Timeout).
enter_loop(Mod, Options, State, ServerName, Timeout) ->
    gen_server:enter_loop(Mod, Options, State, ServerName, Timeout).
wake_hib(Parent, Name, State, Mod, Debug) ->
    gen_server:wake_hib(Parent, Name, State, Mod, Debug).

plist(PidRef) -> % {ok, Plist}
    call(PidRef, {'$gen_cluster', plist}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%
%% run the user's init/1 and store the user's state data in our internal
%% state data record for later reference.
%%--------------------------------------------------------------------

init([Mod, Args]) ->
    Seed = case Args of
        {seed, Value} -> Value;
        _ -> undefined
    end,
    InitialState = #state{module=Mod, plist=[self()], seed=Seed},
    {ok, State1} = join_existing_cluster(InitialState),
    {_Resp, State2} = start_cluster_if_needed(State1),
 
    case Mod:init(Args) of
        {ok, ExtState} ->
            StateData = State2#state{module = Mod, state = ExtState},
            {ok, StateData};
        {ok, ExtStateName, ExtStateData} -> 
            StateData = State2#state{module = Mod, state = ExtStateName, data = ExtStateData},
            {ok, StateData};
        {ok, ExtStateName, ExtStateData, Timeout} ->
            StateData = State2#state{module = Mod, state = ExtStateName, data = ExtStateData},
            {ok, StateData, Timeout};
        {stop, Reason} ->
            {stop, Reason};
        Other ->
          ?TRACE("got other:", Other),
          exit(bad_gen_cluster_init_call) % hmmm
    end.

%%--------------------------------------------------------------------
%% Function:  handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                        {reply, Reply, State, Timeout} |
%%                                                      {noreply, State} |
%%                                             {noreply, State, Timeout} |
%%                                          {stop, Reason, Reply, State} |
%%                                                 {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({'$gen_cluster', join}, From, State) ->
    ?TRACE("$gen_cluster join", State),
    {ok, NewState} = handle_node_joining(From, State),
    Reply = {ok, NewState#state.plist},
    {reply, Reply, NewState};

handle_call({'$gen_cluster', joined_announcement, KnownRing}, From, State) ->
    ?TRACE("$gen_cluster joined_announcement", State),
    {ok, NewState} = handle_node_joined_announcement(From, KnownRing, State),
    Reply = {ok, NewState#state.plist},
    {reply, Reply, NewState};

handle_call({'$gen_cluster', plist}, _From, State) ->
    Reply = {ok, State#state.plist},
    {reply, Reply, State};

handle_call({'$gen_cluster', globally_registered_name}, _From, State) ->
    Reply = {ok, globally_registered_name(State)},
    {reply, Reply, State};

handle_call(Request, From, State) -> 
    Mod = State#state.module,
    ExtState = State#state.state,
    Reply = Mod:handle_call(Request, From, ExtState),
    handle_call_reply(Reply, From, State).

% handle the replies by updating and substituting our own state
handle_call_reply({reply, Reply, ExtState}, _From, State) ->
    NewState = State#state{state=ExtState},
    {reply, Reply, NewState};

handle_call_reply({reply, Reply, ExtState, Timeout}, _From, State) ->
    NewState = State#state{state=ExtState},
    {reply, Reply, NewState, Timeout};

handle_call_reply({noreply, ExtState}, _From, State)  ->
    NewState = State#state{state=ExtState},
    {noreply, NewState};

handle_call_reply({noreply, ExtState, Timeout}, _From, State) ->
    NewState = State#state{state=ExtState},
    {noreply, NewState, Timeout};

handle_call_reply({stop, Reason, Reply, ExtState}, _From, State)  ->
    NewState = State#state{state=ExtState},
    {stop, Reason, Reply, NewState};

handle_call_reply({stop, Reason, ExtState}, _From, State) ->
    NewState = State#state{state=ExtState},
    {stop, Reason, NewState}.
    % handle Other?

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) -> 
    Mod = State#state.module,
    ExtState = State#state.state,
    Reply = Mod:handle_cast(Msg, ExtState),
    handle_cast_info_reply(Reply, State).

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', MonitorRef, process, Pid, Info}, State) ->
    ?TRACE("received 'DOWN'. Removing node from list. Info:", Info),
    ExtState = State#state.state,
    Mod = State#state.module,

    case does_pid_exist_in_plist(Pid, State) of
        true ->
            {ok, NewState2} = remove_pid_from_plist(Pid, State),
            Pidlist = NewState2#state.plist,
            {ok, NewExtState} = Mod:handle_leave(Pid, Pidlist, Info, ExtState),
            NewState3 = take_over_globally_registered_name_if_needed(NewState2),
            % NewState3 = NewState2,
            NewState4 = NewState3#state{state=NewExtState},
            {noreply, NewState4};
        false ->
            Reply = Mod:handle_info({'DOWN', MonitorRef, process, Pid, Info}, ExtState),
            handle_cast_info_reply(Reply, State)
    end;

handle_info(Info, State) -> 
    ?TRACE("got other INFO", val),
    Mod = State#state.module,
    ExtState = State#state.state,
    Reply = Mod:handle_info(Info, ExtState),
    handle_cast_info_reply(Reply, State).

handle_cast_info_reply({noreply, ExtState}, State) ->
    NewState = State#state{state=ExtState},
    {noreply, NewState};
handle_cast_info_reply({noreply, ExtState, Timeout}, State) ->
    NewState = State#state{state=ExtState},
    {noreply, NewState, Timeout};
handle_cast_info_reply({stop, Reason, ExtState}, State) ->
    NewState = State#state{state=ExtState},
    {stop, Reason, NewState}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) -> 
    Mod = State#state.module,
    ExtState = State#state.state,
    _ = Mod:terminate(Reason, ExtState),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) -> 
    Mod = State#state.module,
    ExtState = State#state.state,
    {ok, NewExtState} = Mod:code_change(OldVsn, ExtState, Extra),
    NewState = State#state{state=NewExtState},
    {ok, NewState}.

%%--------------------------------------------------------------------
%% Func: handle_node_joining(OtherNode, State) -> {ok, NewState}
%% Description: Called when another node joins the server cluster. 
%%--------------------------------------------------------------------
handle_node_joining({OtherPid, _Tag}, State) ->
    {ok, NewState} = add_pid_to_plist(OtherPid, State),

    % callback
    #state{module=Mod, plist=Plist, state=ExtState} = NewState,
    {ok, NewExtState} = Mod:handle_join(OtherPid, Plist, ExtState),

    % update the external state
    StateData = NewState#state{state=NewExtState},
    {ok, StateData}.

%%--------------------------------------------------------------------
%% Func: handle_node_joined_announcement(KnownRing, State) -> {ok, NewState}
%% Description: When a node joins a known server, it then broadcasts to all
%% other servers that it joined. It tells all other servers about the entire
%% pidlist it received from the known node. This is a check to make sure that
%% everytime a node joins all the other nodes know about it as well as every
%% other node in the cluster.
%% 
%% TODO, consider removing this method entirely
%%--------------------------------------------------------------------
handle_node_joined_announcement({OtherPid, _Tag}, KnownRing, State) ->
    {ok, NewState} = add_pids_to_plist(KnownRing, State),

    % callback
    #state{module=Mod, plist=Plist, state=ExtState} = NewState,
    {ok, NewExtState} = Mod:handle_node_joined(OtherPid, Plist, ExtState),

    % update the external state
    StateData = NewState#state{state=NewExtState},

    {ok, StateData}.

%%--------------------------------------------------------------------
%% Func: join_existing_cluster(State) -> {ok, NewState} | false
%% Description: Look for any existing servers in the cluster, try to join them
%%--------------------------------------------------------------------
join_existing_cluster(State) ->
    Mod = State#state.module,
    Servers = get_seed_nodes(State),
    connect_to_servers(Servers),
    global:sync(), % otherwise we may not see the pid yet
    NewState = case whereis_global(State) of % join unless we are the main server 
        undefined ->
            ?TRACE("existing cluster undefined", undefined),
            State;
        X when X =:= self() ->
            ?TRACE("we are the cluster, skipping", X),
            State;
        _ ->
            ?TRACE("joining server...", whereis_global(State)),
            ?TRACE("join state", [State, Mod]),
            {ok, KnownPlist} = gen_cluster:call({global, globally_registered_name(State)}, {'$gen_cluster', join}),
            {ok, NewInformedState} = add_pids_to_plist(KnownPlist, State),
            broadcast_join_announcement(NewInformedState)
    end,
    {ok, NewState}.

connect_to_servers(ServerNames) ->
    ?TRACE("servernames", ServerNames),
   ServerRefs = lists:map(fun(Server) ->
      case Server of
      undefined -> 
          ?TRACE("warning, skipping server", Server),
          skip; % do nothing
      _ -> 
         ?TRACE("connecting to server: ", Server),
         Node = Server,
         case net_adm:ping(Node) of
             pong ->
                 ok;
             _ ->
                 ?TRACE("WARNING: ping of Node failed:", Node) % should this be a bigger failure? how should we handle this so that way the first server doesn't always have to have this problem?
         end
      end
    end,
    ServerNames),
   {ok, ServerRefs}.

%%--------------------------------------------------------------------
%% Func: start_cluster_if_needed(State) -> {{ok, yes}, NewState} |
%%                                         {{ok, no}, NewState}
%% Description: Start cluster if we need to
%%--------------------------------------------------------------------
start_cluster_if_needed(State) ->
    global:sync(), % otherwise we may not see the pid yet
    {Resp, NewState} = case whereis_global(State) of
      undefined ->
          start_cluster(State);
      _ ->
          {no, State}
    end,
    {{ok, Resp}, NewState}.

whereis_global(State) ->
    global:whereis_name(globally_registered_name(State)).

%% gen_cluster will globally register a pid of the format below. This allows
%% for each module that becomes a gen_cluster to have a central rally point and
%% will not confluct with other modules using gen_cluster
globally_registered_name(State) ->
    Mod = State#state.module,
    "gen_cluster_" ++ atom_to_list(Mod).

%%--------------------------------------------------------------------
%% Func: start_cluster(State) -> {yes, NewState} | {no, NewState}
%% Description: Start a new cluster, basically just globally register a pid for
%% joining
%%--------------------------------------------------------------------
start_cluster(State) ->
    global:sync(), % otherwise we may not see the other pids yet
    ?TRACE("Starting server:", globally_registered_name(State)),
    RegisterResp = global:register_name(globally_registered_name(State), self()),
    {RegisterResp, State}.

add_pids_to_plist([Head|OtherPids], State) ->
    {ok, NewState} = add_pid_to_plist(Head, State),
    add_pids_to_plist(OtherPids, NewState);
add_pids_to_plist([], State) ->
    {ok, State}.

add_pid_to_plist(OtherPid, State) ->
    % Exists = lists:any(fun(Elem) -> Elem =:= OtherPid end, State#state.plist),
    NewPlist = case does_pid_exist_in_plist(OtherPid, State) of
        true ->
          State#state.plist;
        false ->
          % monitor that pid
          erlang:monitor(process, OtherPid),
          % add the other pid to our plist
          [OtherPid|State#state.plist]
    end,
    NewState  = State#state{plist=NewPlist},
    {ok, NewState}.

does_pid_exist_in_plist(OtherPid, State) -> % bool() 
    lists:any(fun(Elem) -> Elem =:= OtherPid end, State#state.plist).

remove_pid_from_plist(OtherPid, State) ->
    NewPlist = lists:delete(OtherPid, State#state.plist),
    NewState  = State#state{plist=NewPlist},
    {ok, NewState}.

broadcast_join_announcement(State) ->
    NotSelfPids   = lists:delete(self(), State#state.plist),
    NotGlobalPids = lists:delete(whereis_global(State), NotSelfPids),
    [call(Pid, {'$gen_cluster', joined_announcement, State#state.plist}) || Pid <- NotGlobalPids],
    State.

take_over_globally_registered_name_if_needed(State) -> % NewState
    case need_to_take_over_globally_registered_name(State) of
        true -> 
            {YesNo, NewState} = start_cluster(State),  
            NewState;
        false -> State
    end. 

need_to_take_over_globally_registered_name(State) -> % bool()
    case whereis_global(State) of
        undefined -> true;
        Pid ->  
            case is_process_alive(Pid) of
                true -> false;
                false -> true
            end
    end.

% list of Nodes
% Node will be sent to net_adm:ping
get_seed_nodes(State) ->
    Servers = [],
    Mod = State#state.module,
    ExtState = State#state.state,
    Servers1 = case erlang:function_exported(Mod, seed_nodes, 1) of
        true -> lists:append([Mod:seed_nodes(ExtState), Servers]);
        false -> Servers
    end,

    Servers2 = case init:get_argument(gen_cluster_known) of
        {ok, [[Server]]} ->
            lists:append([list_to_atom(Server), Servers1]);
         _ ->
            case State#state.seed of
                [Server|_Servers] ->
                   % [{Server, undefined}]; % ??
                   [Server|Servers1]; % ??
                _ ->
                 Servers1 
            end
    end,

    % file should be of the format
    %
    % "node@nohost".
    % "foo@bar".
    Servers3 = case os:getenv("GEN_CLUSTER_SEED_CONFIG") of
        false -> [];
        File ->
            case file:consult(File) of
                {ok, Terms} -> lists:append(Servers2, Terms);
                _ -> Servers2
            end
    end,

    Servers3.
