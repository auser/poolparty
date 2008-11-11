%%%-------------------------------------------------------------------
%%% File    : pm_master.erl
%%% Author  : Ari Lerner <arilerner@mac.com>
%%% The client is a running process that will run on the master node
%%% and spawn requests to the pm_nodes and compile the responses
%%% for use within the poolparty network
%%%-------------------------------------------------------------------
-module(pm_master).
-behaviour(gen_server).

-include_lib("../include/defines.hrl").

-record(state, {
					nodes = ?DICT:new()  % Dictionary of running nodes
				}).
				
-define (SERVER, global:whereis_name(?MODULE)).

%% API

%% gen_server callbacks
-export([start_link/0,
         stop/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% Client function definitions
-export ([get_load/1, reconfigure_cloud/0]).
-export ([run_cmd/1, fire_cmd/1, get_current_nodes/0]).
-export ([shutdown_cloud/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

% pm_master:get_load("cpu").
get_load(Type) ->
	% {Loads, _} = pm_cluster:send_call(get_load_for_type, [Type]),
	% {Loads, _} = gen_server:call(?SERVER, {get_load, [Type]}),
	gen_server:call(?SERVER, {get_current_load, Type}).

% Send reconfigure tasks to every node
reconfigure_cloud() -> gen_server:cast(?SERVER, {force_reconfig}).

% Fire the given command on all nodes
run_cmd(Cmd) -> gen_server:call(?SERVER, {run_cmd, Cmd}).
fire_cmd(Cmd) -> gen_server:call(?SERVER, {fire_cmd, Cmd}).
	
% Shutdown
shutdown_cloud() ->
	pm_cluster:send_call(stop, []),
	{ok}.

get_current_nodes() -> gen_server:call(?SERVER, {get_current_nodes, []}).

stop() -> gen_server:cast(?MODULE, stop).	
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
	% pm_event_manager:add_handler(pm_master_event_handler),
	process_flag(trap_exit, true),
  {ok, #state{
		nodes = ?DICT:new()
	}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
% Handle load messages
handle_call({get_load, Args}, _From, State) ->
		Nodes = pm_cluster:get_live_nodes(),
		List = rpc:multicall(Nodes, pm_node, get_load_for_type, [Args]),
		Loads = utils:convert_responses_to_int_list(List),
		{reply, Loads, State};
handle_call({get_current_load, Types}, _From, State) ->	
	LoadForType = [utils:average_of_list(get_load_for_type(Type, State)) || Type <- Types],
	?TRACE("LoadForType: ",[LoadForType]),
	{reply, LoadForType, State};
handle_call({get_current_nodes, _Args}, _From, State) ->
	{reply, get_live_nodes(State), State}.
	
% handle_call(_Request, _From, State) ->
	% Nodes = pm_cluster:get_live_nodes(),
	% Reply = Reply = rpc:multicall(Nodes, pm_node, Request, []),
	% {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({update_node_load, From, Loads}, State) ->
	% ?TRACE("Cast with load message", [From, Loads]),
	% {_Nodes, NewState} = get_node_listing(From, State),
	% {noreply, NewState};
	{_LoadState, NewState} = store_load_for(From, Loads, State),
	{noreply, NewState};
handle_cast({force_reconfig}, State) ->
	Fun = fun(NodeEntry) ->
		{_Node, Pid} = NodeEntry,
		gen_server:cast(Pid, {reconfig}) end,
	run_on_nodes(Fun, State),
	{noreply, State};
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
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Private methods
get_node_listing(Name, State) ->
	% Find or create the stored node	
	case ?DICT:is_key(Name, State#state.nodes) of
		true -> NodeStore = ?DICT:fetch(Name, State#state.nodes), {NodeStore, State};
		false -> 
			NodeStore = {},  NewState = State#state{nodes = ?DICT:store(Name, NodeStore, State#state.nodes)}, {NodeStore, NewState}
	end.

get_load_for_type(Type, State) ->
	Loads = [ get_load_for_node(Type, Name, State) || Name <- get_live_nodes(State) ],
	[Load || Load <- Loads, Load =/= false, Load > 0.0].

get_load_for_node(Type, Name, State) ->
	{NodeStore, _} = get_node_listing(Name, State),
	case proplists:is_defined(erlang:list_to_atom(Type), NodeStore) of
		true -> proplists:get_value(erlang:list_to_atom(Type), NodeStore);
		_ -> false
	end.

get_node_pids(State) ->
	Nodes = dict:fetch_keys(State#state.nodes),	
	[ {Node, global:whereis_name(Node)} || Node <- Nodes, global:whereis_name(Node) =/= undefined].
	
get_live_nodes(State) ->
	NodePids = get_node_pids(State),
	RespondingNodes = lists:map(
	fun(NodeEntry) ->
		{Node, Pid} = NodeEntry,
		case is_pid(Pid) of
			true ->
				case gen_server:call(Pid, {still_there}) of
					still_here -> Node;
					_ -> false
				end;
			false -> false
		end
	end, NodePids),
	[ Node || Node <- RespondingNodes, Node =/= false].

run_on_nodes(Fun, State) ->
	NodePids = get_node_pids(State),
	lists:map(Fun, NodePids).
	
store_load_for(Name, Loads, State) ->
	NewNodeEntry = [ {erlang:list_to_atom(Key), proplists:get_value(Key, Loads)} || Key <- proplists:get_keys(Loads)],
	NewState = ?DICT:store(Name, NewNodeEntry, State#state.nodes),
	{NewNodeEntry, State#state{nodes = NewState}}.