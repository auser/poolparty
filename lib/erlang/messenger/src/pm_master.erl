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

% pm_master:get_load("0", "cpu").
get_load(Type) ->
	% {Loads, _} = pm_cluster:send_call(get_load_for_type, [Type]),
	{Loads, _} = gen_server:call(?SERVER, {get_load, [Type]}),
	utils:convert_responses_to_int_list(Loads).

% Send reconfigure tasks to every node
reconfigure_cloud() ->
	gen_server:call(?SERVER, {run_reconfig}).

% Fire the given command on all nodes
run_cmd(Cmd) -> gen_server:call(?SERVER, {run_cmd, Cmd}).
fire_cmd(Cmd) -> gen_server:call(?SERVER, {fire_cmd, Cmd}).
	
% Shutdown
shutdown_cloud() ->
	pm_cluster:send_call(stop, []),
	{ok}.

get_current_nodes() ->
	gen_server:call(?SERVER, {get_live_nodes}).

stop() ->
	gen_server:cast(?MODULE, stop).	
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
		{reply, List, State};		
handle_call({get_live_nodes}, _From, State) ->
	{reply, dict:fetch_keys(State#state.nodes), State}.
	
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
	{LoadStore, NewState} = get_load_for(From, State),
	[?TRACE("Type Load", [Type, Load]) || {Type, Load} <- Loads],
	NewLoadStore = lists:map(
		fun(Type, Load) ->
			BinType = erlang:list_to_binary(Type),
			case ?DICT:is_key(BinType, LoadStore) of
				true -> ?DICT:update(BinType, Load, LoadStore);
				false ->
					LoadStore = ?DICT:new(), 
					NewState = ?DICT:store(BinType, Load, LoadStore)
			end, Loads),
	?TRACE("Load", [dict:fetch_keys(LoadStore)]),
	{noreply, NewState};
handle_cast(stop, _State) ->
	{ok};
handle_cast(Msg, State) ->
	?TRACE("Cast with unknown message", [Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	?TRACE("Handling info", [Info]),
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
	?TRACE("Find ", [Name, ?DICT:is_key(Name, State#state.nodes)]),
	case ?DICT:is_key(Name, State#state.nodes) of
		true -> 
			NodeStore = dict:fetch(Name, State#state.nodes),
			{NodeStore, State};
		false -> 
			NodeStore = ?DICT:new(), 
			NewState = State#state{nodes = ?DICT:store(Name, NodeStore, State#state.nodes)},
			{NodeStore, NewState}
	end.

get_load_for(Name, State) ->
	{Node, NewState} = get_node_listing(Name, State),	
	case ?DICT:is_key(load, Node) of
		true -> 
			{dict:fetch(load, Node), NewState};
		false -> 
			LoadStore = ?DICT:new(),
			NewState1 = NewState#state{nodes = ?DICT:store(Name, LoadStore, NewState#state.nodes)},
			{LoadStore, NewState1}
	end.