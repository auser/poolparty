% 
%  pm_spawner.erl
%  Spawn a process in this gen_server remotely
%  	and hear back from it when it's got stuff to say
%  
%  Created by Ari Lerner on 2008-12-05.
%  Copyright 2008 CitrusByte. All rights reserved.
% 
-module (pm_spawner).
-behaviour(gen_server).

-include_lib("../include/defines.hrl").

%% API
-export([start_link/0]).
-export ([run_command/1, run_command/2, check_command/1]).
-export ([collect_output/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { 
		processes = ?DICT:new() % Array of the output
	}).
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

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
	process_flag(trap_exit, true),
  {ok, #state{
		processes = ?DICT:new()
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
handle_call({run_command, Cmds}, _From, State) ->
	{ok, NewState} = run_command(Cmds, State),
	?TRACE("storing into state from call", [Cmds, NewState]),
	{reply, ok, NewState};
handle_call({check_command, Cmd}, _From, State) ->
	AtomCommand = erlang:list_to_atom(Cmd),
	?TRACE("is_key for ", [AtomCommand, ?DICT:is_key(AtomCommand, State#state.processes)]),
	case ?DICT:is_key(AtomCommand, State#state.processes) of
		false -> Reply = nil;
		true ->
			[Port, Pid] = ?DICT:fetch(AtomCommand, State#state.processes),
			?TRACE("Port", [Port]),
			Reply = Pid
	end,
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({run_command, Cmd}, State) ->
	{ok, NewState} = run_command(Cmd, State),
	{noreply, NewState};
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

% Quick helper for running commands
run_command(Cmd) -> gen_server:call(server_location(), {run_command, Cmd}).
check_command(Cmd) -> 
	Command = lists:flatten(io_lib:format("sh ~s", [Cmd])),
	gen_server:call(server_location(), {check_command, Command}).

% Run the command, start up the 
run_command(Cmd, State) ->
	Command = lists:flatten(io_lib:format("sh ~s", [Cmd])),
	Port = erlang:open_port({spawn, Command}, [stream, exit_status, stderr_to_stdout]),
	Pid = spawn(fun() -> collect_output(Port, []) end),
	AtomCommand = erlang:list_to_atom(Command),
	?TRACE("storing into state", [AtomCommand, Port, Pid]),
	NewState = State#state{processes = ?DICT:store(Command, [Port, Pid], State#state.processes)},
	{ok, NewState}.

collect_output(Port, Acc) ->
		receive
			{Port, {data, Data}} ->
				collect_output(Port, [Data | Acc]);
			{Port, {exit_status, 0}} ->
				catch erlang:port_close(Port),
				lists:flatten(lists:reverse(Acc));
			{Port, {exit_status, _}} ->
				catch erlang:port_close(Port),
				Output = lists:flatten(lists:reverse(Acc)),	
				{error, Output};
			_ ->
				collect_output(Port, Acc)				
	end.

server_location() -> global:whereis_name(?SERVER).