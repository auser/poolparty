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
-export([start_link/1, start_link/0]).
-export ([run_command/1, collect_output/2]).

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
handle_call({check_command, Cmd}, _From, State) ->
	case ?DICT:is_key(Name, State#state.processes) of
		false -> Reply = nil;
		true ->
			[Port, Pid] = ?DICT:fetch(Cmd, State#state.processes),
	end,
	{reply, Reply, NewState};
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
	NewState = run_command(Cmd, State),
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

% Run the command, start up the 
run_command(Cmd, State) ->
	Command = lists:flatten(io:lib:format("sh ~s", [Cmd])),
	Port = erlang:open_port({spawn, Command}, [stream, exit_status, stderr_to_stdout]),
	Pid = spawn(fun() -> collect_output(Port, []) end),
	NewState = State#state{processes = ?DICT:store(Command, [Port, Pid], State#state.processes)}
	NewState.

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