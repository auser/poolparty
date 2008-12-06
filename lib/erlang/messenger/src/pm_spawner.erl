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
	% process_flag(trap_exit, true),
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
		false -> 
			Reply = nil,
			NewState = State;
		true ->
			Pid = ?DICT:fetch(AtomCommand, State#state.processes),
			case erlang:is_pid(Pid) of
				true ->
					Pid ! {self(), collect},
					receive
						{command_response, OutFromCommand} -> 					
							Reply = OutFromCommand,
							NewState = State#state{processes = ?DICT:store(AtomCommand, OutFromCommand, State#state.processes)};
						Msg ->
							NewState = State,
							Reply = Msg
					after 
						1000 -> 
							Pid ! {self(), collect},
							NewState = State,
							Reply = "no output"
					end;
				false ->
					NewState = State,
					Reply = Pid
			end
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
	Command = lists:flatten(io_lib:format("~s", [Cmd])),
	gen_server:call(server_location(), {check_command, Command}).

% Run the command, start up the 
run_command(Cmd, State) ->
	Command = lists:flatten(io_lib:format("~s", [Cmd])),
	AtomCommand = erlang:list_to_atom(Command),
	Pid = spawn(fun() -> spawn_command(Command) end),
	Storage = Pid,
	?TRACE("storing into state", [AtomCommand]),
	NewState = State#state{processes = ?DICT:store(AtomCommand, Storage, State#state.processes)},
	{ok, NewState}.

spawn_command(Command) ->
	Port = erlang:open_port({spawn, Command}, [stream, exit_status, stderr_to_stdout, use_stdio]),
	ResponsePid = response_reader(Port),
	spawn_command_loop(Command, Port, ResponsePid).
	
spawn_command_loop(Command, Port, Responses) ->
	receive
		{Pid, collect} ->
			case erlang:is_pid(Responses) of
				true ->
					io:format("There is no output on your command yet, try again in a few seconds ~p~n", [Responses]),
					spawn_command_loop(Command, Port, Responses);
				false ->
					Pid ! {command_response, Responses}
			end;
		{exit_status, _} -> 
			spawn_command_loop(Command, Port, Responses);
		{done, Port, Response} ->			
			?TRACE("Received done message", Response),
			spawn_command_loop(Command, Port, Response);
	  {Port, {exit_status, 0}} ->			
			?TRACE("Received exit_status", [Port]),
			Responses ! {Port, {exit_status, 0, self()}},
			receive
				{ok, Resp} ->
					?TRACE("Received ok back", [Resp]),
					spawn_command_loop(Command, Port, Resp)
			end;
		Mes ->
			?TRACE("received message", [Mes]),
			Responses ! Mes,
			spawn_command_loop(Command, Port, Responses)
	end.

response_reader(Port) -> 
	ResponsePid = spawn(fun() -> response_reader_loop(Port, []) end),
	ResponsePid.
	
response_reader_loop(Port, Acc) ->
	receive
		{Port, {data, Bin}} ->
			?TRACE("received data", [Bin]),
			response_reader_loop(Port, [Bin|Acc]);
    {Port, {exit_status, 0, Pid}} ->			
      catch erlang:port_close(Port),
      Response = lists:flatten(lists:reverse(Acc)),
			?TRACE("received exit_status", [Response, Port]),
			Pid ! {ok, Response};
    {Port, {exit_status, _}} ->
      catch erlang:port_close(Port),
      Output = lists:flatten(lists:reverse(Acc)),
			{error, Output};
		{Pid, get_data} ->
			?TRACE("get_data for", [Pid, Acc]),
			Pid ! {data, Port, Acc},			
			response_reader_loop(Port, Acc);
		{_, collect} ->
			response_reader_loop(Port, Acc);
		Mes ->
			io:format("Unexpected message in response_reader_loop ~p~n", [Mes]),
			response_reader_loop(Port, Acc)
	end.

server_location() -> global:whereis_name(?SERVER).