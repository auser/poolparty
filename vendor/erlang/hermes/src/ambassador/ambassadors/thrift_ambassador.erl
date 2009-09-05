%%%-------------------------------------------------------------------
%%% File    : 
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Aug 12 14:19:36 PDT 2009
%%%-------------------------------------------------------------------
-module (thrift_ambassador).
-include ("hermes.hrl").

-include ("poolparty_types.hrl").
-include ("commandInterface_thrift.hrl").
-include ("poolparty_constants.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export ([
          ask/3,
          run/3,
          stop/0
         ]).

-export ([start_thrift_server/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          start_args,   % args to start with
          thrift_pid,   % thrift client pid
          port,         % port
          retry_times   % times to retry
       }).
                 
-define(SERVER, ?MODULE).
-define(PORT_OPTIONS, [stream, {line, 1024}, binary, exit_status, hide]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

ask(CloudName, Func, Msg) -> gen_server:call(?MODULE, {cloud_query, CloudName, Func, Msg}).
run(CloudName, Func, Msg) -> gen_server:cast(?MODULE, {cloud_run, CloudName, Func, Msg}).

%%--------------------------------------------------------------------
%% Function: stop () -> ok
%% Description: Stop ourselves and the socket_server
%%--------------------------------------------------------------------
stop() ->
  ?INFO("Stopping ~p~n", [?MODULE]),
  stop_thrift_client(config:read()),
  % thrift_socket_server:stop(get_hostname()),
  ok.
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
init([Args]) ->
  ThriftPort = proplists:get_value(proto_port, Args),
  CloudConfig = proplists:get_value(clouds_config, Args),
  CloudName = proplists:get_value(cloud_name, Args),
  
  stop_thrift_client(Args),
  timer:sleep(500),
  
  Port = start_thrift_cloud_server(Args),
  
  {ok, HostName} = get_hostname(),
  BannerArr = [
    {"thrift_port", erlang:integer_to_list(ThriftPort)},
    {"thrift client hostname", HostName},
    {"cloud name", CloudName},
    {"cloud config", CloudConfig}
  ],
  loudmouth:banner("Started thrift", BannerArr),

  % O = thrift_client:start_link(HostName, ThriftPort, commandInterface_thrift),
  timer:sleep(1000),
  ?INFO("Connecting with thrift_client on ~p:~p~n", [HostName, ThriftPort]),
  P = case thrift_client:start_link(HostName, ThriftPort, commandInterface_thrift) of
    {error, R} ->
      ?ERROR("Thrift client could not connect to: ~p, ~p, ~p = ~p~n", [HostName, ThriftPort, commandInterface_thrift, R]),
      timer:sleep(1000),
      self();
    {ok, C} -> C
  end,
  ?INFO("Connected! Ready to go!~n~n", []),
  {ok, #state{
    start_args = Args,
    thrift_pid = P,
    port = Port,
    retry_times = 0
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
handle_call({cloud_query, CloudName, Fun, [Args]}, _From, #state{thrift_pid = P, start_args = StartArgs} = State) ->
  ?INFO("Calling cloud_query: ~p ~p(~p)~n", [CloudName, Fun, Args]),
  Reply = case catch cloud_query(P, CloudName, Fun, Args) of
    {ok, {cloudResponse, _BinCloudName, _BinFun, [<<"unhandled monitor">>]}} -> {error, unhandled_monitor};
    {ok, {cloudResponse, _BinCloudName, _BinFun, BinResponse}} -> 
      ?INFO("Got back: ~p~n", [utils:turn_to_list(BinResponse)]),
      {ok, utils:turn_to_list(BinResponse)};
    % Errors
    {error, {noproc, Reason}} ->
      ?ERROR("Cloud thrift server died. Restarting it: ~p~n", [Reason]),
      start_thrift_cloud_server(StartArgs),
      {error, Reason};
    Else -> 
      ?ERROR("There was an error: ~p~n", [Else]),
      {error, Else}
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
handle_cast({cloud_run, CloudName, Fun, [Args]}, #state{thrift_pid = P} = State) ->
  cloud_run(P, CloudName, Fun, Args),
  {noreply, State};
  
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', _Pid, Reason}, #state{start_args = _Args} = State) ->
  % Port = start_thrift_cloud_server(Args),
  % NewState = State#state{port = Port},
  ?INFO("Received Exit in ~p: ~p~n", [?MODULE, Reason]),
  {noreply, State};

% The process could not be started, because of some foreign error
handle_info({_Port,{exit_status,10}}, State) -> {noreply, State};
handle_info({_Port,{exit_status,0}}, #state{start_args = Args} = State) -> {noreply, State};
    
handle_info({Port, {exit_status, Status}}, #state{port=Port}=State) ->
    ?ERROR("OS Process died with status: ~p", [Status]),
    {stop, {exit_status, Status}, State};

handle_info({_Port, {data, {eol, Data}}}, State) ->
  ?INFO("~p~n", [Data]),
  {noreply, State};
    
handle_info(Info, State) ->
  ?INFO("Received info in ~p: ~p~n", [?MODULE, Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, #state{start_args = _Args} = _State) ->
  ?ERROR("~p terminating: ~p~n", [?MODULE, Reason]),
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: get_hostname () -> HostName
%% Description: Quick accessor to local node's hostname
%% TODO: Make a commandline-passable-option
%%--------------------------------------------------------------------
get_hostname() -> 
  {ok, "localhost"}.
  % inet:gethostname().


%%====================================================================
% Start thrift server (in erlang) 
%%====================================================================
% thrift_server
start_thrift_server(Args) ->
  ThriftPort  = proplists:get_value(proto_port, Args),
  Module      = proplists:get_value(module, Args),
  thrift_socket_server:start([
    {port, ThriftPort},
    {name, ambassador},
    {service, ambassador_thrift},
    {handler, Module},
    {socket_opts, [{recv_timeout, infinity}]}]),
  ok.

start_thrift_cloud_server(Args) ->  
  process_flag(trap_exit, true),
  StartCmd = build_start_command("start", Args),
  Port = open_port({spawn, StartCmd ++ " "}, ?PORT_OPTIONS),
  Port.
  % start_and_link_thrift_server(StartCmd).
  % case whereis(cloud_thrift_server) of
  %   undefined -> start_and_link_thrift_server(StartCmd);
  %   Node -> Node
  % end.  
  
% start_and_link_thrift_server(StartCmd) ->
%   ?INFO("Starting ~p: ~p~n", [?MODULE, StartCmd]),
%   case (fun() -> os:cmd(StartCmd) end) of
%     {error, Reason} ->
%       ?INFO("Assuming the thrift_client is already started error: ~p~n", [Reason]),
%       ok;
%     Pid ->
%       % case utils:is_process_alive(Pid) of
%       %   true -> 
%           % erlang:register(cloud_thrift_server, Pid),
%           % erlang:monitor(process, Pid),
%         % _ -> ok
%       % end,
%       Pid
%   end.
  
  
stop_thrift_client(Args) ->
  StopCmd = build_start_command("stop", Args),
  ?INFO("Stopping ~p: ~p~n", [?MODULE, StopCmd]),
  spawn_link(fun() -> os:cmd(StopCmd) end).

%%--------------------------------------------------------------------
%% Function: build_start_command (Action, Args) -> cloud thrift string
%% Description: build a start command
%%--------------------------------------------------------------------  
build_start_command(Action, Args) ->
  ThriftPort = proplists:get_value(proto_port, Args),
  CloudConfig = proplists:get_value(clouds_config, Args),
  % CloudName = proplists:get_value(cloud_name, Args),
  
  ExtraArgs = lists:append([
                      [" --port ", erlang:integer_to_list(ThriftPort)], 
                      [" -c ", CloudConfig]
                      % [" -n ", CloudName]
                    ]),
  
  StartCommand = lists:flatten(lists:append([["cloud thrift ", Action, " "], [ExtraArgs]])),
  StartCommand.

%%====================================================================
%% Query on the thrift server
%%====================================================================

cloud_query(P, Name, Meth, Args) ->
  Query = #cloudQuery{name=Name},
  case catch thrift_client:call(P, run_command, [Query, Meth, Args]) of % infinite timeout
    {'EXIT', R} -> 
      case R of
        {timeout, _} -> {error, timeout};
        E -> {error, E}
      end;
    E -> E
  end.
  

cloud_run(P, Name, Meth, Args) ->  
  Query = #cloudQuery{name=Name},
  ?INFO("Casting the command ~p through ~p~n", [Meth, ?MODULE]),
  case catch thrift_client:cast(P, run_command, [Query, Meth, Args]) of % infinite timeout
    {'EXIT', R} -> 
      ?ERROR("Got back an EXIT error: ~p~n", [R]),
      case R of
        {timeout, _} -> {error, timeout};
        E -> {error, E}
      end;
    E -> 
      ?INFO("Received ~p from cloud_run~n", [E]),
      E
  end.
  