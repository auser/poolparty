%%%-------------------------------------------------------------------
%%% File    : ambassador.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Aug 10 11:57:05 PDT 2009
%%%-------------------------------------------------------------------

-module (ambassador).
-behaviour(gen_server).
-include ("hermes.hrl").

%% API
-export([start_link/0]).
-export ([
          ask/1, ask/2,
          run/1, run/2,
          handle_function/2,
          get/1
          ]).
-export ([stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define (PROTO, thrift_ambassador).
-record(state, {
          start_args,
          cloud_name
        }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
ask(Fun)        -> gen_server:call(?MODULE, {ask, Fun}).
ask(Fun, Args)  -> gen_server:call(?MODULE, {ask, Fun, Args}).

run(Fun)        -> gen_server:cast(?MODULE, {run, Fun}).
run(Fun, Args)  -> gen_server:cast(?MODULE, {run, Fun, Args}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(_Args) ->
  ?PROTO:stop(),
  ok.


%%%%% PROTO INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_function(Function, Args) when is_atom(Function), is_list(Args) ->
  [A|_] = Args,
  handle_function(Function, A);
  
handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> {ok, "handled"};
        Reply -> {reply, Reply}
    end.

get(Key) ->
  ?INFO("Get ~p in ~p~n", [Key, ?MODULE]),
  {ok, <<"Nice">>}.
  


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
  Port = case config:get(proto_port) of
    {error, _} -> 11223;
    {ok, V} -> V
  end,
  
  CloudConfig = case application:get_env(hermes, clouds_config) of
    {ok, undefined} -> case config:get(clouds_config) of
      {error, _}    -> ensure_is_list("no_clouds_config");
      {ok, VC}      -> ensure_is_list(VC)
    end;
    {ok, CC} -> CC
  end,
  
  % cloud name COULD be a file
  CNameArg = case application:get_env(hermes, cloud_name) of
    {ok, undefined} -> case config:get(cloud_name) of
      {error, _}    -> ensure_is_list("no_cloud_name");
      {ok, VN}      -> ensure_is_list(VN)
    end;
    {ok, CName} -> CName
  end,
  
  CloudName = case file:read_file(string:strip(CNameArg)) of
    {ok, Binary} ->
      [Cnameout|_] = string:tokens(utils:turn_to_list(Binary), "\n"),
      ensure_is_list(Cnameout);
    {error, _} -> ensure_is_list(CNameArg)
  end,
  
  ProtoArgs = [
    {proto_port, Port}, 
    {clouds_config, CloudConfig}, 
    {cloud_name, CloudName}],
  
  ?INFO("STARTING ~p with ~p~n", [?MODULE, ProtoArgs]),
  ?PROTO:start_link(ProtoArgs),
  {ok, #state{
    start_args = ProtoArgs,
    cloud_name = CloudName
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
handle_call({ask, Function}, _From, #state{cloud_name = CloudName} = State) ->
  Out = ?PROTO:ask(CloudName, Function, ""),
  {reply, Out, State};
  
handle_call({ask, Function, Args}, _From, #state{cloud_name = CloudName} = State) ->
  FormattedArgs = format_args(Args),
  Out = ?PROTO:ask(CloudName, Function, [FormattedArgs]),
  {reply, Out, State};
      
handle_call({call, Function, Args}, _From, State) ->
  FormattedArgs = format_args(Args),
  ?INFO("Calling ~p(~p)~n", [Function, FormattedArgs]),
  Out = handle_function(Function, FormattedArgs),
  {reply, Out, State};
  
handle_call(_Request, _From, State) ->
  Reply = {ok, "Handled"},
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({run, Function}, #state{cloud_name = CloudName} = State) ->
  ?PROTO:run(CloudName, Function, ""),
  {noreply, State};
  
handle_cast({run, Function, Args}, #state{cloud_name = CloudName} = State) ->
  ?PROTO:run(CloudName, Function, [Args]),
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
terminate(Reason, _State) ->
  ?INFO("Terminating ambassador because: ~p~n", [Reason]),
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

ensure_is_list(Arg) ->
  case Arg of
    O when is_atom(O) -> erlang:atom_to_list(O);
    O -> O
  end.
  
format_args(Args) ->
  List = ensure_is_list(Args),  
  O = lists:map(fun(Ele) -> ensure_is_list(Ele) end, List),
  O.