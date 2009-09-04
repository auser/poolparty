%%%-------------------------------------------------------------------
%%% File    : rest_server.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Jun 26 17:14:22 PDT 2009
%%%-------------------------------------------------------------------

-module (rest_server).
-behaviour(gen_server).
-include ("hermes.hrl").

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export ([print_banner/1]).
-record(state, {
        
        }).
-define(SERVER, ?MODULE).
-define(JSON_ENCODE(V), mochijson2:encode(V)).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).


stop(_Args) ->
  io:format("Stopping ~p~n", [?MODULE]),
  gen_server:call(?MODULE, stop),
  stop_mochiweb(),
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
% TODO: Update port args with config variables
init([Args]) ->
	print_banner(Args),
	StartArgs = get_start_args(Args),
	
  start_mochiweb(StartArgs),
  {ok, #state{}}.


print_banner(Args) ->
  io:format("~s~n~s~n~n",
            [?SOFTWARE_NAME, ?COPYRIGHT_MESSAGE]),
  [Port] = get_start_args(Args),
             
  Settings =  [
                {"node ", node()},
                {"port ", erlang:integer_to_list(Port)}
              ],
  DescrLen = lists:max([length(K) || {K, _V} <- Settings]),
  Format = "~-" ++ integer_to_list(DescrLen) ++ "s: ~s~n",
  lists:foreach(fun ({K, V}) -> io:format(Format, [K, V]) end, Settings),
	io:format("---------------------------------\n"),
  io:nl().  

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
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
  io:format("Terminating Rest client~n"),
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
start_mochiweb(Args) ->
  [Port] = Args,
  io:format("Starting mochiweb_http with ~p~n", [Port]),
  mochiweb_http:start([ {port, Port},
                        {loop, fun dispatch_requests/1}]).


stop_mochiweb() ->
  mochiweb_http:stop(),
  ok.
  
dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).
  
% Handle the requests
handle("/favicon.ico", Req) -> Req:respond({200, [{"Content-Type", "text/html"}], ""});

handle(Path, Req) ->
  CleanPath = clean_path(Path),
  CAtom = erlang:list_to_atom(top_level_request(CleanPath)),    
  ControllerPath = parse_controller_path(CleanPath),
  
  case CAtom of
    home -> 
      WebFile = lists:append([filename:absname("."), "/web/index.html"]),
      ?TRACE("WebFile: ", [WebFile]),
      IndexContents = case file:read_file(WebFile) of
        {ok, Contents} -> Contents;
        _ -> "
          <html><head>
            <title>Hermes - Messenger</title>
            <style type='text/css' media='screen'>
            body { margin: 20px 0 0 0;}
            h1 {color: red;}
            </style>
            </head><body>
          <h1>Error</h1>
          </body></html>
        "
      end,
      Req:ok({"text/html", IndexContents});
    assets -> Req:ok(assets:get(ControllerPath));
    ControllerAtom -> 
      Body = case Req:get(method) of
        'GET' -> ControllerAtom:get(ControllerPath);
        'POST' -> ControllerAtom:post(ControllerPath, decode_data_from_request(Req));
        'PUT' -> ControllerAtom:put(ControllerPath, decode_data_from_request(Req));
        'DELETE' -> ControllerAtom:delete(ControllerPath, decode_data_from_request(Req));
        Other -> subst("Other ~p on: ~s~n", [users, Other])
      end,
      JsonBody = jsonify(Body),
      Req:ok({"text/json", JsonBody})
  end.
    
% Get the data off the request
decode_data_from_request(Req) ->
  RecvBody = Req:recv_body(),
  Data = case RecvBody of
    <<>> -> erlang:list_to_binary("{}");
    Bin -> Bin
  end,
  {struct, Struct} = mochijson2:decode(Data),
  Struct.

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

% parse the controller path
parse_controller_path(CleanPath) ->
  case string:tokens(CleanPath, "/") of
    [] -> [];
    [_RootPath|Rest] -> Rest
  end.

% Get a clean path
% strips off the query string
clean_path(Path) ->
  case string:str(Path, "?") of
    0 -> Path;
    N -> string:substr(Path, 1, string:len(Path) - (N+1))
  end.

top_level_request(Path) ->
  case string:tokens(Path, "/") of
    [CleanPath|_Others] -> CleanPath;
    [] -> "home"
  end.

jsonify(JsonifiableBody) ->
  [ ?JSON_ENCODE({
        struct, [
          JsonifiableBody
        ]
    })
  ].
  
%%--------------------------------------------------------------------
%% Function: get_start_args (Args) -> [port]
%% Description: Get the start args from the application env
%%--------------------------------------------------------------------
get_start_args(Args) ->
  Module = case proplists:get_value(module, Args) of
    undefined -> hermes;
    Else -> Else
  end,
  lists:map(fun ({Var, Default}) -> 
  	  case application:get_env(Module, Var) of
        undefined -> Default;
  	    {ok, V} -> V
      end
	  end, [
	        {port, 9999}
	       ]).