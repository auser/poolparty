%%%-------------------------------------------------------------------
%%% File    : mon_server.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Jul 29 19:28:46 PDT 2009
%%%-------------------------------------------------------------------

-module (mon_server).
-include ("hermes.hrl").
-behaviour(gen_cluster).

%% API
-export([start_link/1, stop/0]).

-export ([  
            get_average/1, 
            get_average_over/2,
            get_latest_average_for/1,get_latest_average_for/2,
            list_monitors/0, list_all_monitor_files/0,
            list_related_monitors/1, list_related_monitors/2,
            get_monitors/0,
            is_known_monitor/1
         ]).
% Aggregates
-export ([get_all_averages/0, get_all_averages/1, get_all_averages/2,
          cluster_average/0, cluster_average/1, cluster_average/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% gen_cluster callback
-export([handle_join/3, handle_node_joined/3, handle_leave/4]).

-record(state, {}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
  gen_cluster:start_link({local, ?SERVER}, ?MODULE, [Args], []).

stop() -> gen_cluster:call(?SERVER, stop).

%%--------------------------------------------------------------------
%% Function: get_latest_average_for (Monitor) -> float
%% Description: Get the latest average for the 
%%  monitor given (60 seconds)
%%--------------------------------------------------------------------
get_latest_average_for(Monitor) -> get_latest_average_for(Monitor, ?DEFAULT_AVERAGE_TIME).
get_latest_average_for(Monitor, Interval) ->
  Avg = mon_server:get_average_over(Monitor, Interval),
  % Toss out the top 2 values
  [_|SecondMostAverage] = Avg,
  [_|ThirdMostAverage] = SecondMostAverage,
  [LastTuple|_] = ThirdMostAverage,
  
  {_Timestamp, Float} = LastTuple,
  Float.

%%--------------------------------------------------------------------
%% Function: get_average (Module) -> {ok, Fetched}
%% Description: Get the average of a specific module
%%--------------------------------------------------------------------
get_average(Module) -> gen_cluster:call(?MODULE, {get_average, Module}).

%%--------------------------------------------------------------------
%% Function: get_all_averages () -> {[Averages]}
%% Description: Get all the averages for every monitor
%%--------------------------------------------------------------------
get_all_averages() -> gen_cluster:call(?SERVER, {get_all_averages}).
get_all_averages(Time) -> gen_cluster:call(?SERVER, {get_all_averages, Time}).
get_all_averages(Monitors, Time) -> gen_cluster:call(?SERVER, {get_all_averages, Monitors, Time}).
  
%%--------------------------------------------------------------------
%% Function: get_average_over (Module, Seconds) -> {ok, Fetched}
%% Description: Get the average of a specific module over Seconds
%%--------------------------------------------------------------------
get_average_over(Module, Seconds) -> gen_cluster:call(?MODULE, {get_average, Module, Seconds}).

%%--------------------------------------------------------------------
%% Function: list_monitors () -> {ok, List}
%% Description: Get the list of monitors
%%--------------------------------------------------------------------
list_monitors() -> gen_cluster:call(?MODULE, {list_monitors}).

list_related_monitors(SuperMonitor) -> gen_cluster:call(?MODULE, {list_related_monitors, SuperMonitor}).
list_related_monitors(SuperMonitor, SubType) -> gen_cluster:call(?MODULE, {list_related_monitors, SuperMonitor, SubType}).

list_all_monitor_files() -> gen_cluster:call(?MODULE, {list_all_monitor_files}).


%%====================================================================
%% CLUSTERS
%%====================================================================
cluster_average() ->
  Monitors = get_monitors(),
  cluster_average(Monitors, 300).
  
cluster_average(Time) ->
  Monitors = get_monitors(),
  cluster_average(Monitors, Time).

cluster_average(Monitors, Time) ->
  {ok, NodePids} = gen_cluster:plist(?SERVER),
  lists:map(fun(Pid) -> 
    MonitorOutput = rpc:call(node(Pid), ?MODULE, get_all_averages, [Monitors, Time]),
    {node(Pid), MonitorOutput}
  end, NodePids).


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
init(_Args) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({list_monitors}, _From, State) ->
  {reply, get_monitors(), State};
  
handle_call({list_related_monitors, SuperMonitor}, _From, State) -> {reply, get_related_monitors(SuperMonitor), State};
handle_call({list_related_monitors, SuperMonitor, SubType}, _From, State) -> {reply, get_related_monitors(SuperMonitor, SubType), State};

handle_call({list_all_monitor_files}, _From, State)       -> {reply, get_all_monitor_files(), State};

handle_call({get_average, Module, Seconds}, _From, State) -> {reply, handle_get_average(Module, Seconds), State};
handle_call({get_average, Module}, _From, State)          -> {reply, handle_get_average(Module, 60), State};
  
handle_call({get_all_averages, Monitors, Time}, _From, State) ->  {reply, lists:map(fun(Mon) -> handle_get_average(Mon, Time) end, Monitors), State};  

handle_call(stop, _From, State)       -> {stop, normal, stopped, State};
handle_call(_Request, _From, State)   -> {reply, ok, State}.

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
    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_get_average (Module, Seconds) -> {ok, Averages}
%% Description: Handle the fetching of averages for rrd
%%              over the last Seconds
%%--------------------------------------------------------------------
handle_get_average({_SuperMon, SubMonitors}, OverTime) ->
  lists:map(fun(SubMon) -> {SubMon, handle_get_average(utils:turn_to_atom(SubMon), OverTime)} end, SubMonitors);

handle_get_average(Monitors, OverTime) when is_list(Monitors) ->
  lists:map(fun(SubMon) -> {SubMon, handle_get_average(SubMon, OverTime)} end, Monitors);

handle_get_average(Module, OverTime) ->
  case is_known_monitor(Module) of
    false -> {unknown_monitor, []};
    true ->
      {Mega, Secs, _} = now(),
      StartTime = Mega*1000000 + Secs - OverTime,

      % Method
      M = lists:append([
        get_monitor_subtype_file(Module), % File Module.rrd
        " AVERAGE ",
        " --start ", erlang:integer_to_list(StartTime),
        " --end ", erlang:integer_to_list(StartTime + OverTime)
      ]),
      
      {ok, Fetched} = erlrrd:fetch(M),
      O = parse_rrd_return(Fetched),
      O
  end.

%%--------------------------------------------------------------------
%% Function: parse_rrd_return (Arr) -> {ok, Parsed}
%% Description: Take the output from erlrrd and turn it into something usable
% [["                            cpu"],
%  [[]],
%  ["1249284300: 5.0000000000e-01"]]
%%--------------------------------------------------------------------
parse_rrd_return(Arr) -> parse_rrd_return_1(Arr).

parse_rrd_return_1([[_Desc]|Rest]) ->
  % Module = erlang:list_to_atom(string:strip(Desc)),
  [_|ArrOfValues] = Rest,
  Values = lists:map(fun([Line]) -> collect_rrd_values(Line) end, ArrOfValues),
  lists:reverse(Values).
 
collect_rrd_values([]) -> {};
collect_rrd_values(Str) ->
  [Time|[V]] = string:tokens(Str, ":"),
  Val = case string:strip(V) of
    "nan" -> 0.0;
    F -> utils:turn_to_float(F)
  end,
  {Time, Val}.

%%--------------------------------------------------------------------
%% Function: get_monitors (Args) -> {ok, Monitors}
%% Description: Get the monitors either in the known directory or at the
%% behest of the user on the command-line
%%--------------------------------------------------------------------
get_monitors() ->
  Monitors = lists:map(fun(MonString) -> {MonString, get_monitor_subtypes(MonString)} end, get_monitor_types()),
  Monitors.

get_monitor_types() ->
  case file:list_dir(utils:get_rrd_location()) of
    {error, Reason} -> 
      ?LOG_MESSAGE("Error getting monitor types", [Reason]),
      Reason;
    {ok, FileList} ->
      lists:map(
        fun(Filename) ->
          [Name|_] = string:tokens(Filename, "."),
          erlang:list_to_atom(Name)
        end, FileList)
  end.
  
get_monitor_subtypes(MonitorAtom) ->
  lists:map(fun(X) -> erlang:list_to_atom(filename:basename(filename:rootname(X, ".rrd"))) end, get_monitor_files(MonitorAtom)).

% Get the actual file here
get_monitor_subtype_file(MonitorAtom) when is_atom(MonitorAtom) -> get_monitor_subtype_file(utils:turn_to_list(MonitorAtom));
get_monitor_subtype_file(MonitorString) ->
  FileArray = filelib:wildcard( lists:append([utils:get_rrd_location(), "/*", "/", MonitorString, ".rrd"]) ),
  hd(FileArray).

% Get the files associated with this monitor
get_monitor_files(MonitorAtom) ->
  Directory = lists:append([utils:get_rrd_location(), "/", erlang:atom_to_list(MonitorAtom)]),
  RRdFiles = lists:filter(fun(X) -> not filelib:is_dir(X) end, filelib:wildcard( lists:append([Directory, "/*.rrd"]) )),
  RRdFiles.

%%--------------------------------------------------------------------
%% Function: get_all_monitor_files () -> ListOfFiles
%% Description: 
%%--------------------------------------------------------------------
get_all_monitor_files() ->
  AllMonitors = proplists:get_keys(get_monitors()),
  lists:flatten(
    lists:map(fun(SuperMonitor) ->
      get_monitor_subtypes(SuperMonitor)
    end, AllMonitors)
  ).

%%--------------------------------------------------------------------
%% Function: get_related_monitors (SuperMonitor) -> MonitorList
%% Description: 
%%-------------------------------------------------------------------- 
get_related_monitors(SuperMonitor) ->
  AllMonitors = get_monitors(),
  case proplists:get_value(SuperMonitor, AllMonitors) of
    undefined -> [];
    V -> V
  end.
  
get_related_monitors(SuperMonitor, SubType) ->
  RelatedMonitors = get_related_monitors(SuperMonitor),
  % ?TRACE("RelatedMonitors", [RelatedMonitors]),
  Regexp = lists:append(["(.)*", erlang:atom_to_list(SubType), "(.*)"]),
  lists:filter(fun(AtomModule) ->
    % ?TRACE("Regexp: ", [erlang:atom_to_list(AtomModule), Regexp, regexp:match(erlang:atom_to_list(AtomModule), Regexp)]),
    case regexp:match(erlang:atom_to_list(AtomModule), Regexp) of
      {match, _, _} -> true;
      nomatch -> false
    end
  end, RelatedMonitors).

%%--------------------------------------------------------------------
%% Function: is_known_monitor (Monitor, List) -> true/false
%%--------------------------------------------------------------------
is_known_monitor(Monitor) ->
  KnownMonitors = get_monitors(),
  is_known_monitor(Monitor, KnownMonitors).

% is_known_monitor("memory-free", [{memory, ["memory-used", "memory-free"]}, {cpu, ["memory-idle", "memory-used"]}])
is_known_monitor(Monitor, [{_AtomOfKnownMonitors, ListOfKnownMonitors}|Rest]) ->
  case is_known_monitor(Monitor, ListOfKnownMonitors) of
    true -> true;
    _ -> is_known_monitor(Monitor, Rest)
  end;

% is_known_monitor("memory-free", {memory, ["memory-used", "memory-free"]})
is_known_monitor(Monitor, {_AtomOfKnownMonitors, ListOfKnownMonitors}) ->  is_known_monitor(Monitor, ListOfKnownMonitors);

% is_known_monitor("memory-free", ["memory-used", "memory-free"])
is_known_monitor(Monitor, ListOfKnownMonitors) when is_list(ListOfKnownMonitors) -> lists:member(Monitor, ListOfKnownMonitors).
