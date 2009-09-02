-module (monitors).
-include ("hermes.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get([]) ->
  Monitors = mon_server:list_monitors(),
  MonitorData = lists:map(
    fun(Monitor) ->
      case Monitor of
        unknown_monitor -> unknown_monitor;
        _Else ->
          O = handle_get_monitor_over_time(Monitor, 600),
          {struct, O}
      end
    end,
    Monitors),
  {?MODULE, MonitorData};

get(["list"]) ->
  Monitors = mon_server:list_monitors(),
  JsonMonitors = lists:map(fun({Mon, Types}) ->
      {Mon, lists:map(fun(T) -> utils:turn_binary(T) end, Types)}
    end, Monitors),
    
  {?MODULE, {struct, JsonMonitors}};

get([SuperMonitor]) ->
  % Monitors = mon_server:list_monitors(),
  % SuperMonitors = proplists:get_value(erlang:list_to_atom(SuperMonitor), Monitors),
  % ?TRACE("Monitor", [SuperMonitor, Monitors, SuperMonitors]),
  % Vals = handle_get_monitor_over_time({SuperMonitor, SuperMonitors}, 600),
  % {SuperMonitor, {struct, Vals}};
  RelatedMonitors = mon_server:list_related_monitors(erlang:list_to_atom(SuperMonitor)),
  % ?TRACE("RelatedMonitors", [RelatedMonitors]),
  Regexp = lists:append(["(.*)?", SuperMonitor, "(.*)?"]),
  Mons = lists:filter(fun(AtomModule) ->
    % ?TRACE("Regexp: ", [erlang:atom_to_list(AtomModule), Regexp, regexp:match(erlang:atom_to_list(AtomModule), Regexp)]),
    case regexp:match(erlang:atom_to_list(AtomModule), Regexp) of
      {match, _, _} -> true;
      nomatch -> false
    end
  end, RelatedMonitors),
  Vals = handle_get_monitor_over_time({SuperMonitor, Mons}, 600),
  {SuperMonitor, {struct, Vals}};

get([SuperMonitor, Monitor]) when is_list(Monitor) -> ?MODULE:get([SuperMonitor, Monitor, "600"]);
 
get([SuperMonitor, Monitor, Time]) when is_list(Monitor) ->
  RelatedMonitors = mon_server:list_related_monitors(utils:turn_to_atom(SuperMonitor), utils:turn_to_atom(Monitor)),
  Vals = handle_get_monitor_over_time(RelatedMonitors, erlang:list_to_integer(Time)),
  {SuperMonitor, {struct, Vals}};
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(_Path, _Data) -> {"error", <<"unhandled">>}.

%%====================================================================
%% Private methods
%%====================================================================
change_to_float("nan")  -> 0;
change_to_float([])     -> 0.0;
change_to_float(Int)    -> Int.

handle_get_monitor_over_time(MonitorAtom, Time) ->  
  Vals = mon_server:get_average_over(MonitorAtom, Time),
  PrintableVals = lists:map(fun(V) ->
      {A, ListOfAtoms} = V,
      O = lists:map(fun({T, B}) -> 
          {T, change_to_float(B)}
        end, ListOfAtoms),
      {A, [{struct, O}]}
    end, Vals),
  PrintableVals.