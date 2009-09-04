-module (cluster).
-include ("hermes.hrl").
-export ([get/1, post/2, put/2, delete/2]).

% /cluster
get([]) ->
  Monitors = mon_server:list_monitors(),
  JsonMonitors = lists:map(fun({Mon, Types}) ->
      {Mon, lists:map(fun(T) -> utils:turn_binary(T) end, Types)}
    end, Monitors),
  {?MODULE, {struct, 
    JsonMonitors
  }};
  % {?MODULE, {struct, [
  %   {"monitors", JsonMonitors }
  % ]}};

% /cluster/monitors
get(["monitors"]) ->
  Monitors = mon_server:list_monitors(),
  MonVals = mon_server:cluster_average(),
  
  [MonStructs] = get_monitor_info_from(MonVals),
  
  {?MODULE, {struct, [
      {"monitors", Monitors },
      {"stats", MonStructs}
    ]}
  };
  
  
% /cluster/monitors/1000
get(["monitors", Time]) ->
  Monitors = mon_server:list_monitors(),
  MonVals = mon_server:cluster_average(erlang:list_to_integer(Time)),

  [MonStructs] = get_monitor_info_from(MonVals),

  {?MODULE, {struct, [
      {"monitors", Monitors },
      {"stats", MonStructs}
    ]}
  };

   
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(_Path, _Data) -> {"error", <<"unhandled">>}.


%%====================================================================
%% PRIVATE
%%====================================================================

get_monitor_info_from(MonVals) ->
  lists:map(fun({Node, ValueSet}) -> 
      ValueData = lists:map(fun({Monitor, Vals}) ->
          PrintableVals = lists:map(fun(V) ->
              {A, Int} = V,
              {struct, [{A, utils:turn_binary(utils:change_to_float(Int))}]}
            end, Vals),
          {struct, [{Monitor, PrintableVals}]}
        end, ValueSet),
      {struct, [{Node, ValueData}]}
    end, MonVals).