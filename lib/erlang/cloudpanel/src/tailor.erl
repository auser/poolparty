-module (tailor).
-compile(export_all).
-import (mochijson2).
-include ("cloudpanel.hrl").

go(Qs) ->
	Body = case Qs of
		_ ->
			index(Qs)
	end,
	loggers([]),
	views:wrap(Body).

index(Qs) ->
	File = proplists:get_value("log", Qs),
	case File of
		undefined -> Room = get_the_log("syslog");
		_ -> Room = get_the_log(File)
	end,
	Room ! {self(), subscribe},
	receive
		Message -> {_, Message} = {ok, Message}
		after ?TIMEOUT ->
			{_, Message} = {error, "Timeout"}
	end,
	Message.
	
loggers() ->
	loggers([]).
	
loggers(Logs) ->
	io:format("Loggers: ~p~n", [Logs]),
	receive
		{From, post, Message} ->
			From ! posted,
			lists:foreach(fun(Log) -> Log ! Message end, Logs),
			loggers(Logs);
		{From, subscribe} ->
			From ! subscribe,
			loggers(Logs);
		_Any ->
			loggers(Logs)
	end.

	
get_the_log(Name) ->
	Pid = whereis(thelog),
	if
		is_pid(Pid) -> Pid;
		true ->
			Callback = fun(X) -> displayLogLine(X, Name) end,			
			NewPid = tail_log:start(Name, Callback),
			register(thelog, NewPid),
			NewPid
	end.

%% Internal API
displayLogLine(X, Name) ->
	Room = get_the_log(Name),
	Room ! {self(), post, X},
  receive
      posted ->
          % posted
          Body = {ok, <<"posted">>}
  after 1000 ->
      % something went wrong
      Body = {error, <<"timeout">>}
  end,
	Body.