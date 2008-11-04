-module (tail_log).
-compile(export_all).
-include ("cloudpanel.hrl").

start(File) ->
	start(File, fun display/1, "/var/log").

start(File, Callback) ->
  Dir = "/var/log",
  start(File, Callback, Dir).

start(File, Callback, Dir) ->
  spawn_link(?MODULE, init, [File, Callback, Dir]).

stop(Pid) ->
	Pid ! stop.

init(File, Callback, Dir) ->	
  Cmd = "/usr/bin/tail -f "++ Dir ++ "/" ++ File,
  Port = open_port({spawn, Cmd}, [ {cd, Dir}, stderr_to_stdout, {line, 256}, exit_status, binary]), 
	tail_loop(Port, Callback).

snap(Pid) ->	
	Pid ! {snap, self() },
	receive		
		{Port, Callback} ->
			display(element(2, {Port, Callback}));
		Any ->
			io:format("Received: ~p~n", [Any]),
			Any
	after ?TIMEOUT ->
		"Timeout error"
	end.

tail_loop(Port, Callback) ->
	receive
		{Port, {data, {eol, Bin}}} ->
			Callback(Bin),
			tail_loop(Port, Callback);
		{Port, {data, Bin}} ->
			Callback(Bin),
			tail_loop(Port, Callback);
		{snap, Who} ->
			Who ! {Port, Callback},
			tail_loop(Port, Callback);
		stop ->
			port_close(Port),
			{ok, stop};
		Any ->
			io:format("Received: ~p~n", [Any]),
			tail_loop(Port, Callback)
	end.

display(Bin) ->
  Content = iolist_to_binary(Bin),
  utils:subst("[INFO] ~s~n", [Content]).