-module (tailor).
-compile(export_all).
-import (mochijson2).
-include ("cloudpanel.hrl").

go(Qs) ->
	Body = case Qs of
		"log" ->
			latest_log(Qs);
		_ ->
			index(Qs)
	end,
	io:format("Body from go: ~p~n", [Body]),
	Body.

index(Qs) ->
	File = proplists:get_value("log", Qs),	
  % Tell the system that we are watching the logs
	self() ! {self(), subscribe},
	% Wait for messages	
	Dir = "/var/log",
	% Callback = fun(X) -> displayLogLine(X, Log) end,
	Cmd = "/usr/bin/tail -n 10 "++ Dir ++ "/" ++ File,
	Body = os:cmd(Cmd),
	string:join(string:tokens(Body, "\n"), "\n").

latest_log(Qs) ->
	ok.

start_logger(File) ->
	ok.