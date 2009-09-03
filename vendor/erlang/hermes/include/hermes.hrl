-define(SOFTWARE_NAME, "------------- Hermes ------------").
-define(COPYRIGHT_MESSAGE, "Copyright (C) 2009 Ari Lerner, Nate Murray, Michael Fairchild, CloudTeam").

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).
-define (DEFAULT_AVERAGE_TIME, 30).

-define (DEFAULT_NAG_DELAY, 10000).
-define (LOCK_TIMOUT, 600).

-define (FMT_MSG (Msg, Args), lists:flatten([?MODULE, ?LINE, io_lib:format(Msg, Args)])).
-define (INFO (Msg, Args),    hermes_logger:info(Msg, Args)).
-define (ERROR (Msg, Args),   hermes_logger:error(Msg, Args)).

-define (DEBUG, true).
-define (TESTING, ?DEBUG).

-define (CONFIG_FILE, case ?TESTING of
  true -> "include/config.cfg";
  false -> "/etc/poolparty/hermes.cfg"
end).

-define (RRD_DIRECTORY, case ?TESTING of
  true -> "test/fixtures";
  false -> "/var/lib/collectd"
end).

-define (DEFAULT_CONFIG, [
          {port, 8642},
          {module, hermes},
          {proto_port, 11223},
          {log_path, "/var/log/hermes.log"},
          {rrd_directory, "/var/lib/collectd"},
          {cloud_name, "/etc/poolparty/cloud_name"},
          {clouds_config, "/etc/poolparty/clouds.rb"}
        ]). 

-define (LOG_MESSAGE (Message, Args), io_lib:fwrite("~p~p~n", [Message, Args])).

-define (TRACE(X, M), case ?DEBUG of
  true -> io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M]);
  false -> ok
end).
-define (DEBUG_LOG (Bool, Message, Opts), 
  case Bool of true -> 
    io:format(Message, Opts); 
  _ -> ok 
  end,
  ?LOG_MESSAGE(io_lib:fwrite(Message, Opts))).
  