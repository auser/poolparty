-module (protobuffs_ambassador).

-export ([  
          start/1,
          get_pid/0,
          call/3,
          stop/0
         ]).

start(_) ->
  ok.
  
stop() ->
  ok.

call(Pid, Func, Msg) ->
  thrift_client:call(Pid, Func, Msg).
  
%%--------------------------------------------------------------------
%% Function: get_hostname () -> HostName
%% Description: Quick accessor to local node's hostname
%% TODO: Make a commandline-passable-option
%%--------------------------------------------------------------------
% get_hostname() -> inet:gethostname().

get_pid() -> erlang:whereis(ambassador).