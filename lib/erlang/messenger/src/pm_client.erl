-module (pm_client).
-export ([init/0, send_cmd/1, reconfigure_cloud/0, get_load/1, get_live_nodes/0]).
-export ([shutdown/0]).
% Run commands on the running master process
% erl -pa ./ebin/ -run pm_client get_load cpu -run init stop -noshell

% Connect to the master
init() -> net_adm:ping(master).
% Send the command Cmd to the pm_master process
send_cmd(Cmd) ->	pm_master:fire_cmd(Cmd).
% Reconfigure the cloud
reconfigure_cloud() -> pm_master:reconfigure_cloud().
% Get the load on the cloud of type Type
get_load(Type) -> pm_master:get_load(Type).
% Get a list of the live nodes
get_live_nodes() -> pm_master:get_live_nodes().
% Terminate the cloud messenger
% This sends a shutdown to the whole cloud
shutdown() -> pm_master:shutdown_cloud().