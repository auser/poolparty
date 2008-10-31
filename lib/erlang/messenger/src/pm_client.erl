-module (pm_client).
-include_lib("../include/defines.hrl").

-export ([init_conn/0, send_cmd/1, reconfigure_cloud/0, get_load/1, get_live_nodes/0]).
-export ([provision_orphan_running_servers/0]).
-export ([shutdown/0]).
% Run commands on the running master process
% erl -pa ./ebin/ -run pm_client get_load cpu -run init stop -noshell

% Connect to the master
init_conn() -> net_adm:ping(?MASTER_LOCATION).
% Send the command Cmd to the pm_master process
send_cmd(Cmd) ->	
	init_conn(),
	pm_master:run_cmd(Cmd).
% Reconfigure the cloud
reconfigure_cloud() -> 
	init_conn(),
	pm_master:reconfigure_cloud().
% Get the load on the cloud of type Type
get_load(Type) -> 
	init_conn(),
	pm_master:get_load(Type).
% Check to see if there are servers that are unprovisioned
% And if there are, log in to them and start their messenger
% sending the live code on the master to them
provision_orphan_running_servers() ->
	Instances = pm_cluster:any_new_servers(),
	case lists:flatlength(Instances) of
		0 ->
			ok;
		_ ->
			utils:distribute_modules_to([pm_node, pm_node_supervisor, pm_event_manager, node_app], Instances),
			pm_cluster:slaves(Instances),
			Instances
	end.

% Get a list of the live nodes
get_live_nodes() -> 
	init_conn(),
	pm_cluster:get_live_nodes().
% Terminate the cloud messenger
% This sends a shutdown to the whole cloud
shutdown() -> 
	init_conn(),
	pm_master:shutdown_cloud().