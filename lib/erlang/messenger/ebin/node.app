{application, node,
  [
		% Quick description of the server
		{description, "Node Server for PoolParty"},
		% Version
		{vsn, "0.1"},
		% All modules used by the application.  
		{modules, [node_app, pm_node, pm_node_supervisor, utils, pm_event_manager]},
		% All the registered names in the application
		{registered, [pm_node, pm_node_supervisor]},
		% These must be started for application to run
		{applications, [kernel, stdlib]},
		% Environment vars
		{env, []},
		% Module and Args used to start
		{mod, {node_app, []}},
		{start_phases, []}
	]
}.