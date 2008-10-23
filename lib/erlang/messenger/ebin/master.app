{application, master,
  [
		% Quick description of the server
		{description, "Master Server for PoolParty"},
		% Version
		{vsn, "0.1"},
		% All modules used by the application.  
		{modules, [master_app, pm_master, pm_master_supervisor, utils]},
		% All the registered names in the application
		{registered, [pm_master, pm_master_supervisor]},
		% These must be started for application to run
		{applications, [kernel, stdlib]},
		% Environment vars
		{env, []},
		% Module and Args used to start
		{mod, {master_app, []}},
		{start_phases, []}
	]
}.