{application, client,
  [
		% Quick description of the server
		{description, "Client Server for PoolParty"},
		% Version
		{vsn, "0.1"},
		% All modules used by the application.  
		{modules, [client_app, pm_client, client_server, pm_client_supervisor, utils]},
		% All the registered names in the application
		{registered, [client_server, pm_client_supervisor]},
		% These must be started for application to run
		{applications, [kernel, stdlib]},
		% Environment vars
		{env, []},
		% Module and Args used to start
		{mod, {client_app, []}},
		{start_phases, []}
	]
}.