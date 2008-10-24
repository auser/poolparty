{application, packager,
  [
		% Quick description of the server
		{description, "Package manager for PoolParty"},
		% Version
		{vsn, "0.1"},
		% All modules used by the application.  
		{modules, [packager_app, pm_packager, utils]},
		% All the registered names in the application
		{registered, [pm_packager]},
		% These must be started for application to run
		{applications, [kernel, stdlib]},
		% Environment vars
		{env, []},
		% Module and Args used to start
		{mod, {packager_app, []}},
		{start_phases, []}
	]
}.