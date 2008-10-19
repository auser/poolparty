{application, load_app,
  [
		% Quick description of the server
		{description, "Load Server"},
		% Version
		{vsn, "0.1"},
		% All modules used by the application.  
		{modules, [load_supervisor, load_server, load_app, load_client]},
		% All the registered names in the application
		{registered, [load_server]},
		% These must be started for application to run
		{applications, [kernel, stdlib]},
		% Environment vars
		{env, []},
		% Module and Args used to start
		{mod, {load_app, []}}
	]
}.