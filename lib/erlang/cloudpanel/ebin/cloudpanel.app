{application, cloudpanel,
 [{description, "cloudpanel"},
  {vsn, "0.01"},
  {modules, [
    cloudpanel,
    cloudpanel_app,
    cloudpanel_sup,
    cloudpanel_web,
    cloudpanel_deps
  ]},
  {registered, []},
  {mod, {cloudpanel_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
