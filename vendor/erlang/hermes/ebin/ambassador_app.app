{application, ambassador_app, [
        {description, "Ambassador server"},
        {vsn, "0.1"},
        {modules, [ambassador, ambassador_app, ambassador_sup]},
        {env, [
          {thrift_port, 11223}
        ]},
        {registered, [ambassador_app]},
        {applications, [kernel, stdlib]},
        {mod, {ambassador_app, []}}
]}.

