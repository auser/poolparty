{application, erlrrd, [
        {description, "Erlrrd server"},
        {vsn, "0.1"},
        {modules, [erlrrd]},
        {env, [
          {erlrrd, "rrdtools"}
        ]},
        {registered, [erlrrd]},
        {applications, [kernel, stdlib]},
        {mod, {erlrrd, []}}
]}.

