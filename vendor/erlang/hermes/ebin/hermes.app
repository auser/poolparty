{application, hermes, [
        {description, "Hermes"},
        {vsn, "0.0.3"},
        {modules, [
                
                % hermes
                hermes,ambassador,ambassador_app,ambassador_sup,assets,
                athens,athens_srv,athens_app,athens_sup,
                cluster,commandInterface_thrift,config,erlrrd,erlrrd_app,erlrrd_sup,
                hermes_logger,hermes_sup,home,loudmouth,make_boot,mapreduce,mon_server,
                mon_server_sup,monitors,nag,nag_app,nag_sup,poolparty_types,protobuffs_ambassador,
                rest_app,rest_server,rest_server_sup,testing,thrift_ambassador,utils,
                httpd_util,

                % gen_cluster
                gen_cluster,

                % mochiweb
                mochifmt,mochifmt_records,mochifmt_std,mochihex,mochijson,mochijson2,mochinum,
                mochiweb,mochiweb_app,mochiweb_charref,mochiweb_cookies,mochiweb_echo,
                mochiweb_headers,mochiweb_html,mochiweb_http,mochiweb_multipart,
                mochiweb_request,mochiweb_response,mochiweb_skel,mochiweb_socket_server,
                mochiweb_sup,mochiweb_util,

                % thrift
                thrift_app,thrift_base64_transport,thrift_binary_protocol,
                thrift_buffered_transport,thrift_client,thrift_disk_log_transport,
                thrift_file_transport,thrift_framed_transport,thrift_http_transport,
                thrift_processor,thrift_protocol,thrift_server,thrift_service,
                thrift_socket_server,thrift_socket_transport,thrift_sup,thrift_transport,

                % hermes thrift
                hermes_thrift, hermes_types

            ]},
        {env, [
          {port, 8642},
          {monitors, [cpu, memory]},
          {clouds_config, undefined},
          {cloud_name, undefined},
          {log_path, undefined},
          {no_nag, false},
          {no_ambassador, false}
        ]},

        {registered, [hermes]},
        {applications, [kernel, stdlib, sasl]},
        {included_applications, [stoplight]},
        {start_phases, [{go,[]}]},
        {mod, {application_starter,[hermes,[]]}}
]}.
