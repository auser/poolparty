--- !ruby/object:Gem::Specification 
name: poolparty
version: !ruby/object:Gem::Version 
  version: 0.2.77
platform: ruby
authors: 
- Ari Lerner
autorequire: 
bindir: bin
cert_chain: []

date: 2008-12-05 00:00:00 -08:00
default_executable: 
dependencies: 
- !ruby/object:Gem::Dependency 
  name: activesupport
  type: :runtime
  version_requirement: 
  version_requirements: !ruby/object:Gem::Requirement 
    requirements: 
    - - ">="
      - !ruby/object:Gem::Version 
        version: "0"
    version: 
- !ruby/object:Gem::Dependency 
  name: logging
  type: :runtime
  version_requirement: 
  version_requirements: !ruby/object:Gem::Requirement 
    requirements: 
    - - ">="
      - !ruby/object:Gem::Version 
        version: "0"
    version: 
- !ruby/object:Gem::Dependency 
  name: ruby2ruby
  type: :runtime
  version_requirement: 
  version_requirements: !ruby/object:Gem::Requirement 
    requirements: 
    - - ">="
      - !ruby/object:Gem::Version 
        version: "0"
    version: 
- !ruby/object:Gem::Dependency 
  name: hoe
  type: :runtime
  version_requirement: 
  version_requirements: !ruby/object:Gem::Requirement 
    requirements: 
    - - ">="
      - !ruby/object:Gem::Version 
        version: 1.8.2
    version: 
description: Self-healing, auto-scaling system administration, provisioning and maintaining tool that makes cloud computing fun and easy
email: 
- ari.lerner@citrusbyte.com
executables: 
- cloud
- cloud-add-keypair
- cloud-configure
- cloud-contract
- cloud-ensure-provisioning
- cloud-expand
- cloud-handle-load
- cloud-list
- cloud-maintain
- cloud-osxcopy
- cloud-provision
- cloud-refresh
- cloud-run
- cloud-ssh
- cloud-start
- cloud-stats
- cloud-terminate
- messenger-get-current-nodes
- pool
- pool-console
- pool-describe
- pool-generate
- pool-init
- pool-list
- pool-start
- server-build-messenger
- server-fire-cmd
- server-get-load
- server-list-active
- server-list-responding
- server-rerun
- server-send-command
- server-show-stats
- server-start-client
- server-start-master
- server-start-node
- server-stop-client
- server-stop-master
- server-stop-node
- server-update-hosts
extensions: []

extra_rdoc_files: 
- History.txt
- License.txt
- Manifest.txt
- PostInstall.txt
- README.txt
- lib/erlang/messenger/lib/eunit/examples/tests.txt
- lib/poolparty/config/postlaunchmessage.txt
- website/index.txt
files: 
- History.txt
- License.txt
- Manifest.txt
- PostInstall.txt
- README.txt
- Rakefile
- bin/cloud
- bin/cloud-add-keypair
- bin/cloud-configure
- bin/cloud-contract
- bin/cloud-ensure-provisioning
- bin/cloud-expand
- bin/cloud-handle-load
- bin/cloud-list
- bin/cloud-maintain
- bin/cloud-osxcopy
- bin/cloud-provision
- bin/cloud-refresh
- bin/cloud-run
- bin/cloud-ssh
- bin/cloud-start
- bin/cloud-stats
- bin/cloud-terminate
- bin/messenger-get-current-nodes
- bin/pool
- bin/pool-console
- bin/pool-describe
- bin/pool-generate
- bin/pool-init
- bin/pool-list
- bin/pool-start
- bin/server-build-messenger
- bin/server-fire-cmd
- bin/server-get-load
- bin/server-list-active
- bin/server-list-responding
- bin/server-rerun
- bin/server-send-command
- bin/server-show-stats
- bin/server-start-client
- bin/server-start-master
- bin/server-start-node
- bin/server-stop-client
- bin/server-stop-master
- bin/server-stop-node
- bin/server-update-hosts
- config/hoe.rb
- config/requirements.rb
- examples/basic.rb
- examples/plugin_without_plugin_directory.rb
- examples/poolparty.rb
- generators/poolspec/USAGE
- generators/poolspec/poolspec_generator.rb
- generators/poolspec/templates/pool_spec_template.erb
- lib/erlang/messenger/Emakefile
- lib/erlang/messenger/Makefile
- lib/erlang/messenger/README
- lib/erlang/messenger/Rakefile
- lib/erlang/messenger/control
- lib/erlang/messenger/ebin/client.app
- lib/erlang/messenger/ebin/client_app.beam
- lib/erlang/messenger/ebin/client_server.beam
- lib/erlang/messenger/ebin/erl_crash.dump
- lib/erlang/messenger/ebin/master.app
- lib/erlang/messenger/ebin/master_app.beam
- lib/erlang/messenger/ebin/node.app
- lib/erlang/messenger/ebin/node_app.beam
- lib/erlang/messenger/ebin/packager.app
- lib/erlang/messenger/ebin/pm_client.beam
- lib/erlang/messenger/ebin/pm_client_old.beam
- lib/erlang/messenger/ebin/pm_client_rel-0.1.rel
- lib/erlang/messenger/ebin/pm_client_supervisor.beam
- lib/erlang/messenger/ebin/pm_cluster.beam
- lib/erlang/messenger/ebin/pm_event_manager.beam
- lib/erlang/messenger/ebin/pm_master.beam
- lib/erlang/messenger/ebin/pm_master_event_handler.beam
- lib/erlang/messenger/ebin/pm_master_rel-0.1.rel
- lib/erlang/messenger/ebin/pm_master_supervisor.beam
- lib/erlang/messenger/ebin/pm_node.beam
- lib/erlang/messenger/ebin/pm_node_rel-0.1.rel
- lib/erlang/messenger/ebin/pm_node_supervisor.beam
- lib/erlang/messenger/ebin/pm_packager.beam
- lib/erlang/messenger/ebin/pm_strings.beam
- lib/erlang/messenger/ebin/utils.beam
- lib/erlang/messenger/include/defines.hrl
- lib/erlang/messenger/lib/eunit/AUTHORS
- lib/erlang/messenger/lib/eunit/CHANGELOG
- lib/erlang/messenger/lib/eunit/COPYING
- lib/erlang/messenger/lib/eunit/Makefile
- lib/erlang/messenger/lib/eunit/NOTES
- lib/erlang/messenger/lib/eunit/README
- lib/erlang/messenger/lib/eunit/doc/edoc-info
- lib/erlang/messenger/lib/eunit/doc/erlang.png
- lib/erlang/messenger/lib/eunit/doc/eunit.html
- lib/erlang/messenger/lib/eunit/doc/index.html
- lib/erlang/messenger/lib/eunit/doc/modules-frame.html
- lib/erlang/messenger/lib/eunit/doc/overview-summary.html
- lib/erlang/messenger/lib/eunit/doc/overview.edoc
- lib/erlang/messenger/lib/eunit/doc/packages-frame.html
- lib/erlang/messenger/lib/eunit/doc/stylesheet.css
- lib/erlang/messenger/lib/eunit/ebin/autoload.beam
- lib/erlang/messenger/lib/eunit/ebin/code_monitor.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit.app
- lib/erlang/messenger/lib/eunit/ebin/eunit.appup
- lib/erlang/messenger/lib/eunit/ebin/eunit.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_autoexport.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_data.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_lib.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_proc.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_serial.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_server.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_striptests.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_test.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_tests.beam
- lib/erlang/messenger/lib/eunit/ebin/eunit_tty.beam
- lib/erlang/messenger/lib/eunit/ebin/file_monitor.beam
- lib/erlang/messenger/lib/eunit/examples/eunit_examples.erl
- lib/erlang/messenger/lib/eunit/examples/fib.erl
- lib/erlang/messenger/lib/eunit/examples/tests.txt
- lib/erlang/messenger/lib/eunit/include/eunit.hrl
- lib/erlang/messenger/lib/eunit/src/Makefile
- lib/erlang/messenger/lib/eunit/src/autoload.erl
- lib/erlang/messenger/lib/eunit/src/code_monitor.erl
- lib/erlang/messenger/lib/eunit/src/eunit.app.src
- lib/erlang/messenger/lib/eunit/src/eunit.appup.src
- lib/erlang/messenger/lib/eunit/src/eunit.erl
- lib/erlang/messenger/lib/eunit/src/eunit_autoexport.erl
- lib/erlang/messenger/lib/eunit/src/eunit_data.erl
- lib/erlang/messenger/lib/eunit/src/eunit_internal.hrl
- lib/erlang/messenger/lib/eunit/src/eunit_lib.erl
- lib/erlang/messenger/lib/eunit/src/eunit_proc.erl
- lib/erlang/messenger/lib/eunit/src/eunit_serial.erl
- lib/erlang/messenger/lib/eunit/src/eunit_server.erl
- lib/erlang/messenger/lib/eunit/src/eunit_striptests.erl
- lib/erlang/messenger/lib/eunit/src/eunit_test.erl
- lib/erlang/messenger/lib/eunit/src/eunit_tests.erl
- lib/erlang/messenger/lib/eunit/src/eunit_tty.erl
- lib/erlang/messenger/lib/eunit/src/file_monitor.erl
- lib/erlang/messenger/lib/eunit/sys.config
- lib/erlang/messenger/lib/eunit/vsn.mk
- lib/erlang/messenger/pm_client_rel-0.1.boot
- lib/erlang/messenger/pm_client_rel-0.1.script
- lib/erlang/messenger/pm_master_rel-0.1.boot
- lib/erlang/messenger/pm_master_rel-0.1.script
- lib/erlang/messenger/pm_node_rel-0.1.boot
- lib/erlang/messenger/pm_node_rel-0.1.script
- lib/erlang/messenger/src/client_app.erl
- lib/erlang/messenger/src/client_server.erl
- lib/erlang/messenger/src/master_app.erl
- lib/erlang/messenger/src/node_app.erl
- lib/erlang/messenger/src/pm_client.erl
- lib/erlang/messenger/src/pm_client_old.erl
- lib/erlang/messenger/src/pm_client_supervisor.erl
- lib/erlang/messenger/src/pm_cluster.erl
- lib/erlang/messenger/src/pm_event_manager.erl
- lib/erlang/messenger/src/pm_master.erl
- lib/erlang/messenger/src/pm_master_event_handler.erl
- lib/erlang/messenger/src/pm_master_supervisor.erl
- lib/erlang/messenger/src/pm_node.erl
- lib/erlang/messenger/src/pm_node_supervisor.erl
- lib/erlang/messenger/src/pm_packager.erl
- lib/erlang/messenger/src/pm_strings.erl
- lib/erlang/messenger/src/utils.erl
- lib/erlang/messenger/useful_snippets
- lib/poolparty.rb
- lib/poolparty/aska/aska.rb
- lib/poolparty/base_packages/haproxy.rb
- lib/poolparty/base_packages/heartbeat.rb
- lib/poolparty/base_packages/poolparty.rb
- lib/poolparty/base_packages/ruby.rb
- lib/poolparty/base_packages/runit.rb
- lib/poolparty/config/postlaunchmessage.txt
- lib/poolparty/core/array.rb
- lib/poolparty/core/class.rb
- lib/poolparty/core/exception.rb
- lib/poolparty/core/float.rb
- lib/poolparty/core/hash.rb
- lib/poolparty/core/kernel.rb
- lib/poolparty/core/metaid.rb
- lib/poolparty/core/module.rb
- lib/poolparty/core/my_open_struct.rb
- lib/poolparty/core/object.rb
- lib/poolparty/core/proc.rb
- lib/poolparty/core/string.rb
- lib/poolparty/core/symbol.rb
- lib/poolparty/core/time.rb
- lib/poolparty/dependency_resolutions/base.rb
- lib/poolparty/dependency_resolutions/puppet.rb
- lib/poolparty/exceptions/CloudNotFoundException.rb
- lib/poolparty/exceptions/LoadRulesException.rb
- lib/poolparty/exceptions/MasterException.rb
- lib/poolparty/exceptions/RemoteException.rb
- lib/poolparty/exceptions/ResourceException.rb
- lib/poolparty/exceptions/RuntimeException.rb
- lib/poolparty/exceptions/SpecException.rb
- lib/poolparty/exceptions/TemplateNotFound.rb
- lib/poolparty/exceptions/UnacceptableCommand.rb
- lib/poolparty/helpers/binary.rb
- lib/poolparty/helpers/console.rb
- lib/poolparty/helpers/display.rb
- lib/poolparty/helpers/nice_printer.rb
- lib/poolparty/helpers/optioner.rb
- lib/poolparty/helpers/provisioner_base.rb
- lib/poolparty/helpers/provisioners/master.rb
- lib/poolparty/helpers/provisioners/slave.rb
- lib/poolparty/modules/cloud_dsl.rb
- lib/poolparty/modules/cloud_resourcer.rb
- lib/poolparty/modules/configurable.rb
- lib/poolparty/modules/definable_resource.rb
- lib/poolparty/modules/file_writer.rb
- lib/poolparty/modules/method_missing_sugar.rb
- lib/poolparty/modules/output.rb
- lib/poolparty/modules/pretty_printer.rb
- lib/poolparty/modules/resourcing_dsl.rb
- lib/poolparty/modules/s3_string.rb
- lib/poolparty/modules/safe_instance.rb
- lib/poolparty/modules/thread_pool.rb
- lib/poolparty/monitors/base_monitor.rb
- lib/poolparty/monitors/monitors/cpu_monitor.rb
- lib/poolparty/monitors/monitors/memory_monitor.rb
- lib/poolparty/monitors/monitors/web_monitor.rb
- lib/poolparty/net/messenger.rb
- lib/poolparty/net/remote.rb
- lib/poolparty/net/remote_bases/ec2.rb
- lib/poolparty/net/remote_bases/ec2/ec2_response_object.rb
- lib/poolparty/net/remote_instance.rb
- lib/poolparty/net/remoter.rb
- lib/poolparty/net/remoter_base.rb
- lib/poolparty/plugins/deploydirectory.rb
- lib/poolparty/plugins/git.rb
- lib/poolparty/plugins/line.rb
- lib/poolparty/plugins/rsyncmirror.rb
- lib/poolparty/plugins/runit.rb
- lib/poolparty/plugins/svn.rb
- lib/poolparty/poolparty/base.rb
- lib/poolparty/poolparty/cloud.rb
- lib/poolparty/poolparty/custom_resource.rb
- lib/poolparty/poolparty/loggable.rb
- lib/poolparty/poolparty/plugin.rb
- lib/poolparty/poolparty/plugin_model.rb
- lib/poolparty/poolparty/pool.rb
- lib/poolparty/poolparty/resource.rb
- lib/poolparty/poolparty/resources/class_package.rb
- lib/poolparty/poolparty/resources/conditional.rb
- lib/poolparty/poolparty/resources/cron.rb
- lib/poolparty/poolparty/resources/custom_service.rb
- lib/poolparty/poolparty/resources/directory.rb
- lib/poolparty/poolparty/resources/exec.rb
- lib/poolparty/poolparty/resources/file.rb
- lib/poolparty/poolparty/resources/gem_package.rb
- lib/poolparty/poolparty/resources/host.rb
- lib/poolparty/poolparty/resources/mount.rb
- lib/poolparty/poolparty/resources/package.rb
- lib/poolparty/poolparty/resources/remote_file.rb
- lib/poolparty/poolparty/resources/service.rb
- lib/poolparty/poolparty/resources/sshkey.rb
- lib/poolparty/poolparty/resources/symlink.rb
- lib/poolparty/poolparty/resources/variable.rb
- lib/poolparty/poolparty/script.rb
- lib/poolparty/spec/core/string.rb
- lib/poolparty/spec/matchers/a_spec_extensions_base.rb
- lib/poolparty/spec/matchers/have_cron.rb
- lib/poolparty/spec/matchers/have_deploydirectory.rb
- lib/poolparty/spec/matchers/have_directory.rb
- lib/poolparty/spec/matchers/have_exec.rb
- lib/poolparty/spec/matchers/have_file.rb
- lib/poolparty/spec/matchers/have_gempackage.rb
- lib/poolparty/spec/matchers/have_git.rb
- lib/poolparty/spec/matchers/have_host.rb
- lib/poolparty/spec/matchers/have_mount.rb
- lib/poolparty/spec/matchers/have_package.rb
- lib/poolparty/spec/matchers/have_remotefile.rb
- lib/poolparty/spec/matchers/have_rsyncmirror.rb
- lib/poolparty/spec/matchers/have_service.rb
- lib/poolparty/spec/matchers/have_sshkey.rb
- lib/poolparty/spec/matchers/have_symlink.rb
- lib/poolparty/spec/matchers/have_variable.rb
- lib/poolparty/spec/spec/dynamic_matchers.rb
- lib/poolparty/spec/spec/ensure_matchers_exist.rb
- lib/poolparty/spec/templates/have_base.rb
- lib/poolparty/templates/authkeys
- lib/poolparty/templates/cib.xml
- lib/poolparty/templates/gem
- lib/poolparty/templates/ha.cf
- lib/poolparty/templates/haproxy.conf
- lib/poolparty/templates/haresources
- lib/poolparty/templates/logd.cf
- lib/poolparty/templates/messenger/client/log-run.erb
- lib/poolparty/templates/messenger/client/run.erb
- lib/poolparty/templates/messenger/master/log-run.erb
- lib/poolparty/templates/messenger/master/run.erb
- lib/poolparty/templates/messenger/node/log-run.erb
- lib/poolparty/templates/messenger/node/run.erb
- lib/poolparty/templates/namespaceauth.conf
- lib/poolparty/templates/poolparty.monitor
- lib/poolparty/templates/puppet.conf
- lib/poolparty/templates/puppetcleaner
- lib/poolparty/templates/puppetrerun
- lib/poolparty/templates/puppetrunner
- lib/poolparty/templates/yaws.conf
- lib/poolparty/version.rb
- lib/poolpartycl.rb
- lib/poolpartyspec.rb
- log/pool.log
- poolparty.gemspec
- script/destroy
- script/generate
- script/txt2html
- setup.rb
- spec/poolparty/aska/aska_spec.rb
- spec/poolparty/base_packages/haproxy_spec.rb
- spec/poolparty/base_packages/heartbeat_spec.rb
- spec/poolparty/bin/console_spec.rb
- spec/poolparty/core/array_spec.rb
- spec/poolparty/core/float.rb
- spec/poolparty/core/hash_spec.rb
- spec/poolparty/core/kernel_spec.rb
- spec/poolparty/core/module_spec.rb
- spec/poolparty/core/object_spec.rb
- spec/poolparty/core/string_spec.rb
- spec/poolparty/core/time_spec.rb
- spec/poolparty/dependency_resolutions/base_spec.rb
- spec/poolparty/helpers/binary_spec.rb
- spec/poolparty/helpers/display_spec.rb
- spec/poolparty/helpers/optioner_spec.rb
- spec/poolparty/helpers/provisioner_base_spec.rb
- spec/poolparty/helpers/provisioners/master_spec.rb
- spec/poolparty/helpers/provisioners/slave_spec.rb
- spec/poolparty/modules/cloud_resourcer_spec.rb
- spec/poolparty/modules/configurable_spec.rb
- spec/poolparty/modules/definable_resource.rb
- spec/poolparty/modules/file_writer_spec.rb
- spec/poolparty/modules/s3_string_spec.rb
- spec/poolparty/monitors/base_monitor_spec.rb
- spec/poolparty/monitors/monitors/cpu_monitor_spec.rb
- spec/poolparty/monitors/monitors/memory_monitor_spec.rb
- spec/poolparty/net/messenger_spec.rb
- spec/poolparty/net/remote_bases/ec2_spec.rb
- spec/poolparty/net/remote_instance_spec.rb
- spec/poolparty/net/remote_spec.rb
- spec/poolparty/net/remoter_base_spec.rb
- spec/poolparty/net/remoter_spec.rb
- spec/poolparty/plugins/deploydirectory_spec.rb
- spec/poolparty/plugins/git_spec.rb
- spec/poolparty/plugins/line_spec.rb
- spec/poolparty/plugins/svn_spec.rb
- spec/poolparty/pool/base_spec.rb
- spec/poolparty/pool/cloud_spec.rb
- spec/poolparty/pool/configurers/files/ruby_basic.rb
- spec/poolparty/pool/configurers/files/ruby_plugins.rb
- spec/poolparty/pool/configurers/ruby_spec.rb
- spec/poolparty/pool/custom_resource_spec.rb
- spec/poolparty/pool/example_spec.rb
- spec/poolparty/pool/plugin_model_spec.rb
- spec/poolparty/pool/plugin_spec.rb
- spec/poolparty/pool/pool_spec.rb
- spec/poolparty/pool/resource_spec.rb
- spec/poolparty/pool/resources/class_package_spec.rb
- spec/poolparty/pool/resources/conditional_spec.rb
- spec/poolparty/pool/resources/cron_spec.rb
- spec/poolparty/pool/resources/directory_spec.rb
- spec/poolparty/pool/resources/exec_spec.rb
- spec/poolparty/pool/resources/file_spec.rb
- spec/poolparty/pool/resources/gem_spec.rb
- spec/poolparty/pool/resources/host_spec.rb
- spec/poolparty/pool/resources/package_spec.rb
- spec/poolparty/pool/resources/remote_file_spec.rb
- spec/poolparty/pool/resources/service_spec.rb
- spec/poolparty/pool/resources/sshkey_spec.rb
- spec/poolparty/pool/resources/symlink_spec.rb
- spec/poolparty/pool/resources/variable_spec.rb
- spec/poolparty/pool/script_spec.rb
- spec/poolparty/pool/test_plugins/sshkey_test
- spec/poolparty/pool/test_plugins/virtual_host_template.erb
- spec/poolparty/pool/test_plugins/webserver.rb
- spec/poolparty/poolparty_spec.rb
- spec/poolparty/spec/core/string_spec.rb
- spec/poolparty/spec_helper.rb
- tasks/cloud.rake
- tasks/deployment.rake
- tasks/development.rake
- tasks/ec2.rake
- tasks/environment.rake
- tasks/instance.rake
- tasks/server.rake
- tasks/spec.rake
- tasks/website.rake
- test/test_generator_helper.rb
- test/test_helper.rb
- test/test_pool_spec_generator.rb
- test/test_poolparty.rb
- website/index.html
- website/index.txt
- website/javascripts/rounded_corners_lite.inc.js
- website/stylesheets/code.css
- website/stylesheets/screen.css
- website/template.html.erb
has_rdoc: true
homepage: http://poolparty.rubyforge.org
post_install_message: |-
  Get ready to jump in the pool, you just installed PoolParty! (Updated at 06:12 12/05/08)
  
  To get started, run the generator:
  
    pool spec <name>
  
  Please check out the documentation for any questions or check out the google groups at
    http://groups.google.com/group/poolpartyrb
  
  More tutorials can be found at 
    http://poolpartyrb.com
  
  Don't forget to check out the plugin tutorial @ http://poolpartyrb.com to extend PoolParty for your needs!
  
  For more information, check http://PoolPartyrb.com or visit us on IRC at:
    irc.freenode.net
    #poolpartyrb
  
  *** Ari Lerner @ <arilerner@mac.com> ***
rdoc_options: 
- --main
- README.txt
require_paths: 
- lib
required_ruby_version: !ruby/object:Gem::Requirement 
  requirements: 
  - - ">="
    - !ruby/object:Gem::Version 
      version: "0"
  version: 
required_rubygems_version: !ruby/object:Gem::Requirement 
  requirements: 
  - - ">="
    - !ruby/object:Gem::Version 
      version: "0"
  version: 
requirements: []

rubyforge_project: poolparty
rubygems_version: 1.2.0
signing_key: 
specification_version: 2
summary: Self-healing, auto-scaling system administration, provisioning and maintaining tool that makes cloud computing fun and easy
test_files: 
- test/test_generator_helper.rb
- test/test_helper.rb
- test/test_pool_spec_generator.rb
- test/test_poolparty.rb
