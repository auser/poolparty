Gem::Specification.new do |s|
  s.name = %q{poolparty}
  s.version = "0.2.10"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Ari Lerner"]
  s.date = %q{2008-10-16}
  s.description = %q{Self-healing, auto-scaling cloud computing tool}
  s.email = ["ari.lerner@citrusbyte.com"]
  s.executables = ["cloud", "cloud-add-keypair", "cloud-configure", "cloud-contract", "cloud-expand", "cloud-list", "cloud-maintain", "cloud-osxcopy", "cloud-provision", "cloud-refresh", "cloud-ssh", "cloud-start", "cloud-terminate", "pool", "pool-console", "pool-describe", "pool-list", "pool-provision", "pool-spec", "pool-start", "pool-start-monitor", "server-list-active", "server-rerun"]
  s.extra_rdoc_files = ["History.txt", "License.txt", "Manifest.txt", "PostInstall.txt", "README.txt", "website/index.txt"]
  s.files = ["History.txt", "License.txt", "Manifest.txt", "PostInstall.txt", "README.txt", "Rakefile", "bin/cloud", "bin/cloud-add-keypair", "bin/cloud-configure", "bin/cloud-contract", "bin/cloud-expand", "bin/cloud-list", "bin/cloud-maintain", "bin/cloud-osxcopy", "bin/cloud-provision", "bin/cloud-refresh", "bin/cloud-ssh", "bin/cloud-start", "bin/cloud-terminate", "bin/pool", "bin/pool-console", "bin/pool-describe", "bin/pool-list", "bin/pool-provision", "bin/pool-spec", "bin/pool-start", "bin/pool-start-monitor", "bin/server-list-active", "bin/server-rerun", "config/hoe.rb", "config/requirements.rb", "examples/basic.rb", "examples/plugin_without_plugin_directory.rb", "examples/poolparty.rb", "examples/with_apache_plugin.rb", "generators/poolspec/USAGE", "generators/poolspec/poolspec_generator.rb", "generators/poolspec/templates/pool_spec_template.erb", "lib/erlang/eb_server.erl", "lib/poolparty.rb", "lib/poolparty/base_packages/haproxy.rb", "lib/poolparty/base_packages/heartbeat.rb", "lib/poolparty/base_packages/poolparty.rb", "lib/poolparty/base_packages/ruby.rb", "lib/poolparty/core/array.rb", "lib/poolparty/core/exception.rb", "lib/poolparty/core/float.rb", "lib/poolparty/core/hash.rb", "lib/poolparty/core/kernel.rb", "lib/poolparty/core/module.rb", "lib/poolparty/core/my_open_struct.rb", "lib/poolparty/core/object.rb", "lib/poolparty/core/proc.rb", "lib/poolparty/core/string.rb", "lib/poolparty/core/symbol.rb", "lib/poolparty/core/time.rb", "lib/poolparty/exceptions/RemoteException.rb", "lib/poolparty/exceptions/ResourceException.rb", "lib/poolparty/exceptions/RuntimeException.rb", "lib/poolparty/exceptions/SpecException.rb", "lib/poolparty/exceptions/TemplateNotFound.rb", "lib/poolparty/helpers/binary.rb", "lib/poolparty/helpers/console.rb", "lib/poolparty/helpers/display.rb", "lib/poolparty/helpers/optioner.rb", "lib/poolparty/helpers/provisioner_base.rb", "lib/poolparty/helpers/provisioners/master.rb", "lib/poolparty/helpers/provisioners/slave.rb", "lib/poolparty/modules/cloud_resourcer.rb", "lib/poolparty/modules/configurable.rb", "lib/poolparty/modules/definable_resource.rb", "lib/poolparty/modules/file_writer.rb", "lib/poolparty/modules/method_missing_sugar.rb", "lib/poolparty/modules/output.rb", "lib/poolparty/modules/pretty_printer.rb", "lib/poolparty/modules/s3_string.rb", "lib/poolparty/modules/safe_instance.rb", "lib/poolparty/monitors/base_monitor.rb", "lib/poolparty/net/remote.rb", "lib/poolparty/net/remote_bases/ec2.rb", "lib/poolparty/net/remote_instance.rb", "lib/poolparty/net/remoter.rb", "lib/poolparty/net/remoter_base.rb", "lib/poolparty/plugins/gem_package.rb", "lib/poolparty/plugins/git.rb", "lib/poolparty/plugins/line.rb", "lib/poolparty/plugins/svn.rb", "lib/poolparty/pool/base.rb", "lib/poolparty/pool/cloud.rb", "lib/poolparty/pool/custom_resource.rb", "lib/poolparty/pool/loggable.rb", "lib/poolparty/pool/plugin.rb", "lib/poolparty/pool/plugin_model.rb", "lib/poolparty/pool/pool.rb", "lib/poolparty/pool/resource.rb", "lib/poolparty/pool/resources/class_package.rb", "lib/poolparty/pool/resources/conditional.rb", "lib/poolparty/pool/resources/cron.rb", "lib/poolparty/pool/resources/directory.rb", "lib/poolparty/pool/resources/exec.rb", "lib/poolparty/pool/resources/file.rb", "lib/poolparty/pool/resources/gem.rb", "lib/poolparty/pool/resources/host.rb", "lib/poolparty/pool/resources/package.rb", "lib/poolparty/pool/resources/remote_file.rb", "lib/poolparty/pool/resources/service.rb", "lib/poolparty/pool/resources/sshkey.rb", "lib/poolparty/pool/resources/symlink.rb", "lib/poolparty/pool/resources/variable.rb", "lib/poolparty/pool/script.rb", "lib/poolparty/puppet_plugins/poolparty/plugins/puppet/parser/functions/activenodeips.rb", "lib/poolparty/puppet_plugins/poolparty/plugins/puppet/parser/functions/activenodenames.rb", "lib/poolparty/templates/authkeys", "lib/poolparty/templates/cib.xml", "lib/poolparty/templates/fileserver.conf", "lib/poolparty/templates/gem", "lib/poolparty/templates/ha.cf", "lib/poolparty/templates/haproxy.conf", "lib/poolparty/templates/haresources", "lib/poolparty/templates/namespaceauth.conf", "lib/poolparty/templates/poolparty.monitor", "lib/poolparty/templates/puppet.conf", "lib/poolparty/version.rb", "lib/poolpartycl.rb", "poolparty.gemspec", "script/destroy", "script/generate", "script/txt2html", "setup.rb", "spec/poolparty/base_packages/haproxy_spec.rb", "spec/poolparty/base_packages/heartbeat_spec.rb", "spec/poolparty/bin/console_spec.rb", "spec/poolparty/core/array_spec.rb", "spec/poolparty/core/float.rb", "spec/poolparty/core/hash_spec.rb", "spec/poolparty/core/kernel_spec.rb", "spec/poolparty/core/module_spec.rb", "spec/poolparty/core/object_spec.rb", "spec/poolparty/core/string_spec.rb", "spec/poolparty/core/time_spec.rb", "spec/poolparty/helpers/binary_spec.rb", "spec/poolparty/helpers/display_spec.rb", "spec/poolparty/helpers/optioner_spec.rb", "spec/poolparty/helpers/provisioner_base_spec.rb", "spec/poolparty/helpers/provisioners/master_spec.rb", "spec/poolparty/helpers/provisioners/slave_spec.rb", "spec/poolparty/modules/cloud_resourcer_spec.rb", "spec/poolparty/modules/configurable_spec.rb", "spec/poolparty/modules/definable_resource.rb", "spec/poolparty/modules/file_writer_spec.rb", "spec/poolparty/modules/s3_string_spec.rb", "spec/poolparty/net/remote_bases/ec2_spec.rb", "spec/poolparty/net/remote_instance_spec.rb", "spec/poolparty/net/remote_spec.rb", "spec/poolparty/net/remoter_base_spec.rb", "spec/poolparty/net/remoter_spec.rb", "spec/poolparty/plugins/git_spec.rb", "spec/poolparty/plugins/line_spec.rb", "spec/poolparty/plugins/svn_spec.rb", "spec/poolparty/pool/base_spec.rb", "spec/poolparty/pool/cloud_spec.rb", "spec/poolparty/pool/configurers/files/ruby_basic.rb", "spec/poolparty/pool/configurers/files/ruby_plugins.rb", "spec/poolparty/pool/configurers/ruby_spec.rb", "spec/poolparty/pool/custom_resource_spec.rb", "spec/poolparty/pool/example_spec.rb", "spec/poolparty/pool/plugin_model_spec.rb", "spec/poolparty/pool/plugin_spec.rb", "spec/poolparty/pool/pool_spec.rb", "spec/poolparty/pool/resource_spec.rb", "spec/poolparty/pool/resources/class_package_spec.rb", "spec/poolparty/pool/resources/conditional_spec.rb", "spec/poolparty/pool/resources/cron_spec.rb", "spec/poolparty/pool/resources/directory_spec.rb", "spec/poolparty/pool/resources/exec_spec.rb", "spec/poolparty/pool/resources/file_spec.rb", "spec/poolparty/pool/resources/gem_spec.rb", "spec/poolparty/pool/resources/host_spec.rb", "spec/poolparty/pool/resources/package_spec.rb", "spec/poolparty/pool/resources/remote_file_spec.rb", "spec/poolparty/pool/resources/service_spec.rb", "spec/poolparty/pool/resources/sshkey_spec.rb", "spec/poolparty/pool/resources/symlink_spec.rb", "spec/poolparty/pool/resources/variable_spec.rb", "spec/poolparty/pool/script_spec.rb", "spec/poolparty/pool/test_plugins/sshkey_test", "spec/poolparty/pool/test_plugins/virtual_host_template.erb", "spec/poolparty/pool/test_plugins/webserver.rb", "spec/poolparty/poolparty_spec.rb", "spec/poolparty/spec_helper.rb", "tasks/cloud.rake", "tasks/deployment.rake", "tasks/development.rake", "tasks/ec2.rake", "tasks/environment.rake", "tasks/instance.rake", "tasks/server.rake", "tasks/spec.rake", "tasks/website.rake", "test/test_generator_helper.rb", "test/test_helper.rb", "test/test_pool_spec_generator.rb", "test/test_poolparty.rb", "test_manifest.pp", "website/index.html", "website/index.txt", "website/javascripts/rounded_corners_lite.inc.js", "website/stylesheets/code.css", "website/stylesheets/screen.css", "website/template.html.erb"]
  s.has_rdoc = true
  s.homepage = %q{http://poolparty.rubyforge.org}
  s.post_install_message = %q{Get ready to jump in the pool, you just installed poolpartyrb!

To get started, run the generator:

  pool spec <name>

Please check out the documentation for any questions or check out the google groups at
  http://groups.google.com/group/poolpartyrb

Don't forget to check out the plugin tutorial @ http://poolpartyrb.com for extending PoolParty!

For more information, check http://PoolPartyrb.com
On IRC: 
  irc.freenode.net
  #poolpartyrb
  
*** Ari Lerner @ <arilerner@mac.com> ***}
  s.rdoc_options = ["--main", "README.txt"]
  s.require_paths = ["lib"]
  s.rubyforge_project = %q{poolparty}
  s.rubygems_version = %q{1.2.0}
  s.summary = %q{Self-healing, auto-scaling cloud computing tool}
  s.test_files = ["test/test_generator_helper.rb", "test/test_helper.rb", "test/test_pool_spec_generator.rb", "test/test_poolparty.rb"]

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 2

    if current_version >= 3 then
      s.add_runtime_dependency(%q<activesupport>, [">= 0"])
      s.add_runtime_dependency(%q<open4>, [">= 0"])
      s.add_runtime_dependency(%q<logging>, [">= 0"])
      s.add_runtime_dependency(%q<ruby2ruby>, [">= 0"])
      s.add_development_dependency(%q<hoe>, [">= 1.7.0"])
    else
      s.add_dependency(%q<activesupport>, [">= 0"])
      s.add_dependency(%q<open4>, [">= 0"])
      s.add_dependency(%q<logging>, [">= 0"])
      s.add_dependency(%q<ruby2ruby>, [">= 0"])
      s.add_dependency(%q<hoe>, [">= 1.7.0"])
    end
  else
    s.add_dependency(%q<activesupport>, [">= 0"])
    s.add_dependency(%q<open4>, [">= 0"])
    s.add_dependency(%q<logging>, [">= 0"])
    s.add_dependency(%q<ruby2ruby>, [">= 0"])
    s.add_dependency(%q<hoe>, [">= 1.7.0"])
  end
end