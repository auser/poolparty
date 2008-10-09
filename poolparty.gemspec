Gem::Specification.new do |s|
  s.name = %q{PoolParty}
  s.version = "0.2.2"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Ari Lerner"]
  s.autorequire = %q{PoolParty}
  s.date = %q{2008-10-08}
  s.description = %q{Self-healing, auto-scaling cloud computing tool}
  s.email = %q{ari.lerner@citrusbyte.com}
  s.extra_rdoc_files = ["README.txt", "License.txt", "History.txt"]
  s.files = ["Rakefile", "History.txt", "README.txt", "examples/basic.rb", "examples/plugin_without_plugin_directory.rb", "examples/poolparty.rb", "examples/with_apache_plugin.rb", "lib/erlang", "lib/erlang/eb_server.erl", "lib/poolparty", "lib/poolparty/base_packages", "lib/poolparty/base_packages/haproxy.rb", "lib/poolparty/base_packages/heartbeat.rb", "lib/poolparty/base_packages/poolparty.rb", "lib/poolparty/base_packages/ruby.rb", "lib/poolparty/core", "lib/poolparty/core/array.rb", "lib/poolparty/core/exception.rb", "lib/poolparty/core/float.rb", "lib/poolparty/core/hash.rb", "lib/poolparty/core/kernel.rb", "lib/poolparty/core/module.rb", "lib/poolparty/core/my_open_struct.rb", "lib/poolparty/core/object.rb", "lib/poolparty/core/proc.rb", "lib/poolparty/core/string.rb", "lib/poolparty/core/symbol.rb", "lib/poolparty/core/time.rb", "lib/poolparty/exceptions", "lib/poolparty/exceptions/RemoteException.rb", "lib/poolparty/exceptions/ResourceException.rb", "lib/poolparty/exceptions/RuntimeException.rb", "lib/poolparty/exceptions/SpecException.rb", "lib/poolparty/exceptions/TemplateNotFound.rb", "lib/poolparty/helpers", "lib/poolparty/helpers/binary.rb", "lib/poolparty/helpers/console.rb", "lib/poolparty/helpers/display.rb", "lib/poolparty/helpers/optioner.rb", "lib/poolparty/helpers/provisioner_base.rb", "lib/poolparty/helpers/provisioners", "lib/poolparty/helpers/provisioners/master.rb", "lib/poolparty/helpers/provisioners/slave.rb", "lib/poolparty/modules", "lib/poolparty/modules/cloud_resourcer.rb", "lib/poolparty/modules/configurable.rb", "lib/poolparty/modules/definable_resource.rb", "lib/poolparty/modules/file_writer.rb", "lib/poolparty/modules/method_missing_sugar.rb", "lib/poolparty/modules/output.rb", "lib/poolparty/modules/pretty_printer.rb", "lib/poolparty/modules/s3_string.rb", "lib/poolparty/modules/safe_instance.rb", "lib/poolparty/monitors", "lib/poolparty/monitors/base_monitor.rb", "lib/poolparty/net", "lib/poolparty/net/remote.rb", "lib/poolparty/net/remote_bases", "lib/poolparty/net/remote_bases/ec2.rb", "lib/poolparty/net/remote_instance.rb", "lib/poolparty/net/remoter.rb", "lib/poolparty/net/remoter_base.rb", "lib/poolparty/plugins", "lib/poolparty/plugins/gem_package.rb", "lib/poolparty/plugins/line.rb", "lib/poolparty/plugins/svn.rb", "lib/poolparty/pool", "lib/poolparty/pool/base.rb", "lib/poolparty/pool/cloud.rb", "lib/poolparty/pool/custom_resource.rb", "lib/poolparty/pool/loggable.rb", "lib/poolparty/pool/plugin.rb", "lib/poolparty/pool/plugin_model.rb", "lib/poolparty/pool/pool.rb", "lib/poolparty/pool/resource.rb", "lib/poolparty/pool/resources", "lib/poolparty/pool/resources/class_package.rb", "lib/poolparty/pool/resources/cron.rb", "lib/poolparty/pool/resources/directory.rb", "lib/poolparty/pool/resources/exec.rb", "lib/poolparty/pool/resources/file.rb", "lib/poolparty/pool/resources/gem.rb", "lib/poolparty/pool/resources/host.rb", "lib/poolparty/pool/resources/package.rb", "lib/poolparty/pool/resources/remote_file.rb", "lib/poolparty/pool/resources/service.rb", "lib/poolparty/pool/resources/sshkey.rb", "lib/poolparty/pool/resources/variable.rb", "lib/poolparty/pool/script.rb", "lib/poolparty/templates", "lib/poolparty/templates/authkeys", "lib/poolparty/templates/cib.xml", "lib/poolparty/templates/fileserver.conf", "lib/poolparty/templates/ha.cf", "lib/poolparty/templates/haproxy.conf", "lib/poolparty/templates/namespaceauth.conf", "lib/poolparty/templates/puppet.conf", "lib/poolparty/version.rb", "lib/poolparty.rb", "lib/poolpartycl.rb", "tasks/cloud.rake", "tasks/deployment.rake", "tasks/development.rake", "tasks/ec2.rake", "tasks/environment.rake", "tasks/instance.rake", "tasks/server.rake", "tasks/spec.rake", "tasks/website.rake", "script/destroy", "script/generate", "script/txt2html", "generators/poolspec", "generators/poolspec/poolspec_generator.rb", "generators/poolspec/templates", "generators/poolspec/templates/pool_spec_template.erb", "generators/poolspec/USAGE", "bin/cloud", "bin/cloud-add-keypair", "bin/cloud-configure", "bin/cloud-contract", "bin/cloud-expand", "bin/cloud-list", "bin/cloud-maintain", "bin/cloud-provision", "bin/cloud-reconfigure", "bin/cloud-ssh", "bin/cloud-start", "bin/pool", "bin/pool-console", "bin/pool-describe", "bin/pool-list", "bin/pool-provision", "bin/pool-spec", "bin/pool-start", "bin/pool-start-monitor", "License.txt"]
  s.has_rdoc = true
  s.homepage = %q{http://poolpartyrb.com}
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.2.0}
  s.summary = %q{Self-healing, auto-scaling cloud computing tool}

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 2

    if current_version >= 3 then
    else
    end
  else
  end
end
