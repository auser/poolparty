Capistrano::Configuration.instance(:must_exist).load do

  desc "Start provisioner base"
  def start_provisioner_base
    # run "/etc/init.d/puppetmaster start"
    run "/usr/bin/puppetrunner"
  end

  desc "Restart provisioner base"
  def restart_provisioner_base #FIXME: should inherit from cloud dependency_resolver_command
    "/usr/bin/puppetrunner"
    # run "/etc/init.d/puppetmaster stop;rm -rf /etc/poolparty/ssl;start_provisioner_based --verbose;/etc/init.d/puppetmaster start"
  end

  desc "Ensure provisioner is running"
  def ensure_provisioner_is_running
    "/usr/bin/puppetrunner"
    # run "/usr/sbin/puppetmasterd --verbose 2>1 > /dev/null;echo ''"
  end

  desc "Create local node for puppet manifest"
  def create_local_node_entry_for_puppet
    # run ". /etc/profile && server-write-new-nodes"
    str = ["node default { include poolparty }"]
    list_of_running_instances.each do |ri| 
      str << "node \"#{ri.name}\" inherits default {}\n"
    end
    put( str.join("\n"), "#{manifest_path}/nodes/nodes.pp")
  end

  #DEPRECATED
  # desc "Move template files into place"
  # def move_template_files
  #   run <<-EOR
  #     mkdir -p #{template_path} &&
  #     cp -R #{remote_storage_path}/templates/* #{template_path}
  #   EOR
  # end

  desc "put manifest into place" 
  def put_provisioner_manifest_on_server  
    put build_manifest, '/etc/puppet/manifests/classes/poolparty.pp'
  end

  desc "Create poolparty runner command"
  def create_puppetrunner_command
    run 'mkdir -p /root/log'
    put(::File.read(::File.dirname(__FILE__)+'/../../../templates/puppetrunner'), '/usr/bin/puppetrunner', :mode=>755)
  end
  
  desc "Create poolparty rerun command"
  def create_puppetrerun_command
    run <<-EOR
      cp #{remote_storage_path}/templates/puppetrerun /usr/bin/puppetrerun &&
      chmod +x /usr/bin/puppetrerun
    EOR
  end
  
  desc "Add the proper configs for provisioner"
  def add_provisioner_configs
    run "cp #{remote_storage_path}/namespaceauth.conf /etc/puppet/namespaceauth.conf"
  end
  
  desc "Setup config file for provisioner"
  def setup_provisioner_config
    run "mv #{remote_storage_path}/puppet.conf /etc/puppet/puppet.conf"
  end
  
  desc "Run the provisioner twice (usually on install)"
  def run_provisioner_twice
    # run "/usr/sbin/puppetd --test --server master 2>1 > /dev/null && /usr/sbin/puppetd --onetime --daemonize --logdest syslog --server master"
    run "/usr/bin/puppetrunner"
  end
  
  desc "Run the provisioner"
  def run_provisioner
    # run "/usr/sbin/puppetd --onetime --daemonize --logdest syslog --server master"
    run "/usr/bin/puppetrunner"
  end
  
  desc "Rerun the provisioner"
  def rerun_provisioner
    run "/usr/bin/puppetrunner"
  end
  
  desc "Remove the certs"
  def remove_certs
    run "rm -rf /etc/puppet/ssl"
  end
  
  desc "Update rubygems"
  def update_rubygems
    run "/usr/bin/gem update --system 2>1 > /dev/null;/usr/bin/gem update --system;echo 'gems updated'"
  end
  
  desc "Stop provisioner daemon"
  def stop_provisioner_daemon
    run "/etc/init.d/puppetmaster stop"
  end

end