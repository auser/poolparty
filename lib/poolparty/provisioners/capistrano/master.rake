# Cloud tasks
namespace(:master) do
  desc "Set hostname to master"
  task :set_hostname_to_master do
    run "hostname master"
  end
  desc "Add host entry into the master instance"
  task :create_local_hosts_entry do
    run "if [ -z \"$(grep -v '#' /etc/hosts | grep 'puppet')\" ]; then echo '#{@master_ip}          master puppet localhost' >> /etc/hosts; fi"
  end
  desc "Setup basic poolparty structure"
  task :setup_basic_poolparty_structure do
    run <<-EOR
      echo "Creating basic structure for poolparty"        
      mkdir -p /etc/puppet/manifests/nodes 
      mkdir -p /etc/puppet/manifests/classes
      echo "import 'nodes/*.pp'" > /etc/puppet/manifests/site.pp
      echo "import 'classes/*.pp'" >> /etc/puppet/manifests/site.pp          
    EOR
  end
  desc "Setup shareable file system for provisioner"
  task :setup_provisioner_filestore do
    run <<-EOR
      echo "
      [files]
        path #{Base.remote_storage_path}
        allow *" > /etc/puppet/fileserver.conf
      mkdir -p /var/poolparty/facts
      mkdir -p /var/poolparty/files
      mkdir -p #{Base.base_config_directory}
    EOR
  end
  desc "Setup autosigning for provisioner"
  task :setup_provisioner_autosigning do
    run "echo \"*\" > /etc/puppet/autosign.conf"
  end
  desc "Setup poolparty structure"
  task :setup_poolparty_base_structure do
    run <<-EOR
      cp #{Base.remote_storage_path}/#{Base.key_file_locations.first} "#{Base.base_config_directory}/.ppkeys"
      mv #{Base.remote_storage_path}/#{Base.default_specfile_name} #{Base.base_config_directory}/
    EOR
  end
  desc "Download base gems"
  task :download_base_gems do
    run returning Array.new do |arr|
      base_gems.each do |name, url|
        arr << "wget #{url} -O #{Base.remote_storage_path}/#{name}.gem 2>&1"
      end
    end.join("\n")
  end
  desc "Install base gems"
  task :install_base_gems do
    run returning Array.new do |arr|
      base_gems.each do |name, url|
        arr << "/usr/bin/gem install --ignore-dependencies -y --no-ri --no-rdoc #{Base.remote_storage_path}/#{name}.gem"
      end
    end.join("\n")
  end
  desc "Restart provisioner base"
  task :restart_provisioner_base do
    run "/etc/init.d/puppetmaster stop;rm -rf /etc/poolparty/ssl;puppetmasterd --verbose;/etc/init.d/puppetmaster start"
  end
  desc "Create local node for puppet manifest"
  task :create_local_node_entry_for_puppet do
        @str = <<-EOS
node default {
  include poolparty
}
#{list_of_running_instances.map {|ri| "node \"#{ri.name}\" inherits default {}"}.join("\n")}
        EOS
        run "echo '#{str}' > #{Base.manifest_path}/nodes/nodes.pp"
  end
  desc "Move template files into place"
  task :move_template_files do
    run <<-EOR
      mkdir -p #{Base.template_path}
      cp -R #{Base.remote_storage_path}/#{Base.template_directory}/* #{Base.template_path}
    EOR
  end
  desc "Move manifest into place" 
  task :move_provisioner_manifest do
    run <<-EOR
      cp #{Base.remote_storage_path}/poolparty.pp /etc/puppet/manifests/classes/poolparty.pp      
    EOR
  end
  desc "Move poolparty keys"
  task :move_poolparty_keys do
    run "cp #{Base.remote_storage_path}/#{@cloud.full_keypair_name} #{@cloud.remote_keypair_path}"
  end
end