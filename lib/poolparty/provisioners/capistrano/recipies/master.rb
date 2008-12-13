# Cloud tasks
Capistrano::Configuration.instance(:must_exist).load do
  namespace(:master) do
    desc "Provision master"
    task :provision_master do
      upgrade_system
      set_hostname_to_master
      create_local_hosts_entry
      install_provisioner
      start_provisioner_base
      setup_basic_poolparty_structure
      setup_provisioner_filestore
      setup_provisioner_autosigning
      install_rubygems
      fix_rubygems
      add_provisioner_configs
      setup_provisioner_config
      create_puppetrunner_command
      create_puppetrerun_command
      download_base_gems
      install_base_gems
    end
    desc "Set hostname to master"
    task :set_hostname_to_master do
      run "hostname master"
    end
    desc "Add host entry into the master instance"
    task :create_local_hosts_entry do
      run "if [ -z \"$(grep -v '#' /etc/hosts | grep 'puppet')\" ]; then echo '#{master_ip}          master puppet localhost' >> /etc/hosts; fi"
    end
    desc "Setup basic poolparty structure"
    task :setup_basic_poolparty_structure do
      run <<-EOR
        echo "Creating basic structure for poolparty" &&
        mkdir -p /etc/puppet/manifests/nodes  &&
        mkdir -p /etc/puppet/manifests/classes &&
        echo "import 'nodes/*.pp'" > /etc/puppet/manifests/site.pp &&
        echo "import 'classes/*.pp'" >> /etc/puppet/manifests/site.pp          
      EOR
    end
    desc "Setup shareable file system for provisioner"
    task :setup_provisioner_filestore do
      run <<-EOR
        echo '[files]' > /etc/puppet/fileserver.conf &&
        echo '  path #{remote_storage_path}' >> /etc/puppet/fileserver.conf &&
        echo '  allow *' >> /etc/puppet/fileserver.conf &&
        mkdir -p /var/poolparty/facts &&
        mkdir -p /var/poolparty/files &&
        mkdir -p #{base_config_directory}
      EOR
    end
    desc "Setup autosigning for provisioner"
    task :setup_provisioner_autosigning do
      run "echo \"*\" > /etc/puppet/autosign.conf"
    end
    desc "Setup poolparty structure"
    task :setup_poolparty_base_structure do
      run <<-EOR
        cp #{remote_storage_path}/#{key_file_location} "#{base_config_directory}/.ppkeys" &&
        mv #{remote_storage_path}/#{default_specfile_name} #{base_config_directory}/
      EOR
    end
    desc "Download base gems"
    task :download_base_gems do
      run download_base_gems_string
    end
    desc "Install base gems"
    task :install_base_gems do
      run install_base_gems_string
    end
    desc "Start provisioner base"
    task :start_provisioner_base do
      run "/etc/init.d/puppetmaster start"
    end
    desc "Restart provisioner base"
    task :restart_provisioner_base do
      run "/etc/init.d/puppetmaster stop;rm -rf /etc/poolparty/ssl;puppetmasterd --verbose;/etc/init.d/puppetmaster start"
    end
    desc "Create local node for puppet manifest"
    task :create_local_node_entry_for_puppet do
      run "
        echo 'node default { include poolparty }' > #{manifest_path}/nodes/nodes.pp &&
        echo '#{node_string}' >> #{manifest_path}/nodes/nodes.pp
      "
    end
    desc "Move template files into place"
    task :move_template_files do
      run <<-EOR
        mkdir -p #{template_path} &&
        cp -R #{remote_storage_path}/#{template_directory}/* #{template_path}
      EOR
    end
    desc "Move manifest into place" 
    task :move_provisioner_manifest do
      run <<-EOR
        cp #{remote_storage_path}/poolparty.pp /etc/puppet/manifests/classes/poolparty.pp
      EOR
    end
    desc "Move poolparty keys"
    task :move_poolparty_keys do
      run "cp #{remote_storage_path}/#{@full_keypair_name} #{@remote_keypair_path}"
    end
  end
end