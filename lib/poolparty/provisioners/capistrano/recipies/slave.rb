Capistrano::Configuration.instance(:must_exist).load do
  # namespace(:slave) do
    desc "Provision a slave"
    def slave_provision_slave_task
      upgrade_system
      add_master_to_hosts_file
      setup_for_poolparty
      install_provisioner
      stop_provisioner_daemon
      setup_basic_poolparty_structure
      setup_provisioner_filestore
      setup_provisioner_autosigning
      install_rubygems
      fix_rubygems
      add_provisioner_configs
      setup_provisioner_config
      create_puppetrunner_command
      # create_puppetrerun_command
      download_base_gems
      install_base_gems
      copy_gem_bins_to_usr_bin
      write_erlang_cookie
    end
    desc "Configure a slave"
    def slave_configure_slave_task
      create_local_node_entry_for_puppet
      move_provisioner_manifest
      move_template_files
      setup_poolparty_base_structure
      run_provisioner
    end
    desc "Add master ip to hosts file"
    def add_master_to_hosts_file
      run "if [ -z \"$(grep -v '#' /etc/hosts | grep 'master' | grep '#{cloud.master.ip}' )\" ]; then echo '#{cloud.master.ip} puppet master' >> /etc/hosts; else echo 'host already set'; fi"
    end
    desc "Stop provisioner daemon"
    def stop_provisioner_daemon
      run "/etc/init.d/puppetmaster stop"
    end
  # end
end