# Cloud tasks
# Run each of these methods inside the Capistrano:Configuration context, dynamicly adding each method as a capistrano task.
Capistrano::Configuration.instance(:must_exist).load do
  # namespace(:master) do
    desc "Provision master"
    def master_provision_master_task
      upgrade_system
      set_hostname_to_master
      create_local_hosts_entry
      setup_for_poolparty
      install_provisioner
      setup_basic_poolparty_structure
      setup_provisioner_filestore
      setup_provisioner_autosigning      
      # install_rubygems
      install_ruby_from_stable_source
      install_rubygems_from_stable_source
      
      # fix_rubygems
      add_provisioner_configs
      setup_provisioner_config
      put_aws_credintials_on_server if using_remoter? == 'ec2'
      create_puppetrunner_command
      # download_base_gems
      unpack_dependencies_store
      install_base_gems  
      # copy_gem_bins_to_usr_bin
      # install_poolparty_from_github
      write_erlang_cookie
      vputs "master_provision_master_task complete"
    end
    
    desc "Configure master"
    def master_configure_master_task
      create_local_node_entry_for_puppet
      put_provisioner_manifest_on_server
      start_provisioner_base
      # move_template_files
      ensure_provisioner_is_running
      run_provisioner
    end
    
    desc "Set hostname to master"
    def set_hostname_to_master
      run "hostname master"
    end
    
    desc "Add host entry into the master instance"
    def create_local_hosts_entry
      run "if [ -z \"$(grep -v '#' /etc/hosts | grep 'puppet')\" ]; then echo '#{cloud.master.ip} master puppet localhost' >> /etc/hosts; fi"
    end
    
    desc "Download base gems"
    def download_base_gems
      run(returning(Array.new) do |arr|
        base_gems.each do |name, url|
          if url && !url.empty?
            arr << "curl -L -o #{Default.remote_storage_path}/#{name}.gem #{url} 2>&1; echo 'downloaded #{name}'"
            arr << "if test -s #{Default.remote_storage_path}/#{name}.gem; then echo ''; else rm #{Default.remote_storage_path}/#{name}.gem; fi; echo ''"
          end
        end
      end.join(" && "))
    end
    
  # end
end