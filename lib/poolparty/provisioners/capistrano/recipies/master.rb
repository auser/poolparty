# Cloud tasks
Capistrano::Configuration.instance(:must_exist).load do
  # namespace(:master) do
    desc "Provision master"
    def master_provision_master_task
      upgrade_system
      set_hostname_to_master
      create_local_hosts_entry
      setup_for_poolparty
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
      write_erlang_cookie
    end
    desc "Configure master"
    def master_configure_master_task
      create_local_node_entry_for_puppet
      move_provisioner_manifest
      move_template_files
      setup_poolparty_base_structure
      ensure_provisioner_is_running
      run_provisioner
    end
    desc "Set hostname to master"
    def set_hostname_to_master
      run "hostname master"
    end
    desc "Add host entry into the master instance"
    def create_local_hosts_entry
      run "if [ -z \"$(grep -v '#' /etc/hosts | grep 'puppet')\" ]; then echo '#{cloud.master.ip}          master puppet localhost' >> /etc/hosts; fi"
    end
    desc "Download base gems"
    def download_base_gems
      run(returning(Array.new) do |arr|
        base_gems.each do |name, url|
          if url && !url.empty?
            arr << "curl -L -o #{Base.remote_storage_path}/#{name}.gem #{url} 2>&1; echo 'downloaded #{name}'"
            arr << "if test -s #{Base.remote_storage_path}/#{name}.gem; then echo ''; else rm #{Base.remote_storage_path}/#{name}.gem; fi; echo ''"
          end
        end
      end.join(" && "))
    end
    desc "Install base gems"
    def install_base_gems
      run(returning(Array.new) do |arr|
        base_gems.each do |name, url|
          str = url.empty? ? "#{name}" : "#{Base.remote_storage_path}/#{name}.gem"
          arr << "/usr/bin/gem install --ignore-dependencies --no-ri --no-rdoc #{str}; echo 'insatlled #{name}'"
        end
      end.join(" && "))
    end
    desc "Start provisioner base"
    def start_provisioner_base
      run "/etc/init.d/puppetmaster start"
    end
    desc "Restart provisioner base"
    def restart_provisioner_base
      run "/etc/init.d/puppetmaster stop;rm -rf /etc/poolparty/ssl;start_provisioner_based --verbose;/etc/init.d/puppetmaster start"
    end
    desc "Ensure provisioner is running"
    def ensure_provisioner_is_running
      run "/usr/sbin/puppetmasterd --verbose 2>1 > /dev/null;echo ''"
    end
    desc "Create local node for puppet manifest"
    def create_local_node_entry_for_puppet
      # run ". /etc/profile && server-write-new-nodes"
      str = returning Array.new do |arr|
        arr << "node default { include poolparty }"
        list_of_running_instances.each do |ri| 
          arr << "node \"#{ri.name}\" inherits default {}\n"
        end
      end.join("\n")
      run "echo #{str} > #{manifest_path}/nodes/nodes.pp"
    end
    desc "Move template files into place"
    def move_template_files
      run <<-EOR
        mkdir -p #{template_path} &&
        cp -R #{remote_storage_path}/templates/* #{template_path}
      EOR
    end
    desc "Move custom modules"
    def move_custom_modules
      run <<-EOR
        mkdir -p 
      EOR
    end
    desc "Move manifest into place" 
    def move_provisioner_manifest
      run <<-EOR
        cp #{remote_storage_path}/poolparty.pp /etc/puppet/manifests/classes/poolparty.pp
      EOR
    end
    desc "Move poolparty keys"
    def move_poolparty_keys
      run "cp #{remote_storage_path}/#{full_keypair_name} #{remote_keypair_path}"
    end
  # end
end