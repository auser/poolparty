=begin rdoc
  Base provisioner capistrano tasks
=end
Capistrano::Configuration.instance(:must_exist).load do
  # namespace(:base) do
    desc "Install rubygems"
    def install_rubygems
      run "#{installer_for} ruby rubygems"
    end
    desc "Setup for poolparty"
    def setup_for_poolparty
      run "mkdir -p /etc/poolparty"
    end
    desc "Install provisioner"
    def install_provisioner
      run "#{installer_for} #{puppet_packages}"
    end
    desc "Create poolparty commands"
    def create_poolparty_commands
    end
    desc "Create poolparty runner command"
    def create_puppetrunner_command
      run <<-EOR
        cp #{remote_storage_path}/templates/puppetrunner /usr/bin/puppetrunner &&
        chmod +x /usr/bin/puppetrunner
      EOR
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
      run "/usr/sbin/puppetd --test --server master 2>1 > /dev/null && /usr/sbin/puppetd --onetime --daemonize --logdest syslog --server master"
    end
    desc "Run the provisioner"
    def run_provisioner
      run "/usr/sbin/puppetd --onetime --daemonize --logdest syslog --server master"
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
    desc "Fix rubygems"
    def fix_rubygems
      # echo '#{open(::File.join(template_directory, "gem")).read}' > /usr/bin/gem &&
      # cp #{remote_storage_path}/gem /usr/bin/gem
      run <<-EOR
        if gem -v; then echo "gem is working"; else cp #{remote_storage_path}/gem /usr/bin/gem; fi;
        /usr/bin/gem update --system 2>&1 > /dev/null;/usr/bin/gem update --system;
        if gem -v; then echo "gem is working"; else cp #{remote_storage_path}/gem /usr/bin/gem; fi;
        echo 'gems updated!'
      EOR
    end
    desc "Upgrade system"
    def upgrade_system
      str = case os
      when :ubuntu
        "
echo 'deb http://mirrors.kernel.org/ubuntu hardy main universe' >> /etc/apt/sources.list &&
aptitude update -y
        "
      else
        "echo 'No system upgrade needed'"
      end
      run str
    end
    desc "Upgrade rubygems"
    def upgrade_rubygems
      
    end
    desc "Make log directory"
    def make_log_directory
      run "mkdir -p /var/log/poolparty"
    end
    desc "Create ssl storage directories for poolparty"
    def create_poolparty_ssl_store
      run <<-EOR
        mkdir -p #{poolparty_config_directory}/ssl/private_keys &&
        mkdir -p #{poolparty_config_directory}/ssl/certs &&
        mkdir -p #{poolparty_config_directory}/ssl/public_keys
      EOR
    end
    desc "Add erlang cookie"
    def write_erlang_cookie
      run <<-EOR
        mv #{remote_storage_path}/cookie ~/.erlang.cookie &&
        chmod 400 ~/.erlang.cookie
      EOR
    end
    desc "Setup basic poolparty structure"
    def setup_basic_poolparty_structure
      run <<-EOR
        echo "Creating basic structure for poolparty" &&
        mkdir -p /etc/puppet/manifests/nodes  &&
        mkdir -p /etc/puppet/manifests/classes &&
        echo "import 'nodes/*.pp'" > /etc/puppet/manifests/site.pp &&
        echo "import 'classes/*.pp'" >> /etc/puppet/manifests/site.pp          
      EOR
    end
    desc "Setup shareable file system for provisioner"
    def setup_provisioner_filestore
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
    def setup_provisioner_autosigning
      run "echo \"*\" > /etc/puppet/autosign.conf"
    end
    desc "Setup poolparty structure"
    def setup_poolparty_base_structure
      run <<-EOR
        cp #{remote_storage_path}/#{key_file_locations.first} "#{base_config_directory}/.ppkeys" &&
        mv #{remote_storage_path}/#{default_specfile_name} #{base_config_directory}/#{default_specfile_name}
      EOR
    end
    
  # end
end