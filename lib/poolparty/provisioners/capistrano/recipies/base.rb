=begin rdoc
  Base provisioner capistrano tasks
=end
Capistrano::Configuration.instance(:must_exist).load do
  # namespace(:base) do
    desc "Install rubygems"
    task :install_rubygems do
      run "#{installer_for} ruby rubygems"
    end
    desc "Install provisioner"
    task :install_provisioner do
      run "#{installer_for} #{puppet_packages} <<heredoc
      Y
      heredoc"
    end
    desc "Create poolparty commands"
    task :create_poolparty_commands do
    end
    desc "Create poolparty runner command"
    task :create_puppetrunner_command do
      run <<-EOR
        cp #{remote_storage_path}/#{template_directory}/puppetrunner /usr/bin/puppetrunner &&
        chmod +x /usr/bin/puppetrunner
      EOR
    end
    desc "Create poolparty rerun command"
    task :create_puppetrerun_command do
      run <<-EOR
        cp #{remote_storage_path}/#{template_directory}/puppetrerun /usr/bin/puppetrerun &&
        chmod +x /usr/bin/puppetrerun
      EOR
    end
    desc "Add the proper configs for provisioner"
    task :add_provisioner_configs do
      run "cp #{remote_storage_path}/namespaceauth.conf /etc/puppet/namespaceauth.conf"
    end
    desc "Setup config file for provisioner"
    task :setup_provisioner_config do
      run "mv #{remote_storage_path}/puppet.conf /etc/puppet/puppet.conf"
    end
    desc "Run the provisioner"
    task :run_provisioner do
      run "/usr/sbin/puppetd --onetime --daemonize --logdest syslog --server master"
    end
    desc "Rerun the provisioner"
    task :rerun_provisioner do
      run "/usr/bin/puppetrerun"
    end
    desc "Remove the certs"
    task :remove_certs do
      run "rm -rf /etc/puppet/ssl"
    end
    desc "Update rubygems"
    task :update_rubygems do
      run "/usr/bin/gem update --system && /usr/bin/gem update --system"
    end
    desc "Fix rubygems"
    task :fix_rubygems do
      # echo '#{open(::File.join(template_directory, "gem")).read}' > /usr/bin/gem &&
      run <<-EOR
        cp -f #{remote_storage_path}/gem /usr/bin/gem &&
        /usr/bin/gem update --system 2>&1 > /dev/null;/usr/bin/gem update --system
      EOR
    end
    desc "Upgrade system"
    task :upgrade_system do
      str = case os
      when "ubuntu"
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
    task :upgrade_rubygems do
      
    end
    desc "Make log directory"
    task :make_log_directory do
      run "mkdir -p /var/log/poolparty"
    end
    desc "Create ssl storage directories for poolparty"
    task :create_poolparty_ssl_store do
      run <<-EOR
        mkdir -p #{poolparty_config_directory}/ssl/private_keys &&
        mkdir -p #{poolparty_config_directory}/ssl/certs &&
        mkdir -p #{poolparty_config_directory}/ssl/public_keys
      EOR
    end
    desc "Add erlang cookie"
    task :write_erlang_cookie do
      run <<-EOR
        mv #{remote_storage_path}/cookie ~/.erlang.cookie &&
        chmod 400 ~/.erlang.cookie
      EOR
    end
  # end
end