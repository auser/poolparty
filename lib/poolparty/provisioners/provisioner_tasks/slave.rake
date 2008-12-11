namespace(:slave) do
  desc "Setup poolparty"
  task :setup_poolparty do
    run <<-EOR
      cp #{Base.remote_storage_path}/#{Base.template_directory}/puppetrunner /usr/bin/puppetrunner
      chmod +x /usr/bin/puppetrunner
    EOR
  end
  desc "Add master ip to hosts file"
  task :add_master_to_hosts_file do
    run "if [ -z \"$(grep -v '#' /etc/hosts | grep 'master')\" ]; then echo '#{master_ip} puppet master' >> /etc/hosts; else echo 'host already set'; fi"
  end
  desc "Stop provisioner daemon"
  task :stop_provisioner_daemon do
    run "/etc/init.d/puppetmaster stop"
  end
end