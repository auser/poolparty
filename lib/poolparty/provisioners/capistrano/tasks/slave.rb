# namespace(:slave) do
  desc "Add master ip to hosts file"
  task :add_master_to_hosts_file do
    run "if [ -z \"$(grep -v '#' /etc/hosts | grep 'master')\" ]; then echo '#{master_ip} puppet master' >> /etc/hosts; else echo 'host already set'; fi"
  end
  desc "Stop provisioner daemon"
  task :stop_provisioner_daemon do
    run "/etc/init.d/puppetmaster stop"
  end
# end