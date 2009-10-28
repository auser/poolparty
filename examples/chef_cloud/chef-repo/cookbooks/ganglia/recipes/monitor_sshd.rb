template "/etc/ganglia/bin/monitors/sshd_ganglia.sh" do
  source "bin/monitors/sshd_ganglia.sh.erb"
  mode "0755"
end

cron "monitor sshd" do
  minute "*/5"
  hour "*"
  day "*"
  month "*"
  weekday "*"
  command "bash -c /etc/ganglia/bin/monitors/sshd_ganglia.sh"
end

