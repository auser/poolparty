template "/etc/ganglia/bin/monitors/watson_channels.sh" do
  source "bin/monitors/watson_channels.sh.erb"
  mode "0755"
end

cron "watson-channels" do
  minute "*/1"
  hour "*"
  day "*"
  month "*"
  weekday "*"
  command "bash -c /etc/ganglia/bin/monitors/watson_channels.sh"
end

