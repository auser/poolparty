# Cookbook Name:: ganglia
# Recipe:: web

# hmm, maybe unify with below
execute "create ganglia web interface" do
  command "cp -r  /opt/local/src/ganglia-3.1.2/web #{node[:ganglia][:gmond_web_root]}"
  action :run
  creates node[:ganglia][:gmond_web_root]
end

# setenforce 0
execute "selinux: allow httpd can network for ganglia web interface" do
  command "setsebool -P httpd_can_network_connect 1"
  action :run
  only_if "/usr/sbin/getenforce | grep Enforcing"
  not_if "getsebool httpd_can_network_connect | grep on"
end
