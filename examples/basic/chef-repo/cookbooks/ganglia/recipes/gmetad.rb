# Cookbook Name:: ganglia
# Recipe:: gmetad

template "/etc/init.d/gmetad" do
  mode "0755"
  source "bin/gmetad.erb"
end

service "gmetad" do
  action :nothing
end

template "/etc/ganglia/gmetad.conf" do
  source "gmetad.conf.erb"
  mode "0755"
  notifies :restart, resources(:service => "gmetad")
end

directory "/var/lib/ganglia/rrds" do
  action :create
  recursive true
  owner node[:ganglia][:gmetad_user]
  group node[:ganglia][:gmetad_user]
end

script "install ganglia" do
  interpreter "sh -x"

  configure = "./configure --with-gmetad --disable-python --with-librrd=/usr/local/rrdtool-1.3.1"

  code <<-EOH 
mkdir -p /opt/local/src
cd /opt/local/src
wget http://superb-west.dl.sourceforge.net/sourceforge/ganglia/ganglia-3.1.2.tar.gz -O ganglia.tar.gz
tar -xvvf ganglia.tar.gz 
cd ganglia-3.1.2/
#{configure}
make && make install
  EOH
  creates "/usr/sbin/gmetad"
  notifies(:restart, resources(:service => "apache2")) rescue nil
  notifies :restart, resources(:service => "gmetad")
end

