# Cookbook Name:: collectd
# Recipe:: default
#
# Copyright 2009, AT&T Interactive
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

package "collectd" do
  action :install
end

directory node[:collectd][:data_dir] do
  action :create
end

template "/etc/collectd/collectd.conf" do
  source "collectd.conf.erb"
  if node[:collectd][:listen]==true
    variables :server => node[:collectd][:server], :listen =>node.ipaddress, :data_dir => node[:collectd][:data_dir]
  else
    variables :server => node[:collectd][:server]
  end
  owner "root"
  group "root"
  mode 0644
end

template "/etc/collectd/collection.conf" do
  source "collection.conf.erb"
  owner "root"
  group "root"
  mode 0644
end

service "collectd" do
  supports :restart => true, :status=>true
  action [:enable, :start]
  action [:restart]  # To ensure configuration is reloaded
end

if true || recipie?(:apache)
  execute "copy collecion3 directory to www dir" do
    command "cp -R  /usr/share/doc/collectd/examples/collection3 /var/www/ && chmod -R www-data /var/www/collecion3"
    action :run
    not_if "[ -d /var/www/collecion3 ] "
  end
  
  template "/etc/apache2/conf.d/collection3.conf" do
    source 'collection3.conf.erb'
  end
end