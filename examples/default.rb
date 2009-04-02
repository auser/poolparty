require "/etc/chef/cookbooks/chef-deploy/lib/chef-deploy.rb"
include_recipe "apache2"
include_recipe "passenger"
include_recipe "rails"
include_recipe "git"
include_recipe "ec2"
include_recipe "sqlite"

gem_package "sqlite3-ruby" do
  action :install
end

web_app "paparazzi" do
  docroot "/srv/paparazzi/public"
  template "paparazzi.conf.erb"
  server_name "www.paparazzi.com" #node[:fqdn]
  server_aliases [node[:hostname], "paparazzi.com"]
  passenger_version "2.1.3"
  rails_env "production"
end

directory "/srv" do
  mode 755
end