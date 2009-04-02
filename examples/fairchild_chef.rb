# require "/etc/chef/cookbooks/chef-deploy/lib/chef-deploy.rb"
include_recipe "apache2"
include_recipe "passenger"
include_recipe "rails"
# include_recipe "git"
# include_recipe "ec2"
# include_recipe "sqlite"

# gem_package "sqlite3-ruby" do
#   action :install
# end

web_app "paparazzi" do
  docroot "/var/www/paparazzi/public"
  template "paparazzi.conf.erb"
  server_name "www.paparazzi.com"
  server_aliases [node[:hostname], node[:fqdn], "paparazzi.com"]
  rails_env "production"
end