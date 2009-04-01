require "/etc/chef/cookbooks/chef-deploy/lib/chef-deploy.rb"
include_recipe "apache2"
include_recipe "passenger"
include_recipe "rails"
include_recipe "git"

deploy "/var/www/bort" do
  repo "git://github.com/fudgestudios/bort.git"
  branch "HEAD"
  enable_submodules true
  shallow_clone true
  action :manage
end

web_app "bort" do
  docroot "/var/www/bort/current/public"
  template "suspenders.conf.erb"
  server_name node[:fqdn]
  server_aliases [node[:hostname], "bort"]
  passenger_version "2.1.3"
  rails_env "production"
end