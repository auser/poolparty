=begin rdoc
  Chef deploy (ezmobius http://github.com/ezmobius/chef-deploy/tree/master)
  
  deploy "/data/#{app}" do
    repo "git://github.com/engineyard/rack-app.git"
    branch "HEAD"
    user "ez"
    enable_submodules true
    migrate true
    migration_command "rake db:migrate"
    environment "production"
    shallow_clone true
    revision '0xbeadbeef'
    action :deploy # or :rollback
  end
  
=end
require "chef_deploy_definition"

module PoolParty
  module Plugin
    
    class ChefDeploy < Plugin
      
      dsl_methods :branch, :enable_submodules, :migrate, :environment, :shallow_clone, :user,
                  :restart_command, :migration_command, :repo
      
      def loaded(o={})
        if dsl_options.reject {|k,v| v.nil? }.keys == [:ensures]
          has_chef_library "chef-deploy/lib/chef-deploy.rb"
        else
          raise ::ReposMissingError.new unless repo
          has_chef_library "chef-deploy/lib/chef-deploy.rb"
          has_chef_deploy_definition(default_options.merge(:to => name))
        end
      end
      
      def before_configure
        configure_commands [
          "mkdir -p /etc/chef/lib",
          "if [ -d /var/poolparty/dr_configure/etc/chef/lib ] ; then cp -R /var/poolparty/dr_configure/etc/chef/lib /etc/chef; fi"
        ]
        ::Suitcase::Zipper.add("#{::File.dirname(__FILE__)}/../../../../vendor/chef/chef-deploy", "etc/chef/lib")
      end
      
    end
    
  end
end

class ReposMissingError < StandardError
  def initialize
    super("You must include the repo to deploy with chef_deploy")
  end 
end