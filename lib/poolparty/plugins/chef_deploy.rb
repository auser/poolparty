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
module PoolParty
  class ChefDeploy
    
    define_resource :chef_deploy_definition do
      
      dsl_methods :repo
      
      default_options(
                      :branch => "HEAD",
                      :enable_submodules => true,
                      :migrate => true,
                      :environment => "production",
                      :shallow_clone => true,
                      :user => "www-data",
                      :restart_command => "touch tmp/restart.txt",
                      :migration_command => "rake db:migrate"
                      )
      def present
        :deploy
      end
    end
    
    plugin :chef_deploy do
      dsl_methods :branch, :enable_submodules, :migrate, :environment, :shallow_clone, :user,
                  :restart_command, :migration_command, :repo
      
      def loaded(o={})
        if dsl_options.keys == [:ensures]
          has_chef_library "chef-deploy/lib/chef-deploy.rb"
        else
          raise ::ReposMissingError.new unless repo
          has_chef_library "chef-deploy/lib/chef-deploy.rb"
          has_chef_deploy_definition(dsl_options)
        end
      end
      
      def before_configure
        configure_commands [
          "mkdir -p /etc/chef/lib",
          "cp -R /var/poolparty/dr_configure/etc/chef/lib /etc/chef"
        ]
        ::Suitcase::Zipper.add("#{::File.dirname(__FILE__)}/../../../vendor/chef/chef-deploy", 
                                "etc/chef/lib")
      end
      
    end
    
  end
end

class ReposMissingError < StandardError
  def initialize
    super("You must include the repo to deploy with chef_deploy")
  end 
end