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
                      
      def loaded(o={})
        raise "You must specify a git repo" unless repo?
        has_chef_library :name => "chef-deploy/lib/chef-deploy.rb"        
        has_chef_deploy_definition(options)
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