module PoolParty
  module Resources
    class ChefDeployDefinition < Resource
      
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
  end
end