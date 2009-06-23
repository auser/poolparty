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
      
      def to_properties_hash
        {:pp_type => "deploy", :name => "#{to}"}.merge!(default_options)
      end
      
      def to(n=nil)
        @to ||= n.nil? ? "/var/www" : n
      end
                      
      def present
        :deploy
      end
      
    end
  end
end