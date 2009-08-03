module PoolParty
  module Resources
    
    class Site < Rails
      
      default_options(
        :rails_version => "2.3.3",
        :on => :passenger,
        :at => nil
      )
      
      def after_loaded(o={})
        
        raise PoolPartyError.create("RailsSiteError", "You must specify a root directory with at") unless self.at
        case on
        when :passenger
          site_name = name
          apache do
            passenger_site(:name => site_name, :dir => at, :deploy_dirs => true)
          end
        else
          raise PoolPartyError.create("RailsSiteError", "You must specify a supported rails provider. Supported: [:passenger]")
        end
      end
      
    end
    
  end
end