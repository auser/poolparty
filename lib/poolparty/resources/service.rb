module PoolParty    
  module Resources
        
    class Service < Resource
      
      default_options({
        :ensures => "running",
        :enable => true
      })
      
      def present
        :start
      end
      def absent
        :stop
      end
      
    end
    
  end
end