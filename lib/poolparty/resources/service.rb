module PoolParty    
  module Resources
        
    class Service < Resource
      
      default_options({
        :ensures => "running",
        :enable => true
      })
      
      def present
        "running"
      end
      def absent
        "stopping"
      end
    end
    
  end
end