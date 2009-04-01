module PoolParty    
  module Resources
        
    class Directory < Resource
            
      default_options({
        :mode => 644
      })
      
      def present
        "directory"
      end
      
    end
    
  end
end