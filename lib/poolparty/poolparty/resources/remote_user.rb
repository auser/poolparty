module PoolParty    
  module Resources
        
    class RemoteUser < Resource      
      
      default_options({
        :shell => "/bin/sh"
      })
      
      def class_type_name
        "user"
      end
      
    end
    
  end
end