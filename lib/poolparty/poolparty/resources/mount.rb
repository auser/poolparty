module PoolParty    
  module Resources
        
    class Mount < Resource      
      
      default_options({
        :name => "/data",
        :remounts => "true",
        :options => "rw,nosuid,noquota",
        :fstype => "xfs",
        :atboot => "yes"
      })
      
      def disallowed_options
        [:name]
      end
            
      
    end
    
  end
end