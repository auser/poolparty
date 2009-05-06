module PoolParty    
  module Resources
        
    class Group < Resource      
      
      default_options({
        :shell => "/bin/sh"
      })
      
      def present
        :manage
      end
      
      def absent
        :remove
      end
      
    end
    
  end
end