module PoolParty    
  module Resources
        
    class Package < Resource
      
      def present
        :install
      end
      
      def absent
        :remove
      end
      
    end
    
  end
end