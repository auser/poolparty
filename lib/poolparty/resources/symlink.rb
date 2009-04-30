module PoolParty    
  module Resources
        
    class Symlink < Resource
      
      def initialize *args, &block
        super
      end
      
      def present
        :create
      end
      
      def absent
        :delete!
      end
      
    end
    
  end
end