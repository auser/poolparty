module PoolParty    
  module Resources
        
    class Symlink < Resource
            
      def class_type_name
        "file"
      end
      
      def disallowed_options
        [:name, :source]
      end
      
      def present
        source
      end
      
    end
    
  end
end