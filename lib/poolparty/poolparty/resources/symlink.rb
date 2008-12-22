module PoolParty    
  module Resources
        
    class Symlink < Resource
            
      def class_type_name
        "file"
      end
      
      def disallowed_options
        [:name, :source]
      end
      
      # We can set the source several ways, with either source or from
      # in the manifest
      def present
        source || from
      end
      
      def ensure
        present
      end
      
      def printable?
        true
      end
      
    end
    
  end
end