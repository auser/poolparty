module PoolParty    
  module Resources
        
    class Symlink < Resource
      
      def from(*args)
        options.merge!(:ensure => args[0])
      end
      
      def class_type_name
        "file"
      end
      
      def disallowed_options
        [:name]
      end
      
    end
    
  end
end