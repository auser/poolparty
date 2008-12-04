module PoolParty    
  module Resources
        
    class Directory < Resource
            
      default_options({
        :mode => 644,
        :owner => "#{Base.user}"
      })
      
      def class_type_name
        "file"
      end
      
      def ensure
        "directory"
      end
      
      def present
        'directory'
      end
      
    end
    
  end
end