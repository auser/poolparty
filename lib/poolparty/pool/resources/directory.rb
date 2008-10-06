module PoolParty    
  module Resources
        
    class Directory < Resource
            
      default_options({
        :ensure => "directory",
        :mode => 644,
        :owner => "#{Base.user}"
      })
      
      def class_type_name
        "file"
      end
      
    end
    
  end
end