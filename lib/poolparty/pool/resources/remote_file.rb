module PoolParty    
  module Resources
        
    class Remotefile < File
            
      default_options({
        :ensure => "present",
        :mode => 644,
        :owner => "#{Base.user}",
        :source => nil
      })
      
      def class_type_name
        "file"
      end
      
    end
    
  end
end