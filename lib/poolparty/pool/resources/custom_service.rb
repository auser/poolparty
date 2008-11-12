module PoolParty    
  module Resources
        
    class Customservice < Resource
      
      default_options({
        :ensure => "running",
        :provider => "base"
      })
      
      def bin(arg)
        options.merge!(:binary => arg)
        options.merge!(:start => arg)
        options.merge!(:stop => arg.gsub(/start/, 'stop'))
        options.merge!(:restart => "#{arg.gsub(/start/, 'stop')} && #{arg}")
      end
                  
      def present
        "running"
      end
      def absent
        "stopping"
      end
      def class_type_name
        "Service"
      end
    end
    
  end
end