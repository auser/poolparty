module PoolParty    
  module Resources
        
    class File < Resource
            
      default_options({
        :ensure => "present",
        :mode => 644,
        :owner => "#{Base.user}"
      })
      
      def disallowed_options
        [:name, :template]
      end
      
      def source(arg=nil)
        arg ? options[:source] = arg : "#{Base.fileserver_base}/#{::File.basename(name)}"
      end
      
    end
    
  end
end