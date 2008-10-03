module PoolParty    
  module Resources
        
    class File < Resource
            
      default_options({
        :ensure => "present",
        :mode => 644,
        :owner => "poolparty"
      })
      
      def source(arg=nil)
        arg ? options[:source] = arg : "#{Base.fileserver_base}files"
      end
      
    end
    
  end
end