module PoolParty    
  module Resources
        
    class File < Resource
            
      default_options({
        :ensure => "present",
        :mode => 644,
        :owner => "poolparty"
      })
      
      def source(arg=nil)
        arg ? options[:source] = arg : ::File.join(Base.fileserver_base, (options[:source] || name))
      end
      
    end
    
  end
end