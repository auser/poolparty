module PoolParty    
  module Resources
        
    class File < Resource
            
      default_options({
        :ensure => "present",
        :mode => 644,
        :owner => "poolparty",
        :content => ""
      })
      
      def source
        File.join(Base.fileserver_base, name)
      end
      
    end
    
  end
end