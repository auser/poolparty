module PoolParty    
  module Resources
        
    class Remotefile < File
            
      default_options({
        :ensure => "present",
        :mode => 644,
        :owner => "poolparty",
        :source => nil
      })
      
    end
    
  end
end