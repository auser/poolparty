module PoolParty    
  module Resources
        
    class User < Resource      
      
      default_options({
        :shell => "/bin/sh",
        :password => nil,
        :home => nil,
      })
      
    end
    
  end
end