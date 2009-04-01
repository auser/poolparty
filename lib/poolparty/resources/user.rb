module PoolParty    
  module Resources
        
    class User < Resource      
      
      default_options({
        :shell => "/bin/sh"
      })
      
    end
    
  end
end