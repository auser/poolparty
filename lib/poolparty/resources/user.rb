module PoolParty    
  module Resources
        
    class User < Resource      
      
      dsl_methods :password, :home
      default_options({
        :shell => "/bin/sh"
      })
      
    end
    
  end
end