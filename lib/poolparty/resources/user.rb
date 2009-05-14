module PoolParty    
  module Resources
        
    class User < Resource      
      
      dsl_methods :name,        # Name of the user
                  :password,    # Password for the user
                  :home         # Home directory
                  
                  
      default_options({
        :shell => "/bin/sh"
      })
      
    end
    
  end
end