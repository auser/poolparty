module PoolParty    
  module Resources
    
    class Exec < Resource
      
      default_options({
        :path => "/usr/bin:/bin:/usr/local/bin"
      })
            
    end
    
  end
end