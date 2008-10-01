module PoolParty    
  module Resources
        
    class Host < Resource
            
      default_options({
        :name => "master puppet",
        :ip => "$ipaddress"
      })
      
    end
    
  end
end