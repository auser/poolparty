module PoolParty    
  module Resources
        
    class Host < Resource
            
      default_options({
        :name => "$hostname",
        :ip => "$ipaddress"
      })
            
    end
    
  end
end