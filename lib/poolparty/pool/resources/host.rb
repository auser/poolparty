module PoolParty    
  module Resources
        
    class Host < Resource
            
      default_options({
        :name => "master puppet",
        :ip => "$ipaddress",
        :alias => "$hostname"
      })
      
    end
    
  end
end