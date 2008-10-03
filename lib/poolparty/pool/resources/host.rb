module PoolParty    
  module Resources
        
    class Host < Resource
            
      default_options({
        :name => "$hostname",
        :ip => "$ipaddress"
      })
      
      def key
        name
      end
      
    end
    
  end
end