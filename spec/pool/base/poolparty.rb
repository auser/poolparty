module PoolParty
  module Requirements
    
    plugin :heartbeat do
      
      has_service do
        name "mon"        
      end
      
    end
    
  end
end