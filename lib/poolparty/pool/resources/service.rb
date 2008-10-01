module PoolParty    
  module Resources
        
    class Service < Resource
      
      default_options({
        :ensure => "running",
        :name => nil,
        :enable => true
      })
           
    end
    
  end
end