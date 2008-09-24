module PoolParty    
  module Resources
        
    class Service < Resource
      
      default_options({
        :ensure => "running",
        :enable => "true",
        :name => nil,
        :require => nil
      })
           
    end
    
  end
end