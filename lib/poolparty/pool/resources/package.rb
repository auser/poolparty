module PoolParty    
  module Resources
        
    class Package < Resource
      
      default_options({
        :ensure => "installed",
        :name => nil
      })      
      
    end
    
  end
end