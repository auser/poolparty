module PoolParty    
  module Resources
        
    class Directory < Resource
            
      default_options({
        :ensure => "directory",
        :mode => 644,
        :owner => "#{Base.user}"
      })
      
    end
    
  end
end