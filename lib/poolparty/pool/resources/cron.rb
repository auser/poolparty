module PoolParty    
  module Resources
        
    class Cron < Resource
      
      default_options({
        :command => nil,
        :user => "root"
      })

    end
    
  end
end