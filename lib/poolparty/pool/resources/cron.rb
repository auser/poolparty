module PoolParty    
  module Resources
        
    class Cron < Resource
      
      default_options({
        :command => nil,
        :user => "root",
        :hour => "*",
        :minute => "*",
        :month => "*",
        :monthday => "*",
        :weekday => "*"
      })

    end
    
  end
end