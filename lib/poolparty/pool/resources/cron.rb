module PoolParty    
  module Resources
        
    class Cron < Resource
      
      default_options({
        :command => nil,
        :user => "poolparty",
        :hour => "*",
        :minute => "*",
        :month => "*",
        :monthday => "*",
        :weekday => "*"
      })

    end
    
  end
end