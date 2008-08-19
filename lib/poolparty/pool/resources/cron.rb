module PoolParty    
  module Resources
    
    def cron(opts={}, &block)
      returning PoolParty::Resources::Exec.new(opts, &block) do |r|
        resources << r
      end
    end
    
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