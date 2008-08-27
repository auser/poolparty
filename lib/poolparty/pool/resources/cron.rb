module PoolParty    
  module Resources
    
    def cron(opts={}, &block)
      resource(:cron) << PoolParty::Resources::Cron.new(opts, &block)
    end
    
    class Cron < Resource
      
      default_options({
        :name => "cron",
        :command => nil,
        :user => "poolparty",
        :hour => "*",
        :minute => "*",
        :month => "*",
        :monthday => "*",
        :weekday => "*"
      })
      
      
      def name(*args)
        command(*args)
      end
    end
    
  end
end