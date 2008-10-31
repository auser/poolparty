=begin rdoc
  Monitor class
  
  TODO: Fill this out
=end
module PoolParty
  module Monitors
    
    def self.register_monitor(*args)
      args.each do |arg|
        (available_monitors << "#{arg}".downcase.to_sym)
      end
    end

    def self.available_monitors
      $available_monitors ||= []
    end
    
    class BaseMonitor      
      def self.run
        new.run
      end            
    end
    
  end
end

Dir["#{File.dirname(__FILE__)}/monitors/*.rb"].each {|f| require f}

module PoolParty
  module Cloud
    class Cloud
      include PoolParty::Monitors
    end
  end
end