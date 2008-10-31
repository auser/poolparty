=begin rdoc
  Monitor class
  
  TODO: Fill this out
=end
module PoolParty
  module Monitors    
    
    module ClassMethods
      def expansions(arr=[])
        @expansions ||= rules(:expansions, arr)
      end

      def contractions(arr=[])
        @contractions ||= rules(:contractions, arr)
      end

      def expand_when(*args)
        expansions(args)
      end
      
      def contract_when(*args)
        contractions(args)
      end
    end
    
    module InstanceMethods
      def expansions;self.class.expansions;end
      def contractions;self.class.contractions;end
    end
    
    def self.register_monitor(*args)
      args.each do |arg|
        (available_monitors << "#{arg}".downcase.to_sym)
        
        ClassMethods.module_eval "def #{arg}; PoolParty::Messenger.messenger_send!(\"get_load #{arg}\"); end"
        InstanceMethods.module_eval "def #{arg}; self.class.#{arg}; end"
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
    
    def self.included(receiver)
      receiver.extend                 PoolParty::Monitors::ClassMethods      
      receiver.send :include,         PoolParty::Monitors::InstanceMethods
      receiver.send :include,         Aska
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