=begin rdoc
  Monitor class
  
  TODO: Fill this out
=end
module PoolParty
  module Monitors    
    
    module ClassMethods
    end
    
    module InstanceMethods
      def expand_when(*arr)
        @expand_when ||= ((arr && arr.empty?) ? options[:expand_when] : configure(:expand_when => self.class.send(:rules,:expand_when,arr,false)))
      end
      
      def contract_when(*arr)
        @contract_when ||= ((arr&&arr.empty?) ? options[:contract_when] : configure(:contract_when => self.class.send(:rules,:contract_when,arr,false)))
      end
    end
    
    def self.register_monitor(*args)
      args.each do |arg|
        (available_monitors << "#{arg}".downcase.to_sym unless available_monitors.include?("#{arg}".downcase.to_sym))
        
        InstanceMethods.module_eval "def #{arg}; @#{arg} ||= get_live_#{arg}; end"
        InstanceMethods.module_eval "def get_live_#{arg}; messenger_send!(\"get_current_load #{arg}\").to_f rescue -1.0; end"
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