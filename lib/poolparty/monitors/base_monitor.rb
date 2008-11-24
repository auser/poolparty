=begin rdoc
  Monitor class
  
  Monitors are the basis for PoolParty scaling. Your cloud will expand and
  contract against these monitors. You can set your cloud to be monitored by these
  monitors simply by using them in the contract_when and the expand_when macros 
  on your cloud like so:
  
    expand_when "cpu > 1.2", "memory > 0.94"
    contract_when "cpu < 0.4", "memory < 0.3"
  
  You can also add your own monitors simply by creating a directory in the same
  directory as the pool spec (the same directory as the plugin directory exists) and
  placing your monitor file (format: [monitorname]_monitor.rb) there.
  
  Monitors are simply classes of the name of the monitor. They subclass the BaseMonitor
  class from PoolParty. A sample monitor would look similar to:
    
    class SampleMonitor < PoolParty::Monitors::BaseMonitor
      def run
      end
    end
    register_monitor :sample
  
  The monitor class must have an instance level method called run. This method is called when 
  the cloud is checking the monitor. The output of this method should be the output of
  the monitor.
  
  Notice that at the end, you must call register_monitor :monitorname. This will tell your cloud
  that it can monitor it with this monitor.
=end
require "#{::File.dirname(__FILE__)}/../pool/base"

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

# Require included monitors
Dir["#{File.dirname(__FILE__)}/monitors/*.rb"].each {|f| require f}
# Require custom monitors
Dir["#{PoolParty::Base.custom_monitor_directories}/*.rb"].each {|f| require f}

module PoolParty
  module Cloud
    class Cloud
      include PoolParty::Monitors
    end
  end
end