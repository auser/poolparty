require File.dirname(__FILE__) + "/remoter_base"
require File.dirname(__FILE__) + "/remoter"

module PoolParty  
  module Remote
    
    def using(type)
      if available_bases.include?(type.to_sym)
        unless using_remoter? || type.nil?
          self.instance_eval do |t|
            t.extend "#{type}".preserved_module_constant
            t.class.send :include, "#{type}".preserved_module_constant
          end
          @remote_base = type          
        end
      else
        puts "Unknown remote base" 
      end
    end
    
    def available_bases
      remote_bases
    end
    
    def using_remoter?
      @remote_base ||= nil
    end
    
    def self.included(receiver)
      receiver.send :include, PoolParty::Remote::RemoterBase
      receiver.send :include, PoolParty::Remote::Remoter
      receiver.extend self
    end
        
  end  
end