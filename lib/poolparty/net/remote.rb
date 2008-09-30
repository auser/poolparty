require File.dirname(__FILE__) + "/remoter_base"
require File.dirname(__FILE__) + "/remoter"

module PoolParty  
  module Remote
    
    include PoolParty::Remote::Remoter
    include PoolParty::Remote::RemoterBase
    
    def using(t)
      if available_bases.include?(t.to_sym)
        unless using_remoter? || t.nil?
          self.class.send :attr_reader, :remote_base
          mod = "#{t}".preserved_module_constant
          
          mod.extend PoolParty::Remote::RemoterBase
          self.class.send :include, mod
          
          @remote_base = "#{t}".preserved_module_constant          
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
                
  end  
end