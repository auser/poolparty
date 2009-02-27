require File.dirname(__FILE__) + "/remoter_base"
require File.dirname(__FILE__) + "/remoter"

module PoolParty  
  module Remote
    
    include PoolParty::Remote::Remoter
    
    def using(t)
      @cloud = self
      if t && available_bases.include?(t.to_sym)
        unless using_remoter?
          self.class.send :attr_reader, :remote_base
          self.class.send :attr_reader, :parent_cloud
          mod = "#{t}".preserved_module_constant
          
          # mod.send :include, PoolParty::Remote::RemoterBase
          self.class.send :include, mod
          self.extend mod
          
          @remote_base = "#{t}".preserved_module_constant
          @parent_cloud = @cloud
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