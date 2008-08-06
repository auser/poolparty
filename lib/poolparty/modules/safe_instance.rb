=begin rdoc
  Make a command thread-safe
=end
require "monitor"
module PoolParty
  extend self
  
  module ThreadSafeInstance
    
    module ClassMethods
      def make_safe(meth)
       original_method = "_unsafe_#{meth}_"
       alias_method original_method, meth
       define_method(meth) {|*args| self.class.synchronize { self.send(original_method) } }
       self
      end  
    end
    
    module InstanceMethods
      def make_safe meth
        self.class.make_safe meth
      end
    end
    
    def self.included(receiver)
      receiver.extend         MonitorMixin
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
    end
  end
end