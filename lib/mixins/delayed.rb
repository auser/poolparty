=begin rdoc
  Allow delayed actions to be describe and run only after
  the initialize block has completed.
  
  Usage:
    class Test
      include Delayed
      
      def initialize(&block)
        instance_eval &block if block
        loaded = true
        
        run_after_loaded do |b|
          instance_eval &b if b
        end
        
      end
    end
=end

class DelayedProc < Proc
  def initialize(proxy, &block)
    @proxy = proxy
    @proc = block
  end
  def result
    @result ||= @proc.call
  end
  def method_missing(m,*a,&block)
    result.send(m,*a,&block)
  end
end
module Delayed
  module ClassMethods
    def delayed_calls
      @delayed_calls ||= {}
    end
  end
  
  module InstanceMethods
    def loaded
      @loaded ||= false
    end
    def loaded!
      @loaded = true
    end
    def run_after_loaded(&block)
      self.class.delayed_calls.each do |k,b|
        self.class.delayed_calls[block.object_id] ||= b.result(&block) if b && b.is_a?(Proc)
      end
    end
    def delayed_action(&block)
      if loaded
        self.class.delayed_calls[block.object_id] ||= block.call
      else
        self.class.delayed_calls[block.object_id] = DelayedProc.new(self, &block) if block
      end
    end
  end
  
  def self.included(receiver)
    receiver.extend         ClassMethods
    receiver.send :include, InstanceMethods
  end
end