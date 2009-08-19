module Callbacks
  module ClassMethods
    def additional_callbacks(arr=[])
      @additional_callbacks ||= arr
    end
    def callback_block(&block)
      @callback_block ||= block
    end
  end
  
  module InstanceMethods
    # Callbacks on bootstrap and configuration
    # Defines the callback accessors:
    #   call_before/after_bootstrap/configure_callbacks
    # 
    # When called, this method will first check to see if there 
    # are plugins and call those plugin's callbacks when called
    # The method (before/after_bootstrap/configure) is called
    # on self if the callback method is defined on self
    def callback(call_time, *args, &block)
      on_all_callbacks(call_time, *args, &block)
      callback_on_self(call_time, *args, &block)
    end
    
    def callbacks
      @callbacks ||= []
    end
    
    def on_all_callbacks(call_time, *args, &block)
      self.class.callback_block.call(self, call_time) if self.class.callback_block
    end
    
    private
    
    # TODO: Add docs
    def callback_on_self(call_time, *args, &block)
      if respond_to?(call_time)
        callbacks << call_time.to_sym
        case self.method(call_time).arity
        when 0
          self.send(call_time)
        when 1
          self.send(call_time, *args)
        else
          self.send(call_time, *args, &block)
        end
      end
    end
    
  end
  
  def self.included(receiver)
    receiver.extend         ClassMethods
    receiver.send :include, InstanceMethods
  end
end