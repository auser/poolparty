module CloudProviders
  class AutoScaling
    include Dslify
    
    default_options(
    )
    
    # This is the ACTUAL cloud_provider
    attr_accessor :caller, :name, :options
    
    def initialize(name="PoolPartyAutoscalingGroup", caller=nil, opts={}, &block)
      @name = name
      @caller = caller
      @options = opts
      instance_eval(&block) if block
      after_loaded
    end
    
    def after_loaded
    end
    
  end
  
  class Trigger
    include Dslify
    
    default_options(
    )
    
    # This is the ACTUAL cloud_provider
    attr_accessor :caller, :name, :options
    
    def initialize(name="PoolPartyTrigger", caller=nil, opts={}, &block)
      @name = name
      @caller = caller
      @options = opts
      instance_eval(&block) if block
      after_loaded
    end
    
    def after_loaded
    end
    
  end
end