module CloudProviders
  class AutoScaler
    include Dslify
    include Callbacks
    
    def initialize(name="Autoscaling scaler", opts={}, &block)
      @name = name
      @options = opts
      set_vars_from_options(opts)
      instance_eval(&block) if block
      after_loaded
    end
    
    def after_loaded
    end
    
  end
end