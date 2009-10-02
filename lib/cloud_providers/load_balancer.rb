module CloudProviders
  class LoadBalancer
    include Dslify
    include Callbacks
    
    # This is the ACTUAL cloud_provider
    attr_accessor :name, :options
    
    default_options(
      :balancer_port        => 80,
      :instance_server_port => 80,
      :protocol             => "http"
    )
    
    def initialize(name="Load balancer", opts={}, &block)
      @name = name
      @options = opts
      set_vars_from_options(opts)
      instance_eval(&block) if block
      after_loaded
    end
    
    def after_loaded
    end
    
    private
    
    def create_load_balancer
    end
    
    def attach_to_instance(instance)
    end
    
  end
end