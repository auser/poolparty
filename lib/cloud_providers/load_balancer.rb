module CloudProviders
  class LoadBalancer
    include Dslify
    include Callbacks
    
    # This is the ACTUAL cloud_provider
    attr_accessor :caller, :name, :options
    
    default_options(
      :balancer_port        => 80,
      :instance_server_port => 80,
      :protocol             => "http"
    )
    
    def initialize(name="Load balancer", caller=nil, opts={}, &block)
      @name = name
      @caller = caller
      @options = opts
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
    
    def method_missing(m,*a,&block)
      if caller.respond_to?(m)
        caller.send m, *a, &block
      elsif options.has_key?(m)
        options[m]
      else
        super
      end
    end
    
  end
end