module CloudProviders
  class LoadBalancer
    include Dslify
    include Callbacks
    
    # This is the ACTUAL cloud_provider
    attr_accessor :caller, :name
    
    default_options(
      :balancer_port        => 80,
      :instance_server_port => 80,
      :protocol             => "http"
    )
    
    def initialize(name="", caller=nil, opts={}, &block)
      @name = name
      @caller = caller
      set_vars_from_options(opts)
      instance_eval(&block) if block
      after_loaded
    end
    
    def get_or_create_volume
      create_volume(:availability_zones => caller.availability_zones)
    end
    
    def after_loaded
    end
    
    private
    def create_volume(o)
      grempe_elb.create_volume(o)
    end
    
    def grempe_elb
      require_aws
      @grempe_elb ||= AWS::ELB::Base.new(:access_key_id => caller.access_key, :secret_access_key => caller.secret_access_key)
    end
    
    def require_aws
      require PoolParty.lib_dir/"vendor"/"gems"/"amazon-ec2/lib"/"AWS"
    end
    
    
  end
end