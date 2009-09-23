module CloudProviders
  class ElasticLoadBalancer < LoadBalancer
    default_options(
      :availability_zones => ["us-east-1a"],
      :balancer_port        => 80,
      :instance_server_port => 80,
      :protocol             => "http"
    )

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