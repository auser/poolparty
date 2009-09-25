module CloudProviders
  class ElasticLoadBalancer < LoadBalancer
    default_options(
      :load_balancer_port => "80",
      :instance_port => "80",
      :protocol => "http"
    )

    def create_load_balancer
      opts = base_hash.merge(:availability_zones => availability_zone)
      opts.merge!(:listeners => [{:protocol => protocol, :load_balancer_port => load_balancer_port, :instance_port => instance_port}])
      opts.merge!(:load_balancer_name => name.camelcase)
      vputs("[EC2] Creating volume - #{name}")
      cmd = "elb-create-lb #{opts[:load_balancer_name]} \
                    --headers \
                    --listener \"protocol=#{protocol},lb-port=#{load_balancer_port},instance-port=#{instance_port}\" \
                    --availability-zones #{availability_zone}\
                    -I #{access_key} -S #{secret_access_key}"
      puts "cmd: #{cmd}"
      `#{cmd}`
    end
    
    def attach_to_instance(instances)
      instance_names = instances.map {|i| "'#{i.instance_id}'" }
      opts = {:instances => instance_names, :load_balancer_name => name.camelcase}
      vputs("[EC2] Attaching to instance: - #{name} to #{instance_names}")
      # grempe_elb.register_instances_with_load_balancer(opts)
      cmd = "elb-register-instances-with-lb #{opts[:load_balancer_name]} --instances #{instance_names.join(",")}"
      puts cmd
      `#{cmd}`
    end
    
    def base_hash
      {:access_key_id => access_key, :secret_access_key => secret_access_key}
    end

    def grempe_elb
      require_aws
      @grempe_elb ||= AWS::ELB::Base.new(base_hash)
    end

    def require_aws
      require PoolParty.lib_dir/"vendor"/"gems"/"amazon-ec2/lib"/"AWS"
    end
    
  end
end