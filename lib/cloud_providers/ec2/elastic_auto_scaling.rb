=begin rdoc
  Usage:
  
    cloud :app do
      using :ec2 do
        
        elastic_auto_scaling "name" do
          trigger "uptime" do
          end
        end
        
      end
    end
=end

module CloudProviders
  class ElasticAutoScaling < AutoScaling
    default_options(
      :launch_config => "PoolPartyAutoScalingGroup",
      :load_balancer => "PoolPartyLoadBalancer"
    )

    def create_launch_config
      opts = base_hash.merge(:availability_zones => availability_zone, :image_id => image_id, :instance_type => instance_type)
      vputs("[EC2] Creating auto-scaling group - #{name}")
      process('create-launch-config', opts)
    end
    
    def create_auto_scaling_group
      opts = base_hash.merge(:launch_config => launch_config, :load_balancer => load_balancer, :availability_zones => availability_zone, :min_size => minimum_instances, :max_size => maximum_instances)
      vputs("[EC2] Creating auto-scaling group - #{name}")
      process('create-auto-scaling-group', opts)
      vputs("[EC2] Creating or updating trigger")
      trigger.create_or_update! if trigger
    end
    
    def trigger(name=nil, &block)
      trigger ||= name ? Trigger.new(name, &block) : nil
    end
    
    def base_hash
      @base_hash ||= {:access_key_id => access_key, :secret_access_key => secret_access_key}
    end

    # def grempe_elb
    #   require_aws
    #   @grempe_elb ||= AWS::ELB::Base.new(base_hash)
    # end
    # 
    # def require_aws
    #   require PoolParty.lib_dir/"vendor"/"gems"/"amazon-ec2/lib"/"AWS"
    # end
    
    private
    
    def process(cmd, opts={})
        args={'show-xml'=>nil}.merge(opts)
        arg_string = args.inject(''){|str, arg| str<<"--#{arg[0].to_s.gsub(/_/, '-')} #{arg[1] if arg[1]}"; str}      
        cmd = "as-#{cmd.to_s.gsub(/_/, '-')}"
        `#{cmd} #{arg_string}`
    end
    
  end
  class Trigger
    include Dslify
    
    default_options(
      :measure => "CPUUtilization",
      :auto_scaling_group => "PoolPartyAutoscalingGroup",
      :statistic => "Average",
      :dimensions => "AutoScalingGroupName",
      :period => 60,
      :namespace => "AWS/EC2",
      :lower_threshold => 0.0,
      :upper_threshold => 1.5,
      :lower_breach_increment => "-1",
      :upper_breach_increment => 1,
      :breach_duration => 120
    )
    
    
    def create_or_update!
      `as-create-or-update-trigger #{name} \
                  --auto-scaling-group #{auto_scaling_group} \
                  --measure #{measure} \
                  --statistic #{statistic} \
                  --dimensions "#{dimensions}" \
                  --period #{period} --namespace "#{namespace}" \
                  --lower-threshold #{lower_threshold} --upper-threshold #{upper_threshold} \
                  --lower-breach-increment=#{lower_breach_increment} \
                  --upper-breach-increment=#{upper_breach_increment} \
                  --breach-duration #{breach_duration}
      `
    end

  end
end

# as-create-or-update-trigger scale-down \
#           --auto-scaling-group chachaminerva \
#           --measure CPUUtilization \
#           --statistic Average \
#           -dimensions "AutoScalingGroupName=chachaminerva" \
#           --period 60 \
#           --namespace "AWS/EC2" \
#           --lower-threshold 0.0 \
#           --upper-threshold 0.2 \
#           --lower-breach-increment=-1 \
#           --upper-breach-increment=1 \
#           --breach-duration 120