module CloudProviders
  class ElasticAutoScaler < Ec2
    def run
      puts "-----> Checking for launch configuration named: #{name}"
      if should_create_launch_configuration?
        create_launch_configuration!
      end
      if should_create_autoscaling_group?
        create_autoscaling_group
      end
    end
    def should_create_autoscaling_group?
      known = autoscaling_groups.select {|ag| ag.name == cloud.proper_name }
      if known.empty?
        true
      else
        puts "Autoscaling group already defined...: #{name}"
        false
      end
    end
    def should_create_launch_configuration?
      known = launch_configurations.select {|lc| lc.name == name }
      if known.empty?
        true
      else
        differences = known.map do |k|
         t = k.diff({
            :name => cloud.proper_name,
            :image_id => parent.image_id,
            :instance_type => parent.instance_type,
            :security_groups => parent.security_groups.flatten,
            :key_name => cloud.keypair,
            :user_data => parent.user_data,
          }, :user_data, :name, :image_id, :instance_type, :security_groups, :key_name)
          t.empty? ? nil : t
        end.reject {|a| a.nil? }
        if differences.empty?
          false
        else
          puts "-----> Recreating the launch configuration as details have changed: #{differences.inspect}"
          true
        end
      end
    end
    def create_launch_configuration!
      puts "-----> Creating launch configuration: #{cloud.proper_name}"
      begin
        # as.delete_autoscaling_group(:autoscaling_group_name => cloud.proper_name)
        # as.delete_launch_configuration(:launch_configuration_name => cloud.proper_name)
        as.create_launch_configuration({
          :launch_configuration_name => cloud.proper_name,
          :image_id => parent.image_id,
          :instance_type => parent.instance_type,
          :security_groups => parent.security_groups,
          :key_name => cloud.keypair,
          :user_data => parent.user_data,
          :kernel_id => parent.kernel_id,
          :ramdisk_id => parent.ramdisk_id,
          :block_device_mappings => parent.block_device_mappings
        })
      rescue Exception => e
        puts <<-EOE
-----> There was an error: #{e.inspect} when creating the launch_configurations
        EOE
      end
    end
    def launch_configurations
      @launch_configurations ||= as.describe_launch_configurations.DescribeLaunchConfigurationsResult.LaunchConfigurations.member.map do |a|
        {
          :name => a["LaunchConfigurationName"],
          :ramdisk_id => a["RamdiskId"],
          :image_id => a["ImageId"],
          :security_groups => (a["SecurityGroups"]["member"] rescue ["default"]),
          :created_time => a["CreatedTime"],
          :user_data => a["UserData"] || "",
          :keypair => a["KeyName"],
          :instance_type => a["InstanceType"]
        }
      end
    end
    def create_autoscaling_group
      as.delete_autoscaling_group(:autoscaling_group_name => cloud.proper_name) rescue nil
      as.create_autoscaling_group({
        :autoscaling_group_name => cloud.proper_name,
        :availability_zones => parent.availability_zones,
        :launch_configuration_name => cloud.proper_name,
        :min_size => parent.minimum_instances,
        :max_size => parent.maximum_instances,
        :load_balancer_names => parent.load_balancers.map {|k,v| k }
      })
    end
    def autoscaling_groups
      as.describe_autoscaling_groups.DescribeAutoScalingGroupsResult.AutoScalingGroups.member.map do |g|
        {
          :cooldown => g["Cooldown"],
          :desired_capacity => g["DesiredCapacity"],
          :created_time => g["CreatedTime"],
          :min_size => g["MinSize"],
          :max_size => g["MaxSize"],
          :load_balancer_names => (g["LoadBalancerNames"]["member"] rescue []),
          :availabilityZones => (g["AvailabilityZones"]["member"] rescue []),
          :launch_configuration_name => g["LaunchConfigurationName"],
          :name => g["AutoScalingGroupName"],
          :instances => (g["Instances"]["member"] rescue []).map {|i|
            {:instance_id => i["InstanceId"],
            :state => i["LifecycleState"],
            :availability_zone => i["AvailabilityZone"]
          }}
        }
      end
    end
  end
end