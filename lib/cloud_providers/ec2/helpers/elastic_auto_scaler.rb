module CloudProviders
  class ElasticAutoScaler < Ec2
    def run
      puts "-----> Checking for launch configuration named: #{name}"
      if should_create_launch_configuration?
        create_launch_configuration!
      end
      if should_create_autoscaling_group?
        create_autoscaling_group!
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
      known = launch_configurations.select {|lc| lc.name =~ /#{name}/ }
      if known.empty?
        true
      else
        differences = known.map do |k|
          p old_launch_configuration_name
          t = k.diff({
            :name => old_launch_configuration_name,
            :image_id => image_id,
            :instance_type => instance_type,
            :security_groups => security_groups.flatten,
            :key_name => keypair,
            :user_data => user_data,
            }, :name, :user_data, :image_id, :instance_type, :security_groups, :key_name)
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
      puts "-----> Creating launch configuration: #{new_launch_configuration_name} for #{proper_name}"
      begin
        as.create_launch_configuration({
          :launch_configuration_name => new_launch_configuration_name,
          :image_id => image_id,
          :instance_type => instance_type,
          :security_groups => security_groups,
          :key_name => keypair,
          :user_data => user_data,
          :kernel_id => kernel_id,
          :ramdisk_id => ramdisk_id,
          :block_device_mappings => block_device_mappings
        })
        as.delete_launch_configuration(:launch_configuration_name => old_launch_configuration_name)
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
          :key_name => a["KeyName"],
          :instance_type => a["InstanceType"]
        }
      end
    end
    def create_autoscaling_group!
      as.create_autoscaling_group({
        :autoscaling_group_name => name,
        :availability_zones => availability_zones,
        :launch_configuration_name => new_launch_configuration_name,
        :min_size => minimum_instances,
        :max_size => maximum_instances,
        :load_balancer_names => load_balancers.map {|k,v| k }
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
    # Temporary names so we can create and recreate launch_configurations
    def new_launch_configuration_name
      return @new_launch_configuration_name if @new_launch_configuration_name
      used_configuration_names = launch_configurations.map {|hsh| hsh[:name] =~ /#{name}/ ? hsh[:name] : nil }.reject {|a| a.nil?}
      used_ints = used_configuration_names.map {|a| a.gsub(/#{name}/, '').to_i }.reject {|a| a.zero? }
      used_ints = [0] if used_ints.empty?
      @new_launch_configuration_name = "#{name}#{used_ints[-1]+1}"
    end
    def old_launch_configuration_name
      return @old_launch_configuration_name if @old_launch_configuration_name
      used_configuration_names = launch_configurations.map {|hsh| hsh[:name] =~ /#{name}/ ? hsh[:name] : nil }.reject {|a| a.nil?}
      used_ints = used_configuration_names.map {|a| a.gsub(/#{name}/, '').to_i }.reject {|a| a.zero? } || [0]
      used_ints = [0] if used_ints.empty?
      @old_launch_configuration_name = "#{name}#{used_ints[-1]}"
    end
  end
end