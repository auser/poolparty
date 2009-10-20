module CloudProviders
  class ElasticAutoScaler < Ec2Helper
    default_options(
      :cooldown => nil,
      :desired_capacity => nil
    )
    def run      
      if should_create_autoscaling_group?
        puts "Creating autoscaling group"
        create_launch_configuration!
        create_autoscaling_group!
      else
        puts "-----> Checking for launch configuration named: #{old_launch_configuration_name}"
        if should_create_launch_configuration?
          create_launch_configuration!
        elsif should_update_launch_configuration? || should_update_autoscaling_group?
          update_launch_configuration!
          
          puts "Updating autoscaling group"
          update_autoscaling_group!
          puts "Deleting old launch configuration: #{old_launch_configuration_name}"
          as.delete_launch_configuration(:launch_configuration_name => old_launch_configuration_name)
        end
        
      end
    end
    
    # First, change the min_count to 
    def teardown
      if autoscaling_groups.select {|n| n.name == name }.empty?
        puts "Cloud #{cloud.name} autoscaling group does not exist"
      else
        self.minimum_instances = 0
        self.maximum_instances = 0
        @new_launch_configuration_name = old_launch_configuration_name
        puts "Updating autoscaling group: #{@new_launch_configuration_name}"
        update_autoscaling_group!
        puts "Terminating nodes in autoscaling group: #{name}"
        reset!
        # cloud.nodes.each {|n| n.terminate! }
        delete_autoscaling_group!
        delete_launch_configuration!
        puts ""
      end
    end
    
    private
    def delete_autoscaling_group!
      ensure_no_scaling_activities
      reset!
      begin
        as.delete_autoscaling_group(:autoscaling_group_name => name)
      rescue AWS::Error => e
        if e.message =~ /You cannot delete an AutoScalingGroup while there are scaling activities in progress/
          delete_autoscaling_group!
        end
      rescue Exception => e
        p e.inspect
      end
    end
    def delete_launch_configuration!(n=new_launch_configuration_name)
      ensure_no_scaling_activities
      as.delete_launch_configuration(:launch_configuration_name => n)
    end
    def ensure_no_scaling_activities
      # loop do
      #   reset!
      #   activities = scaling_activities.select {|a| !a[:complete] }
      #   running_nodes = cloud.nodes.select {|n| n.running? }
      #   if activities.empty? && running_nodes.empty?
      #     break
      #   else
      #     $stdout.print "."
      #     $stdout.flush
      #     sleep 1
      #   end
      # end
      progress_bar_until("") do
        reset!
        activities = scaling_activities.select {|a| !a[:complete] }
        running_nodes = cloud.nodes.select {|n| n.running? }
        activities.empty? && running_nodes.empty?
      end
      reset!
    end
    public
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
        false
      end
    end
    def should_update_launch_configuration?
      known = launch_configurations.select {|lc| lc.name =~ /#{name}/ }
      if known.empty?
        true
      else
        differences = known.map do |k|
          t = k.diff({
            :image_id => image_id,
            :instance_type => instance_type,
            :security_groups => parent.security_groups.flatten,
            :key_name => keypair.to_s,
            :user_data => user_data,
            }, :user_data, :image_id, :instance_type, :security_groups, :key_name)
          t.empty? ? nil : t
        end.reject {|a| a.nil? }
        if differences.empty?
          false
        else
          true
        end
      end
    end
    def update_launch_configuration!
      create_launch_configuration!
    end
    def create_launch_configuration!(lc=new_launch_configuration_name)
      puts "-----> Creating launch configuration: #{new_launch_configuration_name} for #{proper_name}"
      begin
        @launch_configuration_name = lc
        as.create_launch_configuration({
          :launch_configuration_name => new_launch_configuration_name,
          :image_id => image_id,
          :instance_type => instance_type,
          :security_groups => parent.security_groups,
          :key_name => keypair.to_s,
          :user_data => user_data,
          :kernel_id => kernel_id,
          :ramdisk_id => ramdisk_id,
          :block_device_mappings => block_device_mappings
        })
      rescue Exception => e
        puts <<-EOE
-----> There was an error: #{e.inspect} when creating the launch_configurations
        EOE
      ensure
        reset!
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
        :min_size => minimum_instances.to_s,
        :max_size => maximum_instances.to_s,
        :load_balancer_names => load_balancers.map {|k,v| k }
      })
      reset!
    end
    
    def should_update_autoscaling_group?
      known = autoscaling_groups.select {|lc| lc.name =~ /#{name}/ }
      if known.empty?
        true
      else
        differences = known.map do |k|
          hsh = {
            :min_size => minimum_instances.to_s, :max_size => maximum_instances.to_s,
            :load_balancer_names => load_balancers, 
            :availability_zones => availability_zones,
            :launch_configuration_name => old_launch_configuration_name
          }
          hsh.merge!(:cooldown => cooldown.to_s) if cooldown
          
          t = hsh.diff(k, :cooldown,
                          :min_size,
                          :max_size, 
                          :load_balancer_names, 
                          :availability_zones, 
                          :launch_configuration_name)
          t.empty? ? nil : t
        end.reject {|a| a.nil? }
        if differences.empty?
          false
        else
          puts "-----> Recreating the autoscaling group as details have changed: #{differences.inspect}"
          true
        end
      end
    end
    
    def update_autoscaling_group!
      as.update_autoscaling_group(
        :autoscaling_group_name => name,
        :availability_zones => availability_zones.first, # TODO: Figure out how to support multiple availability_zones
        :launch_configuration_name => new_launch_configuration_name,
        :min_size => minimum_instances.to_s,
        :max_size => maximum_instances.to_s,
        :cooldown => cooldown.to_s,
        :load_balancer_names => load_balancers.map {|k,v| k }
      )
    end
    
    def autoscaling_groups
      @autoscaling_groups ||= as.describe_autoscaling_groups.DescribeAutoScalingGroupsResult.AutoScalingGroups.member.map do |g|
        {
          :cooldown => g["Cooldown"],
          :desired_capacity => g["DesiredCapacity"],
          :created_time => g["CreatedTime"],
          :min_size => g["MinSize"],
          :max_size => g["MaxSize"],
          :load_balancer_names => (g["LoadBalancerNames"]["member"] rescue []),
          :availability_zones => (g["AvailabilityZones"]["member"] rescue []),
          :launch_configuration_name => g["LaunchConfigurationName"],
          :name => g["AutoScalingGroupName"],
          :instances => (g["Instances"]["member"] rescue []).map {|i|
            {:instance_id => i["InstanceId"],
            :state => i["LifecycleState"],
            :availability_zone => i["AvailabilityZone"]
          }}
        }
      end rescue []
    end
    def scaling_activities
      @scaling_activities ||= as.describe_scaling_activities(:autoscaling_group_name => name).DescribeScalingActivitiesResult.Activities.member.map do |action|
        {
          :cause        => action["Cause"],
          :progress     => action["Progress"].to_i,
          :activity_id  => action["ActivityId"],
          :description  => action["Description"],
          :status_code  => action["StatusCode"],
          :complete     => action["StatusCode"] == "Pending" ? false : true,
          :start_time   => action["StartTime"]
        }
      end rescue []
    end
    # Temporary names so we can create and recreate launch_configurations
    def new_launch_configuration_name
      @new_launch_configuration_name ||= "#{name}#{used_launched_config_id.zero? ? 1 : 0}"
    end
    def old_launch_configuration_name
      @old_launch_configuration_name ||= "#{name}#{used_launched_config_id.zero? ? 0 : 1}"
    end
    # Compute the next configuration launch id. We'll be cycling through the usage of 0 and 1
    # Here we are just looking for which one that is, either zero or 1
    def used_launched_config_id
      return @used_launched_config_id if @used_launched_config_id
      used_configuration_names = launch_configurations.map {|hsh| hsh[:name] =~ /#{name}/ ? hsh[:name] : nil }.reject {|a| a.nil?}
      used_launched_config_id = used_configuration_names.map {|a| a.gsub(/#{name}/, '').to_i }.reject {|a| a.zero? }.first
      used_launched_config_id = 0 if used_launched_config_id.nil?
      @used_launched_config_id = used_launched_config_id
    end
    
    def new_auto_scaling_group_name
      @new_auto_scaling_group_name ||= "#{name}#{used_autoscaling_group_id.zero? ? 1 : 0}"
    end
    
    def old_auto_scaling_group_name
      @old_auto_scaling_group_name ||= "#{name}#{used_autoscaling_group_id.zero? ? 0 : 1}"
    end
    
    def used_autoscaling_group_id
      return @used_autoscaling_group_id if @used_autoscaling_group_id
      used_autoscaling_groups = launch_configurations.map {|hsh| hsh[:name] =~ /#{name}/ ? hsh[:name] : nil }.reject {|a| a.nil?}
      used_autoscaling_group_id = used_autoscaling_groups.map {|a| a.gsub(/#{name}/, '').to_i }.reject {|a| a.zero? }.first
      used_autoscaling_group_id = 0 if used_autoscaling_group_id.nil?
      @used_autoscaling_group_id ||= used_autoscaling_group_id
    end
    
    # Convenience methods
    # Return an array of hashes describing the autoscaling groups
    def list
      describe_autoscaling_groups.DescribeAutoScalingGroupsResult.AutoScalingGroups.member
    end
    
    private
    def reset!
      @old_auto_scaling_group_name = 
        @new_auto_scaling_group_name = 
          @autoscaling_groups = @scaling_activities =
            @launch_configurations = nil
      cloud.reset!
    end
  end
end