module CloudProviders
  module Ec2Helpers
    
    # VOLUMES    
    def attach_volume(instance_id, volume_id=next_unused_volume, device="/dev/sdh")
      ec2.attach_volume(volume_id, instance_id, device)
    end
    
    def next_unused_volume
      if all_volumes.empty?
        nil
      else
        available_volumes.first
      end
    end    
        
    private
    
    def all_volumes
      ebs_volumes.empty? ? [] : ec2.describe_volumes.select {|v| ebs_volumes.include?(v[:aws_id]) }
    end
    
    def available_volumes
      all_volumes.select {|v| v[:aws_status] == 'available' }
    end
    
    def unavailable_volumes
      all_volumes.reject {|v| available_volumes.include?(v) }
    end
    
    public
    
    # SECURITY GROUPS
    def security_groups(list=[])
      ec2.describe_security_groups(list)
    end
    
    public
    
    # ELASTIC IPS
    
    # Associate an address with the instance using ec2
    # Get the next_unused_elastic_ip
    # and if there is one, associate the instance to the 
    # public ip
    def associate_address(instance_id)
      new_ip = next_unused_elastic_ip
      vputs("Assigning #{new_ip} to the ec2 instance #{instance_id}")
      ec2.associate_address(instance_id, new_ip)
      loop do
        if describe_instance(:instance_id => instance_id).public_ip == new_ip
          return new_ip
        end
        sleep 1
      end
    end
    
    # Get the next usable elastic ip
    # First, get the list of addresses from ec2 that the client
    # has access to, then select only the ones that are not associated
    # with an instance.
    # If the cloud has set elastic_ips to use, then, using the 
    # intersection of the unused ips and those, find the first one available
    # and return that.
    def next_unused_elastic_ip
      if unused_elastic_ips.empty?
        nil
      else
        vputs("Found an unused elastic ip: #{unused_elastic_ips.first}")
        unused_elastic_ips.first
      end
    end
    
    private
    
    def all_elastic_ips
      elastic_ips.empty? ? [] : ec2.describe_addresses & elastic_ips
    end
    
    def unused_elastic_ips
      all_elastic_ips.select {|i| i[:instance_id] == nil }
    end
    
    # Help create a keypair for the cloud
    # This is a helper to create the keypair and add them to the cloud for you
    # def create_keypair
    #   return false unless keypair
    #   unless ::File.exists?( new_keypair_path )
    #     FileUtils.mkdir_p ::File.dirname( new_keypair_path )
    #     vputs "Creating keypair: #{keypair} in #{new_keypair_path}"
    #     Kernel.system "ec2-add-keypair #{keypair} > #{new_keypair_path} && chmod 600 #{new_keypair_path}"
    #   end
    # end
    
    # wrapper for remote base to perform a snapshot backup for the ebs volume
    # def create_snapshot
    #   return nil if ebs_volume_id.nil?
    #   ec2.create_snapshot(:volume_id => ebs_volume_id)
    # end
    
  end
end