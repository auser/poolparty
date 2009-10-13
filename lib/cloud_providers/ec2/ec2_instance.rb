module CloudProviders
  class Ec2Instance < RemoteInstance
    
    default_options(
      :security_groups => [],
      :private_ip => nil,
      :dns_name => nil,
      :instance_type => nil,
      :public_ip => nil,
      :key_name => nil,
      :launch_time => nil,
      :availability_zones => []
    )
    
    def initialize(raw_response={})
      @raw_response = raw_response
      
      self.instance_id = raw_response["instanceId"] rescue nil
      self.security_groups = raw_response.groupSet.item[0].groupId rescue nil
      self.image_id = raw_response["imageId"] rescue nil
      self.private_ip = raw_response["privateIpAddress"] rescue nil
      self.dns_name = raw_response["dnsName"] rescue nil
      self.instance_type = raw_response["instanceType"] rescue nil
      self.public_ip = raw_response["ipAddress"] rescue nil
      self.key_name = raw_response["keyName"] rescue nil
      self.launch_time = raw_response["launchTime"] rescue nil
      self.availability_zones = raw_response["placement"]["availabilityZone"] rescue nil
      self.status = raw_response["instanceState"]["name"] rescue nil
      super
    end
    
    def keypair(n=nil)
      @keypair ||= Keypair.new(self.key_name)
    end
    
    def reachable?
      ping_port self.public_ip, 22
    end
    
    def in_service?
      running?
    end
    
    def running?
      self.status == "running"
    end
    
    def pending?
      self.status == "pending"
    end
    
    def run!
      cloud_provider.ec2.run_instances(:image_id => image_id,
      :min_count => min_count,
      :max_count => max_count,
      :key_name => keypair.basename,
      :group_id => security_groups,
      :user_data => user_data,
      :instance_type => instance_type,
      :availability_zone => availability_zone,
      :base64_encoded => true)
    end
    def self.run!(hsh); new(hsh).run!; end
    
    def terminate!
      cloud_provider.ec2.terminate_instances(:instance_id => [self.instance_id])
      cloud_provider.reset!
    end
    def self.terminate!(hsh={}); new(hsh).terminate!; end
        
  end
end