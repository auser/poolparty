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
      self.instance_id = raw_response["instanceId"]
      self.security_groups = raw_response.groupSet.item[0].groupId
      self.image_id = raw_response["imageId"]
      self.private_ip = raw_response["privateIpAddress"]
      self.dns_name = raw_response["dnsName"]
      self.instance_type = raw_response["instanceType"]
      self.public_ip = raw_response["ipAddress"]
      self.key_name = raw_response["keyName"]
      self.launch_time = raw_response["launchTime"]
      self.availability_zones = raw_response["placement"]["availabilityZone"]
      self.status = raw_response["instanceState"]["name"]
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
    
    def start!(opts={})
      
    end
    
  end
end