module CloudProviders
  class Ec2Instance < RemoteInstance
    
    def reachable?
      p [dsl_options.keys]
      p [self.instance_id, self.status, self.public_ip, running?]
      ping_port self.public_ip, 22
    end
    
    def running?
      self.status == "running"
    end
    
  end
end