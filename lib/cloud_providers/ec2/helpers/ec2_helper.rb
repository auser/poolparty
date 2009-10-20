module CloudProviders
  class Ec2Helper < CloudProvider
    
    def elb
      cloud.elb
    end
    
    def ec2
      cloud.ec2
    end
    
    def as
      cloud.as
    end
    
  end
end