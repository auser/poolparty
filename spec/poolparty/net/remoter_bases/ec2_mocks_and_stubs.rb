class TestEC2Class < Ec2
  include CloudResourcer
  include CloudDsl
  include Dslify
  
  default_options :access_key => "Not an access key", 
                  :secret_access_key => "not a secret access key", 
                  :keypair => 'id_rsa'
  
  # def ami;"ami-abc123";end
  # def size; "small";end
  # def security_group; "default";end
  # def ebs_volume_id; "ebs_volume_id";end
  # def availabilty_zone; "us-east-1a";end
  # def verbose; false; end
  def describe_instances(o={})
    response_list_of_instances
  end
end

class TestEc2RemoteInstance < PoolParty::Remote::Ec2RemoteInstance
end
