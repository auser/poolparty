# Basic pool spec
# Shows global settings for the clouds
pool :application do
  instances 1..5
  
  cloud :basic_app do
    minimum_instances 3
    maximum_instances 10
    keypair 'ari'
    using :ec2 do
      image_id "ami-abc123"
    end
    
    monitor :cpu do |v|
      configure if v < 0.2
      vote_for(:expand) if v > 0.9
    end
    
    has_file :name => "/etc/motd", :content => "Welcome to your PoolParty instance"
  end
  
  cloud :basic_db do    
    minimum_instances 1
    keypair "id_rsa"
    using :ec2 do
      image_id "ami-1234bc"
    end
  end

end