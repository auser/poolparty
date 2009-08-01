# Basic pool spec
# Shows global settings for the clouds
pool :application do
  instances 1..5
  
  cloud :basic_app do
    minimum_instances 3
    maximum_instance 10
    keypair 'application_front'
    image_id "ami-abc123"
    has_file :name => "/etc/motd", :content => "Welcome to your PoolParty instance"
  end
  
  cloud :basic_db do    
    minimum_instances 1
    image_id "ami-1234bc"
  end

end