# Basic pool spec
# Shows global settings for the clouds
pool :application do
  instances 3..50
    
  cloud :basic_app do
    
    minimum_instances 12
    ami "ami-abc123"
    junk_yard_dogs "pains"
    has_file :name => "/etc/init.d/motd", :content => "Welcome to your PoolParty instance"
  end
  
  cloud :basic_db do    
    ami "ami-1234bc"
    junk_yard_dogs "are bad"
  end

end