# Basic pool spec
# Shows global settings for the clouds
pool :application do
  instances 1..5
  
  cloud :frontend do
    minimum_instances 3
    keypair 'application_frontend'
    image_id "ami-abc123"
    has_file :name => "/etc/motd", :content => "Welcome to your PoolParty instance"
  end
  
  cloud :database do
    keypair 'application_database'
    using :vmrun do
      vmx_hash "/path/to/vmx_file" => "192.168.248.122"
    end
    minimum_instances 1
    image_id "ami-1234bc"
  end

end