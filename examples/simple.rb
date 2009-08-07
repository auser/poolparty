# Poolparty spec

pool "poolparty" do
  
  instances 1
    
  cloud "app" do
    keypair "eucalyptus_sample"
    using :ec2 do
      image_id 'emi-39CA160F'
    end
        
    has_file "/etc/motd", :content => "Simple"
    
  end
  
end