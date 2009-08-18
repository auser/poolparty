# Poolparty spec

pool "poolparty" do
  
  instances 1
  
  cloud "monitored_app" do
    keypair "eucalyptus_sample"
    using :ec2 do
      image_id 'emi-39CA160F'
    end
    
    monitor :cpu do |c|
      vote_for(:expand) if c > 0.9
      vote_for(:contract) if c < 0.1
    end
        
    has_file "/etc/motd", :content => "Simple"
    
  end
  
end