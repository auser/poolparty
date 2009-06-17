# Poolparty spec

pool :poolparty do
  
  instances 1
    
  cloud :app do
    keypair "cloudteam_test"
    
    has_file "/etc/motd", :content => "Simple"
  end
  
end