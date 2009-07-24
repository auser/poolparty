# Poolparty spec

pool :poolparty do
  
  instances 1
    
  cloud :app do
    keypair File.dirname(__FILE__)+"/../keys/test_key"
    has_file "/etc/motd", :content => "Simple"
  end
  
end