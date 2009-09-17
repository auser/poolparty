# Poolparty spec

pool :poolparty do
  
  instances 1
    
  cloud :simple_cloud do
    os :centos
    keypair File.dirname(__FILE__)+"/../keys/test_key"
    has_file "/etc/motd", :content => "Simple"
  end
  
end