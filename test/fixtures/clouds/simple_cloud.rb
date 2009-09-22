# Poolparty spec

pool :poolparty do
  
  instances 1
    
  cloud :simple_cloud do
    os :centos
    keypair "test_key", PoolParty.lib_dir+"/../test/fixtures/keys"
    has_file "/etc/motd", :content => "Simple"
  end
  
end