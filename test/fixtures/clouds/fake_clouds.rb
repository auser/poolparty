pool :boxed do
  
  cloud :fake_cloud do
    keypair File.dirname(__FILE__)+"/../keys/test_key"
    using :ec2 do
    end
  end
  
end