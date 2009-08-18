pool :boxed do
  
  cloud :app do
    keypair 'test_key'
    using :ec2 do
    end
  end
  
end