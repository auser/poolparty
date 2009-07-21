pool :boxed do
  
  cloud :app do
    keypair 'boxed_app'
    using :ec2 do
    end
  end
  
end