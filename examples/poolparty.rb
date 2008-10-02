# Poolparty spec

pool :poolparty do
  
  instances 1..5
  keypair "auser"
  
  cloud :app do
    ami "ami-4bb05422"
  end

end