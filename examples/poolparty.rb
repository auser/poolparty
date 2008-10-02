# Poolparty spec

pool :poolparty do
  
  instances 2..5
    
  cloud :app do
    keypair "auser"
    ami "ami-4bb05422"
  end
  
  cloud :db do
    
  end
  
end