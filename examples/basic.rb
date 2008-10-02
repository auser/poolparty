# Basic pool spec
# Shows global settings for the clouds

pool :application do
  
  instances 3..5
  keypair "auser"
  
  cloud :app do
    minimum_instances 2
    ami "ami-abc123"
  end
  
  cloud :db do
    keypair "hotstuff_database"
    maximum_instances 20
    ami "ami-1234bc"
  end

end