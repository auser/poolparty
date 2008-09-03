# Basic pool spec
# Shows global settings for the clouds

pool :app do
  
  maximum_instances 5
  minimum_instances 3
  
  cloud :app do
    minimum_instances 2
    ami "ami-abc123"
  end
  
  cloud :db do    
  end
end