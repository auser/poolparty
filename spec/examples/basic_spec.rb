# Basic pool spec

pool :app do
  
  maximum_instances 3
  minimum_instances 2
  
  cloud :app do
    
  end
  
end