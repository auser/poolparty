pool :poolpartyrb do
  plugin_directory "plugins"
  
  cloud :app do
    
    # Configuration
    configure({ :maximum_instances => 1,:keypair => "name" })
    minimum_instances 1    
    
  end
  
end