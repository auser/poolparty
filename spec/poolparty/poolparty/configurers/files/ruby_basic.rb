pool :poolpartyrb do
  plugin_directory "docs_plugins"
  
  cloud :app do
        
    # Configuration
    configure({ :maximum_instances => 1,:keypair => "name" })
    minimum_instances 1    
    
    apache do
      enable_php
      site("poolpartyrb.com")
    end
        
  end
  
end