pool :poolpartyrb do
  plugin_directory "docs_plugins"  
  
  cloud :app do
        
    # Configuration
    minimum_instances 1
    
    apachetest do
      enable_php
      site("poolpartyrb.com")
    end
        
  end
  
end