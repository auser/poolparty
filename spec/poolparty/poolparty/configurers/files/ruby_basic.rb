pool :poolpartyrb do  
  cloud :app do
        
    # Configuration
    minimum_instances 1
    
    apachetest do
      enable_php
      site("poolpartyrb.com")
    end
        
  end
  
end