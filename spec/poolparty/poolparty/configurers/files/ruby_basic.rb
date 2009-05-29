pool :poolpartyrb do  
  cloud :app do
        
    # Configuration
    minimum_instances 1
    
    apache_test do
      enable_php
      site("poolpartyrb.com")
    end
        
  end
  
end