pool :poolpartyrb do
  
  cloud :app do
    
    apache do
      enable_php
      
      site({
        :document_root => "/www/domain2",
        :server_name => "domain2.com"
      })
    end    
    
  end
  
end