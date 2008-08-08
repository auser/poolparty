pool :poolpartyrb do
  
  cloud :app do
    
    apache do
      enable_php
      
      virtual_host do
        document_root "/www/domain1"
        server_name "domain1.com"
      end
      virtual_host {
        :document_root => "/www/domain2",
        :server_name => "domain2.com"
      }
    end    
    
  end
  
end