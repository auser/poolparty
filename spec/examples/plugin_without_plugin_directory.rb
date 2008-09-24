require File.dirname(__FILE__) + "/pool/test_plugins/webserver"

pool :app do
  
  instances 2..10
  
  cloud :app do
    apache do
      enable_php
    end    
  end
  
end