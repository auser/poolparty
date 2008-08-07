pool :poolpartyrb do
  plugin_directory "plugins"
  
  cloud :app do
    
    # Configuration
    configure { :maximum_instances => 1,:keypair => "name" }
    minimum_instances 1
    
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