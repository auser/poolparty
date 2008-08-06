application :poolpartyrb do
  
  cloud :app do |c|
    
    c.configure {
      :maximum_instances => 1,
      :keypair => "name"
    }    
    c.minimum_instances = 1
    
    apache do
      enable_php
      virtual_host do |h|
        h.document_root = "/www/domain1"
        h.server_name = "domain1.com"
      end
      virtual_host {
        :document_root => "/www/domain2",
        :server_name => "domain2.com"
      }
    end
    
  end
  
end