class WebServers
  plugin :apache do
    include PoolParty::Resources
    
    attr_accessor :php
    
    def enable      
    end
    
    def enable_php
      @php = true
      php
    end
    
    def php
      @php
    end
    
    def site(name=:domain1, opts={})
      virtual_host name, opts
    end
    
    def virtual_host(name, opts={})
      opts = {
        :document_root => opts[:document_root] || "/www/#{name}/"
      }
      call_function "virtual_host()"
    end
        
    custom_function <<-EOE
define virtual_host($docroot, $ip, $order = 500, $ensure = "enabled") { 
    $file = "/etc/sites-available/$name.conf" 
    file { $file: 
        content => template("virtual_host.erb"), 
        notify => Service[apache] 
    } 
    file { "/etc/sites-enabled/$order-$name.conf": 
        ensure => $ensure ? { 
            enabled => $file, 
            disabled => absent 
        } 
    } 
}
    EOE
        
  end
end
