class WebServers
  plugin :apache do
    
    attr_accessor :php
    
    def initialize(p)
      # Require apache package
      has_package(:name => "apache")
      super
    end
    
    def enable_php
      @php = true
      php
      # has_line_in_file "LoadModule php4_module        libexec/httpd/libphp4.so", "/etc/httpd/httpd.conf"
      # has_line_in_file "AddModule mod_php4.c", "/etc/httpd/httpd.conf"
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
      call "virtual_host()"
    end
    
    def include_modules(*args)
      
    end
    
    set do
      function %q{
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
      }
    end
        
  end
end
