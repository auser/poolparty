class WebServers
  plugin :apache do
    
    attr_accessor :php
    
    def enable_php
      @php = true
      has_line_in_file "LoadModule php4_module        libexec/httpd/libphp4.so", "/etc/httpd/httpd.conf"
      has_line_in_file "AddModule mod_php4.c", "/etc/httpd/httpd.conf"
    end
    
    def site(name=:domain1, opts={})
      virtual_host name, opts
    end
    
    def virtual_host(name, opts={})
      
    end
    
    set do
      custom_function :virtual_host, <<-EOM
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
      EOM
    end
        
  end
end
