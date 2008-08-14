class WebServers
  register_plugin :apache do
    
    attr_accessor :php, :document_root
    
    def enable_php
      @php = true
      has_line_in_file "LoadModule php4_module        libexec/httpd/libphp4.so", "/etc/httpd/httpd.conf"
      has_line_in_file "AddModule mod_php4.c", "/etc/httpd/httpd.conf"
    end
    
    def site(name=:domain1, opts={})
      @document_root = opts[:document_root]
      virtual_host name, opts
    end
    
    
        
  end
end
