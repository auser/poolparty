class WebServers
  plugin :apachetest do    
    def loaded(o={}, &block)
      @loaded ||= true
    end
    
    def enable
    end
    
    def enable_php
      @php = true
    end
    
    def php
      @php ||= false
    end
    
    def site(name=:domain1, opts={})
      virtual_host name, opts
    end
    
    def virtual_host(name, opts={})
      opts = {
        :document_root => opts[:document_root] || "/www/#{name}/"
      }
    end        
  end
end
