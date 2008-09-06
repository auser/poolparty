class WebServers
  plugin :apache do
    
    attr_reader :php
        
    def enable_php
      @php = true
    end
    
  end
end