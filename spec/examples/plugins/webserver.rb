class WebServers
  plugin :apache do
        
    def enable_php
      @php = true
    end
  end
end