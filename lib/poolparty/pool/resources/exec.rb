module PoolParty    
  module Resources
    
    class Exec < Resource
      
      default_options({
        :path => "/usr/bin:/bin:/usr/local/bin:$PATH"
      })
      
      # Execs cannot have the following parameters
      # We use version in the gempackage resource
      # So we have to exclude it here. Alternatively, we could
      # exclude it in the gempackage, but this is an example
      # of how to exclude it as well
      def disallowed_options
        [:ensure, :name, :source, :version, :download_url, :template]
      end
      
      def key
        name || command
      end
      
      def present
        "running"
      end
                  
    end
    
  end
end