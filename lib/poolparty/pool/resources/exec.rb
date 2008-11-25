module PoolParty    
  module Resources
    
    class Exec < Resource
      
      default_options({
        :path => "/usr/bin:/bin:/usr/local/bin:$PATH"
      })
      
      # Execs cannot have the following parameters
      # We use version in the gempackage resource
      # So we have to exclude it here.
      def disallowed_options
        [:ensure, :name]
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