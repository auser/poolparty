module PoolParty    
  module Resources
    
    class Exec < Resource
      
      default_options({
        :path => "/usr/bin:/bin:/usr/local/bin:$PATH"
      })
      
      # Execs cannot have the following parameters
      # We use version in the gempackage resource
      # So we have to exclude it here.
      def allowed_options
        [
          :command, :creates, :cwd, :env, :environment, :group, :logoutput, :user,
          :onlyif, :path, :refresh, :refreshonly, :returns, :timeout, :unless
        ]
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