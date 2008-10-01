module PoolParty    
  module Resources
    
    class Exec < Resource
      
      default_options({
        :path => "/usr/bin:/bin:/usr/local/bin"
      })
      
      
      def disallowed_options
        [:ensure]
      end
      
      def key
        command || name
      end
            
    end
    
  end
end