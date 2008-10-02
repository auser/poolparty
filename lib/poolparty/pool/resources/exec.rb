module PoolParty    
  module Resources
    
    class Exec < Resource
      
      default_options({
        :path => "/usr/bin:/bin:/usr/local/bin:$PATH"
      })
      
      
      def disallowed_options
        [:ensure, :name]
      end
      
      def key
        name || command
      end
                  
    end
    
  end
end