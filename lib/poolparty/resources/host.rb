module PoolParty    
  module Resources
        
    class Host < Resource
            
      default_options({
      })
      
      def aka(i=nil)
        i ? options[:alias] = i : options[:alias]
      end
            
    end
    
  end
end