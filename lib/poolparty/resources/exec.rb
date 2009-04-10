module PoolParty    
  module Resources
    
    class Exec < Resource
      
      default_options({
        :path => ["/usr/bin:/bin:/usr/local/bin:$PATH"]
      })
      
      def present
        nil
      end
      
      def absent
        nil
      end
      
      def after_create
        options[:name] = options[:command] unless options.name
      end

    end
    
  end
end