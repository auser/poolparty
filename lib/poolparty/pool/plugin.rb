module PoolParty
      
  module Plugin
    
    class Plugin
      include Configurable
      include CloudResourcer
      include Resources
      
      class_inheritable_accessor :name
      
      default_options({})
      
      def initialize(p=self, opts={}, &block)        
        run_setup(p, &block)
        enable unless block
      end
      
      # Overwrite this method
      def enable
      end
      
    end
    
  end
end