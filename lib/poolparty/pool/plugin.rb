module PoolParty
      
  module Plugin
    
    class Plugin
      include Configurable
      include CloudResourcer
      include Resources
      
      class_inheritable_accessor :name
      
      default_options({})
      
      def initialize(p=self, opts={}, &block)
        store_block &block
        run_setup(p)
        # run_setup(p, &block)
        # enable unless block
      end
      
      def realize!
        run_setup(p, false, &stored_block)
        enable unless stored_block
      end
      
      # Overwrite this method
      def enable
      end
      
    end
    
  end
end