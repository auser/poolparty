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
        realize! unless block
      end
            
      def realize!(force=false)
        force ? force_realize! : (@realized ? nil : force_realize!)
      end
      
      def force_realize!
        run_setup(parent, false, &stored_block)
        enable unless stored_block
      end
      
      # Overwrite this method
      def enable
      end
      
    end
    
  end
end