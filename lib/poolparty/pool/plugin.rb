module PoolParty
      
  module Plugin
    
    class Plugin
      include MethodMissingSugar
      include Configurable
      include Resources
      extend Resources
      
      attr_accessor :parent
      
      default_options({})
      
      def initialize(parent=self, &block)
        @parent = parent
        block ? self.instance_eval(&block) : enable
      end
      
      # Overwrite this method
      def enable
      end
                  
    end
    
  end
end