module PoolParty
      
  module Plugin
    
    class Plugin
      include MethodMissingSugar
      include Configurable
      include Resources
      extend Resources
      
      attr_accessor :parent
      
      default_options({})
      
      def initialize(parent=nil, &block)
        @parent = parent
        self.instance_eval &block if block
        loaded
      end
      
      # Overwrite this method. It is called on load of the plugin
      def loaded
      end
                  
    end
    
  end
end