module PoolParty
      
  module Plugin
    
    class Plugin
      include MethodMissingSugar
      include Configurable
      include Resources
      
      attr_accessor :parent
      
      default_options({})
      
      def initialize(parent=nil, &block)
        @parent = parent
        self.instance_eval &block if block
      end
      
      def container
        @parent.container
      end
            
    end
    
  end
end