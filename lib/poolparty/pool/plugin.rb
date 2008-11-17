module PoolParty
      
  module Plugin
    
    class Plugin
      include Configurable
      include CloudResourcer
      include Resources
      
      attr_reader :parent
      class_inheritable_accessor :name
      
      default_options({})
      
      def initialize(parent=self, opts={}, &block)
        set_parent(parent)
        block ? run_in_context(&block) : enable
      end
      
      # Overwrite this method
      def enable
      end
      
      # Call the cloud from within the plugin
      def cloud
        @p = parent
        while !@p.is_a?(PoolParty::Cloud)
          @p = @p.parent
        end
        @p
      end
      
    end
    
  end
end