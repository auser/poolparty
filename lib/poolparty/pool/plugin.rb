module PoolParty    
  module Plugin
    
    def plugin(name=:plugin, &block)
      plugins.has_key?(name) ? plugins[name] : (plugins[name] = Plugin.new(name, &block))
    end
    alias_method :register_plugin, :plugin
    def plugins
      @@plugins ||= {}
    end
    
    class Plugin
      attr_accessor :name
      
      def initialize(name,&block)
        @name = name
        self.instance_eval &block
        
        # Add the plugin definition to the cloud as an instance method
        Cloud.extend(self)
        #   define_method name do
        #     instance_eval &block
        #   end
        # end
        
      end
    end
    
  end
end