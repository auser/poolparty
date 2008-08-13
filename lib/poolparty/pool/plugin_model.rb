module PoolParty    
  module PluginModel
    
    def plugin(name=:plugin, &block)
      plugins.has_key?(name) ? plugins[name] : (plugins[name] = PluginModel.new(name, &block))
    end
    alias_method :register_plugin, :plugin
    def plugins
      @@plugins ||= {}
    end
    
    class PluginModel
      attr_accessor :name, :klass
      
      def initialize(name,&block)
        @name = name
        # Create the block inside the instantiated plugin
        "#{name}".module_constant &block
        
        # Create the class to evaluate the plugin on the implmented call
        name.to_s.class_constant &block
        
        # Create the cloud method
        # meth = "def #{name}(&block); #{@klass.instance_eval &block} end"
        # # Add the plugin definition to the cloud as an instance method
        # Cloud.module_eval { meth }
      end
    end
    
  end
end