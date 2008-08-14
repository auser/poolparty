module PoolParty    
  module PluginModel
    
    def plugin(name=:plugin, &block)
      plugins.has_key?(name) ? plugins[name] : (plugins[name] = PluginModel.new(name, &block))
    end
    alias_method :register_plugin, :plugin
    
    def plugins
      @plugins ||= {}
    end
    
    class PluginModel
      attr_accessor :name, :klass
      
      def initialize(name,&block)
        @name = name
        class_string_name = "#{name}"
        
        # Create the class to evaluate the plugin on the implemented call
        klass = class_string_name.class_constant(PoolParty::Plugin::Plugin)
        klass.send :include, PoolParty::Plugin
        
        # Create the block inside the instantiated plugin
        class_string_name.module_constant(&block)
        klass.send :include, class_string_name.module_constant
                
        # Add the plugin definition to the cloud as an instance method
        Cloud.instance_eval do
          define_method name do            
            (@klass ||= klass.new).instance_eval &block
          end
        end

      end
    end
    
  end
end