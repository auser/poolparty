module PoolParty    
  module PluginModel
    
    def plugin(name=:plugin, cloud=nil, &block)
      plugins.has_key?(name) ? plugins[name] : (plugins[name] = PluginModel.new(name, cloud, &block))
    end
    alias_method :register_plugin, :plugin
    
    def plugins
      @plugins ||= {}
    end
    
    class PluginModel
      attr_accessor :name, :klass
      attr_reader :parent
      
      def initialize(name,cld,&block)
        @name = name
        @parent = cld
        class_string_name = "#{name}"
        
        # Create the class to evaluate the plugin on the implemented call
        klass = class_string_name.class_constant(PoolParty::Plugin::Plugin)
        klass.send :include, PoolParty::Plugin
        
        # Create the block inside the instantiated plugin
        class_string_name.module_constant(&block)
        klass.send :include, class_string_name.module_constant
        
        # Add the plugin definition to the cloud as an instance method
        Cloud::Cloud.module_eval <<-EOE
          def #{name}(&block)
            @klass ||= #{class_string_name.class_constant}.new(self).instance_eval(&block)
          end
        EOE
      end
      
    end
    
  end
end