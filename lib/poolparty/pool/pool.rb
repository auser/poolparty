module PoolParty
  module Pool
    
    def pool(name=:main, &block)
      pools.has_key?(name) ? pools[name] : (pools[name] = Pool.new(name, &block))
    end
    
    def pools
      @@pools ||= {}
    end

    class Pool
      attr_accessor :name, :container
      include Cloud
      include MethodMissingSugar
      include PluginModel
      
      def initialize(name,&block)
        @name = name
        @container = Container.new
        self.instance_eval &block if block_given?
      end
      
      def name;@name;end
      
      def options(h={})
        @options ||= {
          :plugin_directory => "plugins"
        }.merge(h).to_os
      end
      
      alias_method :configure, :options
      
      # This is where the entire process starts
      def inflate
      end            
    end
    
  end
end