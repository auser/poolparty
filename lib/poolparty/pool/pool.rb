module PoolParty
  module Pool
    
    def pool(name=:main, &block)
      pools.has_key?(name) ? pools[name] : (pools[name] = Pool.new(name, &block))
    end    
    
    def pools
      $pools ||= {}
    end
    
    def reset!
      $pools = nil
      $clouds = nil
    end

    class Pool
      attr_accessor :name, :container
      include PoolParty::Cloud
      include MethodMissingSugar
      include PluginModel
      include Configurable
      
      default_options({
        :plugin_directory => "plugins"
      })
      
      def initialize(name,&block)
        @name = name
        @container = Container::Container.new
        instance_eval &block if block
      end
      
      # This is where the entire process starts
      def inflate
      end            
    end
    
    # Helpers
    def remove_pool(name)
      pools.delete(name) if pools.has_key?(name)
    end
  end
end