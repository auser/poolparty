module PoolParty
  module Pool
    
    def pool(name=:main, &block)
      pools.has_key?(name) ? pools[name] : (pools[name] = Pool.new(name, &block))
    end    
    
    def pools
      $pools ||= {}
    end
    
    def reset!
      $pools = $clouds = $plugins = nil
    end

    class Pool
      attr_accessor :name
      include PoolParty::Cloud
      include MethodMissingSugar
      # include PluginModel
      include Configurable
      include PrettyPrinter
      include CloudResourcer
      include Remote
      
      def initialize(name,&block)
        @name = name
        instance_eval &block if block
      end
      
      def plugin_directory(*args)
        args.each {|arg| Dir["#{arg}/*.rb"].each {|f| load f }}
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