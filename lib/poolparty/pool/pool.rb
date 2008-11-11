module PoolParty
  module Pool
    
    def pool(name=:app, &block)
      pools.has_key?(name) ? pools[name] : (pools[name] = Pool.new(name, &block))
    end    
    
    def pools
      $pools ||= {}
    end
    
    def with_pool(pool, opts={}, &block)
      pool.options.merge!(opts)
      pool.instance_eval &block if block
    end
        
    def reset!
      $pools = $clouds = $plugins = @describe_instances = nil
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
      
      default_options({
        :access_key => Base.access_key,
        :secret_access_key => Base.secret_access_key
      })
      
      def initialize(name,&block)
        @name = name
        instance_eval &block if block
      end
      
      def plugin_directory(*args)
        args = ["/plugins"] if args.empty?
        args.each {|arg| Dir["#{arg}/*/*.rb"].each {|f| require f rescue "" }}
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