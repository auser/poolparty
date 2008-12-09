module PoolParty
  module Pool
    
    def pool(name=:app, &block)
      pools.has_key?(name) ? pools[name] : (pools[name] = Pool.new(name, &block))
    end    
    
    def pools
      $pools ||= {}
    end
    
    def with_pool(pl, opts={}, &block)
      raise CloudNotFoundException.new("Pool not found") unless pl
      pl.options.merge!(opts) if pl.options
      pl.run_in_context &block if block
    end
        
    def reset!
      $pools = $clouds = $plugins = @describe_instances = nil
    end

    class Pool
      # include PoolParty::Cloud
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
        setup_defaults
        @pool_name = name
        @pool_name.freeze
        # run_in_context &block if block
        run_setup(self, &block)        
      end
      
      def name
        @pool_name
      end
      
      def setup_defaults
        plugin_directory "#{::File.dirname(pool_specfile ? pool_specfile : Dir.pwd)}/plugins"
        PoolParty::Extra::Deployments.include_deployments "#{Dir.pwd}/deployments"
      end
            
      # This is where the entire process starts
      def inflate
      end
      
      def pool_clouds
        returning Array.new do |arr|
          clouds.each do |name, cl|
            arr << cl if cl.parent.name == self.name
          end
        end
      end
      
    end
    
    # Helpers
    def remove_pool(name)
      pools.delete(name) if pools.has_key?(name)
    end
  end
end