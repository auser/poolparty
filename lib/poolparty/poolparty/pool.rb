module PoolParty
  module Pool
    
    def pool(name, &block)
      pools[name] ||= Pool.new(name, &block)
    end
    
    def pools
      $pools ||= {}
    end
    
    def with_pool(pl, opts={}, &block)
      raise CloudNotFoundException.new("Pool not found") unless pl
      pl.options.merge!(opts) if pl.options
      pl.run_in_context &block if block
    end
    
    def set_pool_specfile(filename)
      $pool_specfile = filename unless $pool_specfile
    end
    
    def pool_specfile
      $pool_specfile
    end
    
    def reset!
      $pools = $clouds = $plugins = @describe_instances = nil
    end

    class Pool < PoolParty::PoolPartyBaseClass
      include PrettyPrinter
      include CloudResourcer # WHY?!?! TODO: check on this
      include Remote
      
      def initialize(name,&block)
        @pool_name = name
        @pool_name.freeze
        
        ::PoolParty.context_stack.clear
        
        set_pool_specfile get_latest_caller
        setup_defaults

        super(&block)
      end
      def self.load_from_file(filename=nil)
        # a = new ::File.basename(filename, ::File.extname(filename))
        File.open(filename, 'r') do |f|
          instance_eval f.read, pool_specfile
        end
        # a
      end
      def name(*args)
        @pool_name ||= @pool_name ? @pool_name : (args.empty? ? :default_pool : args.first)
      end

      def parent;nil;end
      
      def setup_defaults        
        PoolParty::Extra::Deployments.include_deployments "#{Dir.pwd}/deployments"
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