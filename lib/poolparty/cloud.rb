module PoolParty  
  class Cloud < Base
    default_options(
      :keypair                => nil,
      :minimum_instances      => 1,
      :maximum_instances      => 3
    )
    
    def instances(arg)
      case arg
      when Range
        minimum_instances arg.first
        maximum_instances arg.last
      when Fixnum
        minimum_instances arg
        maximum_instances arg
      when Hash
        nodes(arg)
      else
        raise PoolParty::PoolPartyError.create("DslMethodCall", "You must call instances with either a number, a range or a hash (for a list of nodes)")
      end
    end
    
    def load_balancer(name=proper_name, o={}, &block);load_balancers[name] = [name, o, block];end
    def load_balancers;@load_balancers ||= {};end
    
    def autoscale(name=proper_name, o={}, &block);autoscales[name] = [name, o, block];end
    def autoscales;@autoscales ||= {};end
    
    attr_reader :cloud_provider
    def using(provider_name, &block)
      return @cloud_provider if @cloud_provider
      @cloud_provider = "#{provider_name}".constantize(CloudProviders).send :new, provider_name, :cloud => self, &block
    end
    def run
      puts "  running on #{cloud_provider.class}"
      load_balancers.each do |lb_name, lb|
        cloud_provider.load_balancer(*lb)
      end
      autoscales.each do |as_name, as|
        cloud_provider.autoscale(*as)
      end
      cloud_provider.run
    end
    
    def proper_name
      "#{parent.name}-#{name}"
    end
  end
end
