module PoolParty  
  class Cloud < Base
    default_options(
      :keypair                => nil,
      :minimum_instances      => 1,
      :maximum_instances      => 3
    )
    
    # returns an instance of Keypair
    # You can pass either a filename which will be searched for in ~/.ec2/ and ~/.ssh/
    # Or you can pass a full filepath
    def keypair(n=nil, extra_paths=[])
      return @keypair if @keypair
      @keypair = case n
      when String
        Keypair.new(n, extra_paths)
      when nil
        fpath = CloudProviders::CloudProvider.default_keypair_path/"#{proper_name}"
        File.exists?(fpath) ? Keypair.new(fpath, extra_paths) : generate_keypair(extra_paths)
      else
        raise ArgumentError, "There was an error when defining the keypair"
      end
    end
    
    private
    def generate_keypair(extra_paths=[])
      puts "Generate the keypair for this cloud because its not found: #{proper_name}"
      cloud_provider.send :generate_keypair, proper_name
      Keypair.new(proper_name, extra_paths)
    end
    public
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
    
    def autoscaler(name=proper_name, o={}, &block);autoscalers[name] = [name, o, block];end
    def autoscalers;@autoscalers ||= {};end
    
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
      autoscalers.each do |as_name, as|
        cloud_provider.autoscale(*as)
      end
      cloud_provider.run
    end
    
    def proper_name
      "#{parent.name}-#{name}"
    end
  end
end
