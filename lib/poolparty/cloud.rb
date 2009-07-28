module PoolParty  
  class Cloud < DslBase
    
    # Options we want on the output of the compiled script
    # but want to take the options from the parent if they
    # are nil on the cloud
    default_options(
      :minimum_instances        => 2,     # minimum_instances default
      :maximum_instances        => 5,     # maximum_instances default
      :minimum_runtime          => 3600,  # minimum_instances default: 1 hour
      :contract_when            => nil,
      :expand_when              => nil,
      :cloud_provider_name      => :ec2,
      :dependency_resolver_name => nil
    )
    
    # Define what gets run on the callbacks
    # This is where we can specify what gets called
    # on callbacks
    #   parameters: cld, time
    #   cld - the cloud the callback came from
    #   callback - the callback called (i.e. :after_provision)
    callback_block do |cld, callback|
    end
    
    # returns an instance of Keypair
    # You can pass either a filename which will be searched for in ~/.ec2/ and ~/.ssh/
    # Or you can pass a full filepath
    def keypair(n=nil)
      @keypair ||= Keypair.new(n)
    end
    
    # Declare the CloudProvider for a cloud
    #  Create an instance of the cloud provider this cloud is using
    def using(provider_symbol, o={}, &block)
      return @cloud_provider if @cloud_provider
      self.cloud_provider_name = provider_symbol
      cloud_provider(o, &block)
    end
    
    # Cloud provider methods
    def nodes(o={}); cloud_provider.nodes(o); end
    def run_instance(o={}); cloud_provider.run_instance(o);end
    def terminate_instance!(o={}); cloud_provider.terminate_instance!(o);end
    def describe_instances(o={}); cloud_provider.describe_instances(o);end
    def describe_instance(o={}); cloud_provider.describe_instance(o);end
    
    # Terminate all instances in the cloud
    def terminate!
      nodes.collect{|n| n.terminate! }
    end
    
    # The actual cloud_provider instance
    def cloud_provider(opts={}, &block)
      return @cloud_provider if @cloud_provider
      klass_name = "CloudProviders::#{cloud_provider_name}".classify
      if provider_klass = CloudProviders.all.detect {|k| k.to_s == klass_name }
        opts.merge!(:cloud => self, :keypair_name => self.keypair.basename)
        @cloud_provider = provider_klass.new(dsl_options.merge(opts), &block)
      else
        raise PoolParty::PoolPartyError.create("UnknownCloudProviderError", "Unknown cloud_provider: #{cloud_provider_name}")
      end
      @cloud_provider
    end
    
    # 1.) Launches a new instance,
    # 2.) Waits for the instance to get an ip address
    # 3.) Waits for port 22 to be open
    # 4.) Calls call_after_launch_instance callbacks
    # 5.) Executes passed &block, if any
    # 6.) Returns the new instance object
    def expand(opts={}, &block)
      timeout = opts.delete(:timeout) || 300
      instance = cloud_provider.run_instance(options)
      callback :before_launch_instance
      #wait for an ip and then wait for ssh port, then configure instance
      if instance.wait_for_public_ip(timeout) && instance.wait_for_port(22, :timeout=>timeout)
        callback :after_launch_instance
        block.call(instance) if block
        instance
      else
        "Instance port not available"
      end
      instance.refresh!
      instance
    end
    
    # Contract the cloud
    def contract(hsh={})
      nodes(hsh).last.terminate!
    end
    
    # Freeze the cloud_name so we can't modify it at all, set the plugin_directory
    # call and run instance_eval on the block and then call the after_create callback
    def initialize(n, o={}, &block)
      @cloud_name = n
      @cloud_name.freeze
      
      super(n,o,&block)
    end
    
    # Temporary path
    # Starts at the global default tmp path and appends the pool name
    # and the cloud name
    def tmp_path
      Default.tmp_path / pool.name / name
    end
    
    # The pool this cloud belongs to
    def pool
      parent
    end
    
    # compile
    
    # Resolve with the dependency resolver
    def resolve_with(a)
      if DependencyResolvers.const_defined?(a.classify)        
        dependency_resolver DependencyResolvers.module_eval("#{a.classify}")
      else
        raise PoolParty::PoolPartyError.create("DependencyResolverError", "Undefined dependency resolver: #{a}. Please specify one of the following: #{DependencyResolvers.all.join(", ")}")
      end
    end
    
    # Set the dependency resolver
    def dependency_resolver(sym=nil)
      @dependency_resolver ||= case sym
      when :chef, nil
        dsl_options[:dependency_resolver_name] = :chef
        DependencyResolvers::Chef
      end
    end
    
    # Take the cloud's resources and compile them down using 
    # the defined (or the default dependency_resolver, chef)
    def compile
      dependency_resolver.compile_to(self, tmp_path/"etc"/"#{dependency_resolver_name}")
    end
    
    
    ##### Internal methods #####
    # Methods that only the cloud itself will use
    # and thus are private
    
    # Form the cloud
    # Run the init block with the init_opts
    # on the cloud
    def cloud_form
      run_with_callbacks(@init_opts, &@init_block)
    end
    
  end
end