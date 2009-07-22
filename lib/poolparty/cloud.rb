module PoolParty  
  class Cloud < DslBase
    
    # Options we want on the output of the compiled script
    # but want to take the options from the parent if they
    # are nil on the cloud
    default_options(
      :minimum_instances    => 2,     # minimum_instances default
      :maximum_instances    => 5,     # maximum_instances default
      :minimum_runtime      => 3600,  # minimum_instances default: 1 hour
      :contract_when        => nil,
      :expand_when          => nil,
      :cloud_provider_name  => :ec2
    )
    
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
    def run_instance(o={}); cloud_provider.run_instance;end
    def terminate_instance!(o={}); cloud_provider.terminate_instance!(o);end
    def describe_instances(o={}); cloud_provider.describe_instances(o);end
    def describe_instance(o={}); cloud_provider.describe_instance(o);end
    
    # The actual cloud_provider instance
    def cloud_provider(opts={}, &block)
      return @cloud_provider if @cloud_provider
      if CloudProviders.all.include?("::CloudProviders::#{cloud_provider_name}".constantize)
        opts.merge!(:cloud => self, :keypair_name => self.keypair.to_s)
        @cloud_provider = "::CloudProviders::#{cloud_provider_name}".constantize.new(dsl_options.merge(opts), &block)
      else
        raise PoolParty::PoolPartyError.create("UnknownCloudProviderError", "Unknown cloud_provider: #{cloud_provider_name}")
      end
      @cloud_provider
    end
        
    # Define what gets run on the callbacks
    # This is where we can specify what gets called
    # on callbacks
    #   parameters: cld, time
    # 
    #   cld - the cloud the callback came from
    #   callback - the callback called (i.e. :after_provision)
    callback_block do |cld, callback|
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
    
    ##### Internal methods #####
    # Methods that only the cloud itself will use
    # and thus are private
    private
    
    # compile
    # Take the cloud's resources and compile them down using 
    # the defined (or the default dependency_resolver, chef)
    def compile
      dependency_resolver.compile_to(resources, tmp_path/"var"/"poolparty")
    end
    
    # Form the cloud
    # Run the init block with the init_opts
    # on the cloud
    def cloud_form
      run_with_callbacks(@init_opts, &@init_block)
    end
    
  end
end