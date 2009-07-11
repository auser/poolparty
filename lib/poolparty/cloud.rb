module PoolParty  
  class Cloud < DslBase
    
    # Options we want on the output of the compiled script
    # but want to take the options from the parent if they
    # are nil on the cloud
    additional_options  :expand_when,
                        :contract_when,
                        :keypair
    
    default_options(
      :minimum_instances    => 2,     # minimum_instances default
      :maximum_instances    => 5,     # maximum_instances default
      :minimum_runtime      => 3600,  # minimum_instances default: 1 hour
      :cloud_provider       => :ec2   # hardware_provider default: ec2
    )
        
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
    
    # compile
    # Take the cloud's resources and compile them down using 
    # the defined (or the default dependency_resolver, chef)
    def compile
      dependency_resolver.compile(resources)
    end
    
    ##### Internal methods #####
    # Methods that only the cloud itself will use
    # and thus are private
    private
    
  end
end