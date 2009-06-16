module PoolParty  
  class Cloud < Base
    
    # Options we want on the output of the compiled script
    # but want to take the options from the parent if they
    # are nil on the cloud
    required_options  :expand_when,
                      :contract_when,
                      :keypair
    
    default_options(
      :minimum_instances    => 2,     # minimum_instances default
      :maximum_instances    => 5,     # maximum_instances default
      :minimum_runtime      => 3600,  # minimum_instances default: 1 hour
      :hardware_provider    => :ec2   # hardware_provider default: ec2
    )
    
    # Callbacks
    additional_callbacks [
      "after_launch_instance",
      "after_provision"
    ]
    
    # Freeze the cloud_name so we can't modify it at all, set the plugin_directory
    # call and run instance_eval on the block and then call the after_create callback
    def initialize(n, &block)
      @cloud_name = n
      @cloud_name.freeze
      
      callback :before_create
      super
      callback :after_create
    end
    
  end
end