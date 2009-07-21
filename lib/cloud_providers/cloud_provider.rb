=begin rdoc
  CloudProvider is the base class for cloud computing services such as Ec2, Eucalyptus - where your servers run.
=end
module CloudProviders
  class CloudProvider
    include Dslify
    include Callbacks
    
    default_options(
      :keypair_name  => nil,
      :image_id => nil
    )
    
    def initialize(opts={}, &block)
      set_vars_from_options(opts)
    end
    
    # All CloudProviders are added to the CloudProviders.all array
    def self.inherited(subclass)
      unless CloudProviders.all.include?(subclass)
        CloudProviders.all << subclass
      end
    end
    
    # Required methods
    # The next methods are required on all RemoteInstance types
    # If your RemoteInstance type does not overwrite the following methods
    # An exception will be raised and poolparty will explode into tiny little 
    # pieces. Don't forget to overwrite these methods
    
    # Launch a new instance
    def self.run_instance(o={})
      new(o).launch_new_instance!(o)
    end
    def run_instance(o={})
      raise StandardError.new("method_not_defined :launch_new_instance")
    end
    
    # Terminate an instance by id
    def self.terminate_instance!(o={})
      new(o).terminate_instance!(o)
    end
    def terminate_instance!(o={})        
      raise StandardError.new("method_not_defined :terminate_instance!")
    end
    
    # Describe an instance's status.  Should return a hash like object
    #Required keys are:
    # :image_id
    # :keypair_name
    # :instance_id
    # :status
    def self.describe_instance(o={})
      new(o).describe_instance(o) 
    end
    def describe_instance(o={})
      raise StandardError.new("method_not_defined :describe_instance")
    end
    
    # Get instances
    # The instances must return an object responding to each
    # Each yielded object must respond to [:status]
    def self.describe_instances(o={})
      new(o).describe_instances(o)
    end
    def describe_instances(o={})
      raise StandardError.new("method_not_defined :describe_instances")
    end
    
  end
end