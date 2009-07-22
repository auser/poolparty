=begin rdoc
  CloudProvider is the base class for cloud computing services such as Ec2, Eucalyptus - where your servers run.
=end
module CloudProviders
  class CloudProvider
    include Dslify
    include Callbacks
    
    default_options(
      :cloud        => nil,
      :keypair_name => nil,
      :image_id     => nil
    )
    
    # All CloudProviders are added to the CloudProviders.all array
    def self.inherited(subclass)
      unless CloudProviders.all.include?(subclass)
        CloudProviders.all << subclass
      end
    end
    
    def initialize(opts={}, &block)
      set_vars_from_options(opts)
      instance_eval(&block) if block
    end
    
    # Required methods
    # The next methods are required on all RemoteInstance types
    # If your RemoteInstance type does not overwrite the following methods
    # An exception will be raised and poolparty will explode into tiny little 
    # pieces. Don't forget to overwrite these methods
    
    # Launch a new instance
    def run_instance(o={})
      raise StandardError.new("method_not_defined :launch_new_instance")
    end
    
    # Terminate an instance by id
    # Required params:
    #   instance_id or instance_ids
    def terminate_instance!(o={})
      raise StandardError.new("method_not_defined :terminate_instance!")
    end
    
    # Describe an instance's status.  Should return a hash like object
    # Required keys are:
    # :image_id
    # :keypair_name
    # :instance_id
    # :status
    def describe_instance(o={})
      raise StandardError.new("method_not_defined :describe_instance")
    end
    
    # Get instances
    # The instances must return an object responding to each
    # Each yielded object must respond to [:status]
    def describe_instances(o={})
      raise StandardError.new("method_not_defined :describe_instances")
    end
    
    # DSL and helpers
    
    # Returns an instance of Keypair
    # You can pass either a filename which will be searched for in ~/.ec2/ and ~/.ssh/
    # or you can pass a full filepath
    def keypair(n=keypair_name)
      @keypair ||= PoolParty::Keypair.new(n)
      keypair_name = @keypair.basename
      @keypair
    end
    def keypair=(n)
      @keypair = n
    end
    
    # Nodes
    # returns nodes from the describe_instances array
    # These can be selected on by passing a hash
    def nodes(hsh={})
      unordered = begin
        results = describe_instances.select_with_hash(hsh)
        results.select_with_hash(hsh)
      end
      
      # provide consistent sorting for nodes 
      # TODO: Add back when cloud_instance
      # unordered.sort_by(&:launch_time)
    end
    
    private
    
    # Selection hash
    # Used to select uniqueness
    def selection_hash(o={})
      kname ||= o.delete(:keypair_name) || (clouds[o[:cloud_name]].keypair.basename if o.delete(:cloud_name))
      {:ssh_key_name => kname}
    end
    
  end
end