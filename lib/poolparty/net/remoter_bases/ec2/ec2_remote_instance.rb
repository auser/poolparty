module PoolParty  
  module Remote
    class Ec2RemoteInstance < RemoteInstance
      include Dslify
      
      attr_reader :uniquely_identifiable_by, :found_at
      
      default_options( {:launching_time => Time.now}.merge(Remote::Ec2.default_options) )
      
      # A new instance will be created from the passed in hash.  
      # This hash of passed in values will be converted to methods on this instance.
      # The parent clouds describe_instances list will be searched for the first one matching any of this instance's provided unique identifiers.
      # If an instance is found, this instance's properties will be set to the properties provided
      # If the found instance has properties of the same key as the provided options, the found instance's values will override the passed in options
      def initialize(opts={})
        @uniquely_identifiable_by = [:ip, :name, :dns_name, :instance_id]
        @original_options = opts
        super(opts)
      end
         
      # Is this instance running?
      def running?
        !(status =~ /running/).nil?
      end
      # Is this instance pending?
      def pending?
        !(status =~ /pending/).nil?
      end
      # Is this instance terminating?
      def terminating?
        !(status =~ /shutting/).nil?
      end
      # Has this instance been terminated?
      def terminated?
        !(status =~ /terminated/).nil?
      end
      
    end
  end
end