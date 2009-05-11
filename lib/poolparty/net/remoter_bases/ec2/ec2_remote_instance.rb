module PoolParty  
  module Remote
    class Ec2RemoteInstance < RemoteInstance
      include Dslify
      
      attr_reader :my_cloud, :uniquely_identifiable_by, :found_at

      
      # A new instance will be created from the passed in hash.  
      # This hash of passed in values will be converted to methods on this instance.
      # The parent clouds describe_instances list will be searched for the first one matching any of this instance's provided unique identifiers.
      # If an instance is found, this instance's properties will be set to the properties provided
      # If the found instance has properties of the same key as the provided options, the found instance's values will override the passed in options
      def initialize(opts={}, prnt=nil)
        @uniquely_identifiable_by = [:ip, :name, :dns_name, :instance_id]
        @original_options = opts
        @my_cloud = prnt
        super(opts, prnt)
        find_myself(@uniquely_identifiable_by && opts.keys) if prnt.respond_to?(:describe_instances)
      end
   
      # Search the clouds describe_instances list for the first match on one of this nodes unique identifiers
      def find_myself(unique_identifiers = @original_options.keys)
        description_hash = @my_cloud.describe_instances.detect do|node|
          unique_identifiers.detect{|identifier_key| node[identifier_key] == options[identifier_key]
            }
        end
        return nil if description_hash.blank?
        @found_at = Time.now
        self.set_vars_from_options(description_hash)
        self
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