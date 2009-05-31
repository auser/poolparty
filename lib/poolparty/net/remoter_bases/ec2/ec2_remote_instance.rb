require "#{::File.dirname(__FILE__)}/ec2"
module PoolParty  
  module Remote
    class Ec2RemoteInstance # < RemoteInstance
      include Dslify
      include Remote
      
      default_options({
        # :launching_time   => Time.now,
        :dns_name         => nil,
        :private_dns_name => nil,
        :key_name         => nil,
        :kernel_id        => nil,
        :ramdisk_id       => nil,
        :launch_time      => nil,
        :instance_id      => nil,
        :ami_launch_index => nil,
        :ip               => nil,
        :public_ip        => nil,
        :internal_ip      => nil
        }.merge(Remote::Ec2.default_options) )
      
      @uniquely_identifiable_by = [:ip, :name, :dns_name, :instance_id]
      
      # A new instance will be created from the passed in hash.  
      # This hash of passed in values will be converted to methods on this instance.
      # The parent clouds describe_instances list will be searched for the first one matching any of this instance's provided unique identifiers.
      # If an instance is found, this instance's properties will be set to the properties provided
      # If the found instance has properties of the same key as the provided options, the found instance's values will override the passed in options
      def initialize(opts={})
        set_vars_from_options(opts) if opts.is_a?(Hash)
        @target_host = public_ip || internal_ip || ip  #set this for the netssh commands
        # super(opts)
      end
      
      def keypair
        @keypair ||= Key.new(key_name)
      end
      
      ## hash like methods
      # TODO: move these into a module, or into dslify
      # include Enumerable
      def each
        dsl_options.each{ |k,v| yield k,v }
      end
      def [](k)
        dsl_options[k]
      end
      
      def []=(k,v)
        dsl_options[k] = v
      end
      
      def keys
        dsl_options.keys
      end
         
      def values
        dsl_options.values
      end
      ##end of hash like methods
      
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