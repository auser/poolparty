require "#{::File.dirname(__FILE__)}/ec2"
module PoolParty  
  module Remote
    class Ec2RemoteInstance < RemoteInstance
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
        opts.delete(:id)  # metavirt passes an id that we do not want to set
        set_vars_from_options(opts) if opts.is_a?(Hash)
        @target_host = public_ip || internal_ip || ip  #set this for the netssh commands
        # super(opts)
      end
      
      # Printing. This is how we extract the instances into the listing on the 
      # local side into the local listing file
      def to_s
        "#{name}\t#{ip}\t#{instance_id rescue ""}"
      end
      
      # Class method to disect a neighborhood line
      def self.hash_from_s(s)
        arr = s.split("\t")
        {:name => arr[0], :ip => arr[1]}
      end
      
      def self.to_s(hsh)
        new(hsh).to_s
      end
      
      def hosts_file_listing_for(cl)
        string = (cl.name == cloud.name) ? "#{name}.#{my_cloud.name}\t#{name}" : "#{name}.#{my_cloud.name}"
        "#{internal_ip}\t#{string}"
      end
      
    end
  end
end