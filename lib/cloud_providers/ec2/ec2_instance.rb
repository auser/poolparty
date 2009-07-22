require "#{::File.dirname(__FILE__)}/ec2"

module CloudProviders

  class Ec2Instance < CloudProviderInstance
    
    default_options(
      Ec2.default_options.merge({
        :launching_time   => nil,
        :dns_name         => nil,
        :private_dns_name => nil,
        :kernel_id        => nil,
        :ramdisk_id       => nil,
        :launch_time      => nil,
        :instance_id      => nil,
        :launch_index     => nil,
        :public_ip        => nil,
        :internal_ip      => nil
        }
      )
    )
    
    @uniquely_identifiable_by = [:ip, :name, :dns_name, :instance_id]
    
    # A new instance will be created from a hash.
    # The parent clouds describe_instances list will be searched for the first one matching any of this instance's provided unique identifiers.
    # If an instance is found, this instance's properties will be set to the properties provided
    # If the found instance has properties of the same key as the provided options, the found instance's values will override the passed in options
    def initialize(opts={}, &block)
      opts.delete(:id)  # metavirt (in case your using it) passes an id that we do not want to set
      set_vars_from_options(opts)
      # @target_host = public_ip || internal_ip || ip  #set this for the netssh commands
      super
    end
    
    # Printing. This is how we extract the instances into the listing on the 
    # local side into the local listing file
    def to_s
      "#{name}\t#{ip}\t#{instance_id}"
    end
    # 
    # def self.to_s(hsh)
    #   new(hsh).to_s
    # end
    
    def hosts_file_listing_for(cl)
      string = (cl.name == cloud.name) ? "#{name}.#{my_cloud.name}\t#{name}" : "#{name}.#{my_cloud.name}"
      "#{internal_ip}\t#{string}"
    end
    
    def cloud_provider(o={}, &block)
      @cloud_provider ||= Ec2.new(dsl_options.merge(o), &block)
    end
    
    def cloud(n=nil)
      @cloud ||= n
    end
    
  end

end