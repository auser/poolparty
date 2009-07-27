require "#{::File.dirname(__FILE__)}/vmware"

module CloudProviders

  class VmwareInstance < CloudProviderInstance
    
    default_options(
      :launching_time   => nil,
      :vmx_file         => nil,
      :instance_id      => nil,
      :public_ip        => nil,
      :status           => "running",
      :dns_name         => nil
    )
    
    # A new instance will be created from a hash.
    # The parent clouds describe_instances list will be searched for the first one matching any of this instance's provided unique identifiers.
    # If an instance is found, this instance's properties will be set to the properties provided
    # If the found instance has properties of the same key as the provided options, the found instance's values will override the passed in options
    def initialize(opts={}, &block)
      set_vars_from_options(opts)
      super
    end
    
    # Printing. This is how we extract the instances into the listing on the 
    # local side into the local listing file
    def to_s
      "#{name}\t#{dns_name}\t#{instance_id}"
    end
    
    def status
      "running"
    end
    
    # Since we aren't actually getting any ips from a list, only using 
    # the one vmx file, we don't actually need to refresh anything
    def wait_for_public_ip(timeout=60)
      public_ip      
    end
    
    def refresh!
      self
    end
    
    def cloud_provider(o={}, &block)
      @cloud_provider ||= if cloud
        cloud.cloud_provider
      else        
        options_for_cloud_provider = o.choose{|k,v| Vmware.default_options.has_key?(k)}
        Vmware.new( options_for_cloud_provider, &block)
      end
    end
    
  end

end