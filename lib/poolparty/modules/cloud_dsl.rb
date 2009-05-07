module PoolParty
  module CloudDsl
    
    # TODO: Add to ec2 remoter_base
    # def mount_ebs_volume_at(id="", loc="/data")
    #   ebs_volume_id id
    #   ebs_volume_mount_point loc
    #   ebs_volume_device "/dev/#{id.sanitize}"
    #         
    #   has_mount(:name => loc, :device => ebs_volume_device)
    #   has_directory(:name => loc)
    # end
    
    def set_dependency_resolver(name='chef')
      klass = name.preserved_class_constant("Resolver")
      raise DependencyResolverException.new("Unknown resolver") unless klass
      dsl_option(:dependency_resolver, klass) unless dsl_options[:dependency_resolver]
    end
    
    # Enable a service package
    def enable(*services)
      services.each do |service|
        dsl_options[service] = :enabled
      end
    end
    # Disable a service package
    def disable(*services)
      services.each do |service|
        dsl_options[service] = :disabled
      end
    end
    
    # Check to see if the package has been enabled
    def enabled?(srv);dsl_options.has_key?(srv) && dsl_options[srv] == :enabled;end
    
    # All services that are :enabled and have a plugin that corresponds, call on the cloud
    def add_optional_enabled_services
      dsl_options.each do |k,v|
        self.send k if enabled?(k) && respond_to?(k)
      end
    end
    
  end
end