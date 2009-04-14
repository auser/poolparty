module PoolParty
  module CloudDsl
        
    def mount_ebs_volume_at(id="", loc="/data")
      ebs_volume_id id
      ebs_volume_mount_point loc
      ebs_volume_device "/dev/#{id.sanitize}"
            
      has_mount(:name => loc, :device => ebs_volume_device)
      has_directory(:name => loc)
    end
    
    def dependency_resolver(name='puppet')
      klass = name.preserved_class_constant("Resolver")
      raise DependencyResolverException.new("Unknown resolver") unless klass
      dsl_options[:dependency_resolver] = klass unless dsl_options[:dependency_resolver]
    end
    
    def enable(service)
      dsl_options[service] = :enabled
    end
    
  end
end