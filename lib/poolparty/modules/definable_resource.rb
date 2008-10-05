module PoolParty
  module DefinableResource
    def define_resource(name, &block)
      symc = "#{name}".classify
      klass = symc.class_constant(PoolParty::Resources::CustomResource, {:preserve => true}, &block)
      PoolParty::Resources.module_eval &block
      klass
    end
    
    # TODO: Refactor this guy
    # Allow us to create virtual resources
    # Generally, in plugins
    # This sets a virtual resource against the Resource class
    # Example:
    # 
    # virtual_resource(:virtualhost) do    
    # end
    # 
    # This defines a virtualhost as a virtual resource
    #  and consequently gives the methods has_virtualhost and does_not_have_virtualhost
    # 
    # Note that you can define any resources within the virtual resource
    # within the definition or the call.
    # Call example:
    # has_virtualhost do        
    #  name "xnot.org"
    # end
    # 
    # Which sets the virtual host's name as xnot.org
    # 
    # This is included in the poolparty-apache-plugin
    def virtual_resource(name=:virtual_resource, opts={}, &block)
      symc = "#{name}".classify
      eval <<-EOE
        class PoolParty::Resources::#{symc} < PoolParty::Resources::Resource
        end
      EOE
      klass = "PoolParty::Resources::#{symc}".constantize
      klass.module_eval &block if block
      klass.send :define_method, :virtual_resource?, Proc.new{true}
      klass
    end
    
  end  
end