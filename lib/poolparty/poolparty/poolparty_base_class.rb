require "dslify"
require "parenting"
module PoolParty
  
  def context_stack
    $context_stack ||= []
  end
  
  class PoolPartyBaseClass
    include Dslify, Parenting
    
    include PoolParty::DependencyResolverCloudExtensions
    # attr_accessor :depth

    def initialize(opts={}, extra_opts={}, &block)
      add_to_parent_if_parent_exists_and_is_a_service
      
      @init_block = block
      @base_name = get_name_from_options_and_extra_options(opts, extra_opts)
      
      o = (opts.is_a?(Hash) ? extra_opts.merge(opts) : extra_opts).merge(:name => @base_name)
      
      run_in_context(o, &block)
      super(&block)
    end
    
    # Overloading the parent run_in_context
    def run_in_context(o={}, &block)
      if o
        context_stack.push self
        set_vars_from_options(o)
        instance_eval &block if block
        context_stack.pop
      else
        super
      end
    end
    
    # Add the parent's options to my own and add myself as a service if I am not a resource
    def add_to_parent_if_parent_exists_and_is_a_service      
      if parent && !parent.is_a?(PoolParty::Resources::Resource)
        dsl_options.merge!(parent.dsl_options) if parent.is_a?(PoolParty::Pool::Pool)
        parent.add_service(self) if parent.respond_to?(:add_service) && !is_a_resource?
      end
    end
    
    # Try to extract the name from the options
    def get_name_from_options_and_extra_options(opts={}, extra_opts={})
      opts.is_a?(Hash) ? (opts.has_key?(:name) ? opts.delete(:name) : nil) : dsl_options[:name] = opts
    end
    
    # Add to the services pool for the manifest listing
    def add_service(serv, extra_name="")
      subclass = "#{serv.class.to_s.top_level_class.underscore.downcase}#{extra_name}"
      lowercase_class_name = subclass.to_s.underscore.downcase || subclass.downcase
      
      (services[lowercase_class_name.to_sym] ||= []) << serv if serv && !serv.empty?
      # services.merge!({lowercase_class_name.to_sym => serv})
    end
    # Container for the services
    def services
      @services ||= OrderedHash.new
    end
    
    def resources
      @resources ||= OrderedHash.new
    end
    
    # Add resource
    # When we are looking to add a resource, we want to make sure the
    # resources isn't already added. This way we prevent duplicates 
    # as puppet can be finicky about duplicate resource definitions. 
    # We'll look for the resource in either a local or global store
    # If the resource appears in either, return that resource, we'll just append
    # to the resource config, otherwise instantiate a new resource of the type
    # and store it into the global and local resource stores
    # 
    # A word about stores, the global store stores the entire list of stored
    # resources. The local resource store is available on all clouds and plugins
    # which stores the instance variable's local resources. 
    def add_resource(ty, opts={}, extra_opts={}, &block)    
      temp_name = get_name_from_options_and_extra_options(opts, extra_opts)
      
      if res = get_resource(ty, temp_name, opts)        
        res
      else
        opts = (opts.is_a?(Hash) ? extra_opts.merge(opts) : extra_opts).merge(:name => temp_name)
        
        # opts.merge!(:name => temp_name) unless opts.has_key?(:name)
        res = if PoolParty::Resources::Resource.available_resources.include?(ty.to_s.camelize)
          "PoolParty::Resources::#{ty.to_s.camelize}".camelize.constantize.new(opts, &block)
        else
          "#{ty.to_s.camelize}".camelize.constantize.new(opts.merge(:name), &block)
        end
        res.after_create
        store_in_local_resources(ty, res)
        ordered_resources << res
        res
      end
    end
    def store_in_local_resources(ty, obj)
      resource(ty) << obj
    end
    def in_local_resources?(ty, k)
      !resource(ty).select {|r| r.name == k }.empty? rescue false
    end
    def get_local_resource(ty, k)
      resource(ty).select {|r| r.name == k }.first
    end
    
    def get_resource(ty, n, opts={}, &block)
      if in_local_resources?(ty, n)
        get_local_resource(ty, n)
      elsif parent && parent != self
        parent.get_resource(ty, n)
      else
        nil
      end
    end
    
    # Store the call and use of plugins into an array
    def plugin_store
      @plugin_store ||= []
    end
    
    def resource(type=:file)
      resources[type.to_sym] ||= []
    end
    
    def ordered_resources
      @ordered_resources ||= []
    end
    
    def is_plugin?
      false
    end
    
    def is_a_resource?
      false
    end
    
    def method_missing(m,*a,&block)
      if this_context && this_context != self && this_context.respond_to?(m)# && !self.is_a?(PoolParty::Resources::Resource)
        this_context.send m, *a, &block      
      else
        # if dsl_options.has_key?(m)
        #          dsl_options[m]
        #        elsif parent && parent.respond_to?(:dsl_options) && parent.dsl_options.has_key?(m)
        #          parent.dsl_options[m]
        #        elsif self.class.default_options.has_key?(m)
        #          self.class.default_options[m]
        #        elsif parent && parent.respond_to?(:dsl_options) && parent.default_options.has_key?(m)
        #          parent.default_options[m]
        #        else
          super
        # end
      end
    end
    
    # Adds two methods to the module
    # Adds the method type:
    #   has_
    # and 
    #   does_not_have_
    # for the type passed
    # for instance
    # add_has_and_does_not_have_methods_for(:file)
    # gives you the methods has_file and does_not_have_file
    # TODO: Refactor nicely to include other types that don't accept ensure
    def self.add_has_and_does_not_have_methods_for(typ=:file)
      method_name = "__#{typ}"
      PoolParty::PoolPartyBaseClass.module_eval <<-EOE
        def has_#{typ}(opts={}, extra={}, &block)
          #{method_name}({:ensures => :present}.merge(handle_option_values(opts).merge(extra)), &block)
        end
        def does_not_have_#{typ}(opts={}, extra={}, &block)
          #{method_name}({:ensures => :absent}.merge(handle_option_values(opts).merge(extra)), &block)
        end
      EOE
    end
    
    def handle_option_values(o)
      case o.class.to_s
      when "String"
        {:name => o}
      else
        o
      end
    end
    
  end
end
