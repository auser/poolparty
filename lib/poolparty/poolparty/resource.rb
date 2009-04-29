=begin rdoc
  The Container

  Container holds the various features for the final compilations
  for each pool.
  
  Most of the Resources will not need to create their own
=end
module PoolParty
  module Resources
    
    def custom_file(path, str)
      write_to_file_in_storage_directory(path, str)
    end
    
    class Resource < PoolParty::PoolPartyBaseClass
      attr_accessor :prestring, :poststring
      
      # include CloudResourcer
      
      # For the time being, we'll make puppet the only available dependency resolution
      # base, but in the future, we can rip this out and make it an option
      # include PoolParty::DependencyResolutions::Puppet
      include PoolParty::DependencyResolverResourceExtensions
      
      # DSL Overriders
      include PoolParty::ResourcingDsl
      
      # When we subclass Resource, we want to add a few methods to the Resources class
      # This will anable us to call out to these resources in our DSLified manner
      # When we call a method from the subclass, say it's the File class
      # then we want to be able to have the method file() available.
      # We also want to be able to fetch the resource with a get_file method.
      # This will just call out to get the resource. If the resource isn't available
      # in a resource store, we expect to return a nil result. 
      # Finally, the has_ and does_not_have_ methods are appended. See below for 
      # those methods. Then we make sure we add these resources as available_resources
      # onto the class so we know it's available as a resource
      def self.inherited(subclass)
        subclass = subclass.to_s.split("::")[-1] if subclass.to_s.index("::")
        lowercase_class_name = subclass.to_s.underscore.downcase || subclass.downcase
        method_name = "__#{lowercase_class_name}".to_sym
        
        # Add add resource method to the Resources module
        unless PoolParty::PoolPartyBaseClass.respond_to?(method_name)
          method =<<-EOE
            private 
            def #{method_name}(opts={}, &blk)              
              add_resource(:#{lowercase_class_name}, opts, &blk)
            end
            def get_#{lowercase_class_name}(n, opts={}, &block)
              res = get_resource(:#{lowercase_class_name}, n, opts, &block)
              raise PackageException.new("Oops. Check that you specified the #{lowercase_class_name} \#\{n\}.") unless res
              res
            end
            public
          EOE
          PoolParty::PoolPartyBaseClass.module_eval method
          PoolParty::PoolPartyBaseClass.add_has_and_does_not_have_methods_for(lowercase_class_name.to_sym)
          
          available_resources << subclass
        end
      end
      
      # Keep track of the resources that are available. This way we can show some pretty output
      # later and ensure that we are only calling available resources
      def self.available_resources
        @available_resources ||= []
      end
      
      # This is set in order of descending precedence
      # The options are overwritten from the bottom up
      # and the resource will use those as the values
      # Then it takes the value of the block and sets whatever is sent there as 
      # the options
      # Finally, it uses the parent's options as the lowest priority
      def initialize(opts={}, extra_opts={}, &block)
        super(opts, extra_opts, &block)
                
        @resource_name = @base_name
        dsl_options[:name] = resource_name unless dsl_options.has_key?(:name)
        
        loaded(opts, &block)
        
        after_create
      end
            
      # Stub, so you can create virtual resources
      # This is called after the resource is initialized
      # with the options given to it in the init-block
      def loaded(opts={}, &block)
      end
      
      def resource_name
        @resource_name ||= nil
      end
      
      def name(*args)
        resource_name
      end
      
      # After create callback
      def after_create
      end
      
      # We don't want to inherit the services on a resource, as resources
      # are a base and should never have services.
      def services
      end
      
      def cloud
        2.upto(context_stack.size) do |i|
          return ::PoolParty.context_stack[-i] if ::PoolParty.context_stack[-i].is_a?(PoolParty::Cloud::Cloud)
        end
        nil
      end
      
      def duplicatable?
        false
      end
      def resource?
        true
      end
      # This way we can subclass resources without worry
      def class_type_name
        self.class.to_s.top_level_class.underscore.downcase
      end
      def self.custom_function(str)
        custom_functions << str
      end
      def self.custom_functions
        @custom_functions ||= []
      end
      def custom_function(str)
        self.class.custom_functions << str
      end
            
      def self.custom_functions_to_string(pre="")
        returning Array.new do |output|
          PoolParty::Resources.available_custom_resources.each do |resource|
            resource.custom_functions.each do |func|
              output << "#{pre*2}#{func}"
            end
          end
        end.join("\n")
      end
      # Some things in puppet aren't allowed, so let's override them here
      def disallowed_options
        []
      end

      def key
        name
      end
      # def virtual_resource?
      #   false
      # end
      # def printable?
      #   true
      # end
      def is_in_plugin?
        parent.is_plugin?
      end
      
      def is_a_resource?
        true
      end
      
      def method_missing(m,*a,&block)
        if parent && parent.dsl_options.has_key?(m) && is_in_plugin?
          parent.send m, *a, &block
        else
          super
        end
      end
      
      # Private method just for resource retrievling purposes
      def class_name_sym
        self.class.to_s.top_level_class.downcase.to_sym
      end
      # We want to gather the options, but if the option sent is nil
      # then we want to override the option value by sending the key as
      # a method so that we can override this if necessary. 
      # Only runs on objects that have options defined, otherwise 
      # it returns an empty hash
      def get_modified_options
        unless @modified_options
          if dsl_options
            opts = dsl_options.inject({}) do |sum,h|
              sum.merge!({h[0].to_sym => ((h[1].nil?) ? self.send(h[0].to_sym) : h[1]) })
            end
          else
            opts = {}
          end
          @full_allowed_options ||= allowed_options.reject {|ele| disallowed_options.include?(ele) }
          @modified_options = opts.reject do |k,v|
            !@full_allowed_options.include?(k) || 
              @parent && @parent.respond_to?(:dsl_options) && @parent != self && @parent.dsl_options.has_key?(k) && @parent.dsl_options[k] == dsl_options[k]
          end
        end
        @modified_options
      end
      
    end
    
  end
end

Dir["#{::File.dirname(__FILE__)}/../resources/*.rb"].each {|lib| require "#{lib}"}