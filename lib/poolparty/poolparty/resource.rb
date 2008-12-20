=begin rdoc
  The Container

  Container holds the various features for the final compilations
  for each pool.
  
  Most of the Resources will not need to create their own
=end
module PoolParty    
  module Resources
    
    def resources
      @resources ||= {}
    end
    
    def resource(type=:file)
      resources[type] ||= []
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
    def add_resource(type, opts={}, parent=self, &block)
      temp_name = (opts[:name] || "#{type}_#{type.to_s.keyerize}")
      if in_a_resource_store?(type, temp_name)
        @res = get_from_local_resource_store(type, temp_name, parent)
        @res ||= get_from_global_resource_store(type, temp_name)
      else
        @res = returning "PoolParty::Resources::#{type.to_s.camelize}".camelize.constantize.new(opts, parent, &block) do |o|                    
          store_into_global_resource_store(o)
          resource(type) << o          
        end        
      end
      @res
    end
    def should_duplicate_resource?(type, res, pare, opts)
      pare != @res.parent && 
        pare.cloud != @res.cloud && 
        in_global_resource_store?(type, opts[:name]) && 
        !in_resources?(type, res, pare)
    end
    def get_resource(ty, key, parent=self)
      get_from_local_resource_store(ty, key, parent) || get_from_global_resource_store(ty, key)
    end
    def get_from_local_resource_store(type, key, parent)
      resource(type).select {|r| r.key == key }.first
    end
    def in_a_resource_store?(type, key, parent=self)
      !(get_resource(type, key) && in_global_resource_store?(type, key)).nil?
    end
    def in_resources?(type, key, parent=self)
      !get_resource(type, key).nil?
    end
    def global_resources_store
      $global_resources ||= []
    end
    def store_into_global_resource_store(r)
      global_resources_store << r unless in_global_resource_store?(r.class_name_sym, r.key)
    end
    def get_from_global_resource_store(ty, key)
      global_resources_store.select {|r| r if r.same_resources_of(ty, key) }.first
    end
    def in_global_resource_store?(ty, key)
      !get_from_global_resource_store(ty, key).nil?
    end
    #:nodoc:
    def reset_resources!
      $global_resources = $global_classpackage_store = @resources = nil
    end
        
    # def resources_string(pre="")
    #   returning Array.new do |output|        
    #     output << resources_string_from_resources(resources)
    #   end.join("\n")
    # end
    
    def custom_file(path, str)
      write_to_file_in_storage_directory(path, str)
    end
        
    class Resource
      attr_accessor :prestring, :poststring
      
      include CloudResourcer
      include Configurable
      # For the time being, we'll make puppet the only available dependency resolution
      # base, but in the future, we can rip this out and make it an option
      include PoolParty::DependencyResolutions::Puppet
      # DSL Overriders
      include PoolParty::ResourcingDsl
      
      extend PoolParty::Resources
      include PoolParty::Resources
      
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
        
        # Add add resource method to the Resources module
        unless PoolParty::Resources.respond_to?(lowercase_class_name.to_sym)          
          method =<<-EOE
            def #{lowercase_class_name}(opts={}, parent=self, &blk)
              add_resource(:#{lowercase_class_name}, opts, parent, &blk)
            end
            def get_#{lowercase_class_name}(n, opts={}, parent=self, &block)
              in_a_resource_store?(:#{lowercase_class_name}, n) ?
                get_resource(:#{lowercase_class_name}, n) : nil
            end
          EOE
          PoolParty::Resources.module_eval method
          PoolParty::Resources.add_has_and_does_not_have_methods_for(lowercase_class_name.to_sym)
          
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
      def initialize(opts={}, parent=self, &block)                        
        run_setup(parent, &block)
        # Take the options of the parents
        set_vars_from_options(opts) unless opts.empty?
        
        set_resource_parent
        
        loaded(opts, @parent, &block)
      end
      
      # Helper to set the containing parent on the resource
      def set_resource_parent
        if @parent && @parent != self
          if can_set_requires_for_parent
            # requires @parent.to_s
          end
        end
      end
      
      def can_set_requires_for_parent
        @parent.is_a?(PoolParty::Resources::Resource) && 
          @parent.printable? && 
          @parent.name != name &&
          !@parent.is_a?(PoolParty::Resources::Classpackage)
      end
            
      # Stub, so you can create virtual resources
      # This is called after the resource is initialized
      # with the options given to it in the init-block
      def loaded(opts={}, parent=self)
      end
      
      def cloud
        @pa = parent
        while !(@pa.is_a?(PoolParty::Cloud::Cloud) || @pa.nil? || @pa == self)
          @pa = @pa.respond_to?(:parent) ? @pa.parent : nil
        end
        @pa
      end
      
      def parent_tree
        @pa = self
        returning Array.new do |arr|
          while !(@pa.is_a?(PoolParty::Cloud::Cloud) || @pa.nil? || @pa == self)
            @pa = @pa.respond_to?(:parent) ? @pa.parent : nil
            arr << @pa
          end
        end
      end
      
      def same_resources_of(t, k)
        key == k && class_name_sym == t
      end
      def duplicatable?
        false
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
      def allowed_options
        [
          :subscribe, :owner, :group, :path, :mode, :source, :notify, :subscribe, :check, :creates, :cwd, :command, :ensure,
          :require, :schedule, :range, :alias, :hour, :minute, :user, :month, :monthday, :name, :onlyif, :unless, :refreshonly,
          :refresh, :content, :template, :ip, :repeat, :provider, :key, :device, :fstype, :remounts, :options, :atboot, :before,
          :binary, :status, :start, :stop, :restart, :pattern, :recurse, :home
        ]
      end
      def key
        name
      end
      def virtual_resource?
        false
      end
      def printable?
        true
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
          if options
            opts = options.inject({}) do |sum,h|
              sum.merge!({h[0].to_sym => ((h[1].nil?) ? self.send(h[0].to_sym) : h[1]) })
            end
          else
            opts = {}
          end
          @full_allowed_options ||= allowed_options.reject {|ele| disallowed_options.include?(ele) }
          @modified_options = opts.reject do |k,v|
            !@full_allowed_options.include?(k) || 
              @parent && @parent.respond_to?(:options) && @parent != self && @parent.options.has_key?(k) && @parent.options[k] == options[k]
          end
        end
        @modified_options
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
    def self.add_has_and_does_not_have_methods_for(type=:file)
      module_eval <<-EOE
        def has_#{type}(opts={}, parent=self, &block)
          #{type}(handle_option_values(opts).merge(:ensures => "present"), parent, &block)
        end
        def does_not_have_#{type}(opts={}, parent=self, &block)
          #{type}(handle_option_values(opts).merge(:ensures => "absent"), parent, &block)
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