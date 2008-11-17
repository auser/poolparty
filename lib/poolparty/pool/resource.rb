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
    
    def add_resource(type, opts={}, parent=self, &block)      
      if in_resources?(type, opts[:name])        
        get_resource(type, opts[:name], parent)
      else
        returning "PoolParty::Resources::#{type.to_s.camelize}".classify.constantize.new(opts, parent, &block) do |o|                    
          store_into_global_resource_store(o)
          resource(type) << o          
        end
      end
    end    
    def get_resource(ty, key, parent=self)
      resource(ty).select {|r| r.key == key }.first || get_from_global_resource_store(ty, key)
    end
    def in_resources?(type, key, parent=self)
      !(get_resource(type, key) && in_global_resource_store?(type, key)).nil?
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
      $global_resources = @resources = nil
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
      
      extend PoolParty::Resources
      include PoolParty::Resources
      
      def self.inherited(subclass)
        subclass = subclass.to_s.split("::")[-1] if subclass.to_s.index("::")
        lowercase_class_name = subclass.to_s.downcase
        
        # Add add resource method to the Resources module
        unless PoolParty::Resources.respond_to?(lowercase_class_name.to_sym)          
          method =<<-EOE
            def #{lowercase_class_name}(opts={}, parent=self, &blk)
              add_resource(:#{lowercase_class_name}, opts, parent, &blk)
            end
            def get_#{lowercase_class_name}(name)              
              get_resource(:#{lowercase_class_name}, name) if in_resources?(:#{lowercase_class_name}, name)
            end
          EOE
          PoolParty::Resources.module_eval method
          PoolParty::Resources.add_has_and_does_not_have_methods_for(lowercase_class_name.to_sym)
          
          available_resources << subclass
        end
      end
      
      def self.available_resources
        @available_resources ||= []
      end
      
      def self.available_resource_methods
        available_resources.map {|a| a.my_methods }
      end
      
      # This is set in order of descending precedence
      # The options are overwritten from the bottom up
      # and the resource will use those as the values
      # Then it takes the value of the block and sets whatever is sent there as 
      # the options
      # Finally, it uses the parent's options as the lowest priority
      def initialize(opts={}, parent=self, &block)
        # Take the options of the parents
        set_vars_from_options(opts) unless opts.empty?
        set_resource_parent(parent)
        self.run_in_context &block if block
        loaded(opts, @parent)
      end
      
      # Helper to set the containing parent on the resource
      def set_resource_parent(parent=nil)
        if parent && parent != self
          @parent = parent
          if @parent.is_a?(PoolParty::Resources::Resource) && @parent.printable? && @parent.name != name
            # requires @parent.to_s
          end
        end
      end
            
      # Stub, so you can create virtual resources
      # This is called after the resource is initialized
      # with the options given to it in the init-block
      def loaded(opts={}, parent=self)
      end
      
      # DSL Overriders
      include PoolParty::ResourcingDsl
      
      def same_resources_of(t, k)
        key == k && class_name_sym == t && !duplicatable?
      end
      def duplicatable?
        false
      end
      # This way we can subclass resources without worry
      def class_type_name
        self.class.to_s.top_level_class
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
      
      def cloud
        @p = parent
        while !@p.is_a?(PoolParty::Cloud)
          @p = @p.parent
        end
        @p
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
          :binary, :status, :start, :stop, :restart, :pattern, :recurse
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
          @modified_options = opts.reject {|k,v| !@full_allowed_options.include?(k) }
        end
        @modified_options
      end
      
      # For the time being, we'll make puppet the only available dependency resolution
      # base, but in the future, we can rip this out and make it an option
      include PoolParty::DependencyResolutions::Puppet
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
          #{type}(#{type == :exec ? "opts" : "{:is_present => ''}.merge(opts)"}, parent, &block)
        end
        def does_not_have_#{type}(opts={}, parent=self, &block)
          #{type}(#{type == :exec ? "opts" : "{:is_absent => ''}.merge(opts)"}, parent, &block)
        end
      EOE
    end
    
  end
end