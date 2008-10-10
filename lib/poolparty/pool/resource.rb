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
      returning "PoolParty::Resources::#{type.to_s.camelize}".classify.constantize.new(opts, parent, &block) do |o|
        resource(type) << o
      end
    end
    
    def get_resource(type, name)
      resource(type).select {|resource| resource.name == name }.first
    end
            
    #:nodoc:
    def reset_resources!
      @resources = nil
    end
        
    def resources_string(prev="")
      returning Array.new do |output|        
        output << resources_string_from_resources(resources)
      end.join("\n")
    end
    
    def custom_file(path, str)
      write_to_file_in_storage_directory(path, str)
    end
    
    class Resource
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
        @parent = parent
        set_vars_from_options(opts) unless opts.empty?
        self.instance_eval &block if block
        loaded
      end
      
      # Stub, so you can create virtual resources
      def loaded
      end
      
      # DSL Overriders
      # Overrides for syntax
      # Allows us to send require to require a resource
      def require(str="")
        options[:require]        
      end
      def requires(str="")
        options.merge!(:require => str)
      end 
      def ensures(str="running")
        str == "absent" ? is_absent : is_present
      end
      # Allows us to send an ensure to ensure the presence of a resource
      def is_present(*args)
        options.merge!(:ensure => present)
      end
      # Ensures that what we are sending is absent
      def is_absent(*args)
        options.merge!(:ensure => absent)
      end
      # Alias for unless
      def ifnot(str="")
        options.merge!(:unless => str)
      end
      def present
        "present"
      end
      def absent
        "absent"
      end
      
      # Give us a template to work with on the resource
      # Make sure this template is moved to the tmp directory as well
      def template(file, opts={})
        raise TemplateNotFound.new("no template given") unless file
        raise TemplateNotFound.new("template cannot be found #{file}") unless ::File.file?(file)
        unless opts[:just_copy]          
          options.merge!({:content => "template(\"#{::File.basename(file)}\")"})
          options.delete(:source) if options.has_key?(:source)
          copy_template_to_storage_directory(file)
        else
          copy_file_to_storage_directory(file)
        end
      end
      # This way we can subclass resources without worry
      def class_type_name
        self.class.to_s.top_level_class
      end      
      def self.custom_function(str)
        custom_functions << str
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
      
      def self.custom_functions_to_string(prev="")
        returning Array.new do |output|
          PoolParty::Resources.available_custom_resources.each do |resource|
            resource.custom_functions.each do |func|
              output << "#{prev*2}#{func}"
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
      def virtual_resource?
        false
      end
      # We want to gather the options, but if the option sent is nil
      # then we want to override the option value by sending the key as
      # a method so that we can override this if necessary. 
      # Only runs on objects that have options defined, otherwise 
      # it returns an empty hash
      def get_modified_options
        if options
          opts = options.inject({}) do |sum,h| 
            sum.merge!({h[0].to_sym => ((h[1].nil?) ? self.send(h[0].to_sym) : h[1]) })
          end
        else
          opts = {}
        end
        opts.reject {|k,v| disallowed_options.include?(k) }
      end
      
      # Generic to_s
      # Most Resources won't need to extend this
      def to_string(prev="")
        opts = get_modified_options
        returning Array.new do |output|
          
          if resources && !resources.empty?
            @cp = classpackage_with_self(self)
            output << @cp.to_string
            output << "include #{@cp.name.sanitize}"
          end
          
          unless virtual_resource?
            output << "#{prev}#{class_type_name} {"
            output << "#{prev}\"#{self.key}\":"
            output << opts.flush_out("#{prev*2}").join(",\n")
            output << "#{prev}}"            
          end
                    
        end.join("\n")
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
          #{type}(#{type == :exec ? "opts" : "{:is_present => ''}.merge(opts)"}, parent, &block)
        end
        def does_not_have_#{type}(opts={}, parent=self, &block)
          #{type}(#{type == :exec ? "opts" : "{:is_absent => ''}.merge(opts)"}, parent, &block)
        end
      EOE
    end
    
  end
end