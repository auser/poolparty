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
    
    def add_resource(type, opts={}, &block)
      returning "PoolParty::Resources::#{type.to_s.camelize}".classify.constantize.new(opts, &block) do |o|
        resource(type) << o
      end
    end
        
    #:nodoc:
    def reset_resources!
      @resources = nil
    end
        
    def resources_string(prev="")
      returning Array.new do |output|
        resources.each do |type, resource|
          output << "#{prev*2}# #{type}"
          output << resource.to_string("#{prev*3}")
        end
      end.join("\n")
    end
    
    def custom_file(path, str)
      write_to_file_in_storage_directory(path, str)
    end
    
    class Resource
      include MethodMissingSugar
      include Configurable
      
      def self.inherited(subclass)
        subclass = subclass.to_s.split("::")[-1] if subclass.to_s.index("::")
        lowercase_class_name = subclass.to_s.downcase
        
        # Add add resource method to the Resources module
        unless PoolParty::Resources.respond_to?(lowercase_class_name.to_sym)
          method =<<-EOE
            def #{lowercase_class_name}(opts={}, &blk)
              add_resource(:#{lowercase_class_name}, opts, &blk)
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
      
      def initialize(opts={}, &block)
        set_vars_from_options(opts) unless opts.empty?
        self.instance_eval &block if block
      end
      def set_vars_from_options(opts={})
        opts.each {|k,v| self.send k.to_sym, v } unless opts.empty?
      end
      def requires(str="")
        options.merge!(:require => str)
      end
      def ensures(str="")
        options.merge!(:ensure => str)
      end
      # Give us a template to work with on the resource
      # Make sure this template is moved to the tmp directory as well
      def template(file)
        raise TemplateNotFound.new("no template given") unless file
        raise TemplateNotFound.new("template cannot be found #{file}") unless ::File.file?(file)
        options.merge!(:template => file)
        copy_file_to_storage_directory(file)
      end
      # Generic to_s
      # Most Resources won't need to extend this
      def to_string(prev="")
        returning Array.new do |output|
          output << "#{prev}#{self.class.to_s.top_level_class} {"
          output << "#{prev}#{self.name}:"
          output << options.flush_out("#{prev*2}",";")
          output << "#{prev}}"
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
    def self.add_has_and_does_not_have_methods_for(type=:file)
      module_eval <<-EOE
        def has_#{type}(opts={}, &block)
          #{type}(opts.merge(:ensure => "present"), &block) 
        end
        def does_not_have_#{type}(opts={}, &block)
          #{type}(opts.merge(:ensure => "absent"), &block)
        end
      EOE
    end
    
  end
end