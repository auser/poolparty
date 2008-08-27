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
      resources[type] ||= ("PoolParty::Resources::#{type.to_s.capitalize}".classify.constantize.new)
    end
    
    #:nodoc:
    def reset_resources!
      resources.each {|k,v| resources[k] = nil}
    end
    
    def resources_string
      returning Array.new do |output|
        resources.each do |type, resource|
          output << "# #{type}"
          output << resource.to_string
        end
      end.join("\n")
    end
    
    class Resource
      include MethodMissingSugar
      include Configurable
      
      def initialize(opts={}, &block)
        set_vars_from_options(opts) unless opts.empty?
        self.instance_eval &block if block
        push self
      end
      def set_vars_from_options(opts={})
        opts.each {|k,v| self.send k.to_sym, v } unless opts.empty?
      end
      # Generic to_s
      # Most Resources won't need to extend this
      def to_string(prev="")
        returning Array.new do |output|
          output << "#{prev}#{self.class.to_s.top_level_class} {"
            instances.each do |resource|
              output << "#{prev}\t#{resource.name}:"
              output << resource.options.flush_out("#{prev}\t\t",";")
            end
          output << "#{prev}}"
        end.join("\n")
      end
      # Each container has instances      
      def instances
        @instances ||= []
      end
      def <<(*args)
        args.each {|arg| instances << arg if can_add_instance?(arg) }
        self
      end
      alias_method :push, :<<
            
      def instance_named(name="")
        instances.select {|a| a.name == name }.first
      end
      def contains_instance_named?(name="")
        !instance_named(name).nil?
      end
      def has_name?(instance)
        (instance.name && !instance.name.empty?)
      end
      def can_add_instance?(instance)
        has_name?(instance) && !contains_instance_named?(instance.name)
      end
    end
    
  end
end