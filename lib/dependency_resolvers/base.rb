=begin rdoc
  Base dependency_resolver
=end
module DependencyResolvers
  
  class Base
    
    def self.inherited(subclass)
      DependencyResolvers.all << subclass unless DependencyResolvers.all.include?(subclass)
    end
    
    def self.compile_to(resources=[], outdir=Dir.pwd, caller=nil)
      @compile_directory = outdir
      @caller = caller
      compile(resources)
    end
    
    def self.caller
      @caller
    end
    
    def self.compile(resources=[])
      before_compile
      o = case resources
      when PoolParty::Cloud
        compile(resources.ordered_resources)
      when Array
        compile_array(resources)
      when PoolParty::Resource
        compile_resource(resources)
      end
      after_compile(o)
      o
    end
    
    # CALLBACKS
    # Called before anything is compiled
    def self.before_compile
    end
    # Called after everything is compiled
    def self.after_compile(o)
    end
    
    # The name of the method that the resource
    # should respond to to compile
    # Format:
    #   print_to_<dependency_resolver.name>
    def self.compile_method_name
      @compile_method_name ||= "print_to_#{name.to_s.top_level_class}".to_sym
    end
    
    private
    
    # Compile a resource directly
    def self.compile_resource(res)
      return "" unless res.respond_to?(compile_method_name)
      res.before_compile
      po = ProxyObject.new(res, @caller)
      out = po.compile(compile_method_name)
      res.after_compile
      out
    end
    
    # Compile an array of resources
    def self.compile_array(array_of_resources=[])
      out = []
      array_of_resources.each do |res|
        out << compile_resource(res)
      end
      out.join("\n")
    end
    
    def self.compile_directory
      @compile_directory
    end
    
    def self.compile_directory=(d)
      @compile_directory = d
    end
    
    # Print objects
    def self.handle_print_variable(obj)
      case obj
      when Fixnum
        case obj
        when /^\d{3}$/
          "0#{obj.to_i}"
        else
          "#{obj.to_i}"
        end
      when String
        "\"#{obj}\""
      when Proc
        obj.call # eh
      when Array
        # If we are sending a notifies with a second argument
        if obj[1] && [:immediately, :delayed].include?(obj[1])
          "#{handle_print_variable(obj[0])}, :#{obj[1]}"
        else
          "[ #{obj.map {|e| handle_print_variable(e) }.reject {|a| a.nil? || a.empty? }.join(", ")} ]"
        end        
      when nil
        nil
      when Symbol
        ":#{obj}"
      when Hash
        "{#{obj.map {|k,v| ":#{k} => #{handle_print_variable(v)}" unless v == obj }.compact.join(",")}}"
      else
        "#{obj}"
      end
    end
    
  end
  
end
