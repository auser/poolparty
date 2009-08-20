module PoolParty
  class Resource < Base
    
    attr_reader :exists
    attr_accessor :meta_notifies, :meta_not_if, :meta_only_if, :meta_subscribes
    attr_accessor :graph_index
    
    default_options(
      :cloud          => nil,
      :name           => to_s.top_level_class,
      :ignore_failure => nil,
      :provider       => nil
    )
    
    def initialize(opts={}, extra_opts={}, &block)
      @exists = true
      super
      valid?
    end
    
    # Dependency resolver methods
    def compile(compiler)
      @compiler ||= PoolParty.module_eval("DependencyResolvers::#{compiler.to_s.capitalize}")
      @compiler.compile(self)
    end
    
    # print_to_chef
    # When the dependency resolver comes through and resolves
    # this resource, it will come through and check if it resolves
    # to chef by checking it it responds to the 
    #  print_to_chef
    # method. The contents of the method are considered an ERB
    # template and will be rendered as an ERB template.
    def print_to_chef
      <<-EOE
# <%= has_method_name %>
      EOE
    end
    
    # META FUNCTIONS
    # ALL RESOURCES HAVE THESE METHODS AVAILABLE
    def notifies(other_resources_hash, action_to_take=:reload)
      @meta_notifies ||= {}
      other_resources_hash.each do |k,v|
        notifies_array = (@meta_notifies[k] ||= [])
        notifies_array << [v, action_to_take] unless notifies_array.include?([v, action_to_take])
        # Implicitly add a require
        # requires(k => v)
      end
    end
    
    def subscribes(other_resources_hash, action_to_take=:reload, at_time=:delayed)
      @meta_subscribes ||= {}
      other_resources_hash.each do |k,v|
        subscribes_array = (@meta_subscribes[k] ||= [])
        subscribes_array << [v, action_to_take, at_time] unless subscribes_array.include?([v, action_to_take, at_time])
      end
    end
    
    # Requires
    def requires(other_resources_obj)
      case other_resources_obj
      when Hash
        other_resources_obj.each do |k,v|
          dependencies[k] ||= []
          dependencies[k] << v unless dependencies[k].include?(v)
        end
      when Array
        other_resources_obj.each do |obj|
          requires(obj)
        end
      end
    end
    
    # Not if
    # If a block is given with the not_if, we assume it is
    # a proc object so we grab the proc source code
    # on both not_if and only_if code
    def not_if(code_str=nil, &block)
      @meta_not_if = block ? [block.code, :block] : [code_str, :string]
    end
    
    # Run only if
    def only_if(code_str=nil, &block)
      @meta_only_if = block ? [block.code, :block] : [code_str, :string]
    end
        
    # Should this resource exist on the remote systems
    # which is a lookup of the instance variable 
    # on the instance of the resource
    # The default is that the resource DOES exist    
    alias :exists? :exists
    
    # The resource exists in the output and should be created
    # on the remote systems.
    def exists!
      @exists = true
    end
    
    # The resource should be removed or deleted from the remote
    # system
    def does_not_exist!
      @exists = false
      false
    end
    
    # CALLBACKS
    def before_compile
    end
    
    def after_compile
    end
    
    # Singleton methods
    # has_name
    # The has_ and does_not_have methods names
    # are considered, unless otherwise denoted to be 
    # the top level class name
    # for instance
    #   class Tengo < Resource
    #   end
    # the has_ method will be
    #   has_tengo
    def self.has_method_name
      to_s.top_level_class
    end
    
    # has_method_name alias for the singleton method has_method_name
    # so that there is access to the has_method_name on the instance
    def has_method_name
      self.class.has_method_name
    end
    
    # DSL METHODS
    # Get access to the cloud that contains this resource
    def cloud
      get_parent_of_class(PoolParty::Cloud)
    end
    # Get access to the pool that contains this resource
    def pool
      get_parent_of_class(PoolParty::Pool)
    end
    
    def case_of(var, &block)
    end
    
    # Define the resource methods for all the resources sublcassed by Resource
    # this creates the methods:
    #   has_<resource_name>
    #   does_not_have_<resource_name>
    #   <resource_name>
    # on the Base class
    # The has_ method calls exists! on the resource, then places the resource
    # in the ordered_resources array
    def self.define_resource_methods
      defined_resources.each do |res|
        next if res.method_defined?
        ddputs "Defining resource: #{res} as #{res.has_method_name}"
        define_resource(res)
        res.method_defined!
        unless res.defined_resources.empty?
          res.define_resource_methods
        end
      end
    end
    
    # Define the resource on the base class so it's available across all
    # PoolParty classes that use Base
    def self.define_resource(res)
      Base.class_eval <<-EOE
        def has_#{res.has_method_name}(a={},b={},&block)
          obj = #{res}.new(a,b,&block)
          obj.exists!
          resources << obj
          obj
        end
        def does_not_have_#{res.has_method_name}(a={},b={},&block)
          obj = has_#{res.has_method_name}(a,b,&block)
          obj.does_not_exist!
          obj
        end
        def #{res.has_method_name}s
          all_resources.select {|q| q if q.class.to_s =~ /#{res.to_s.classify}/ }
        end
        alias :#{res.has_method_name} :has_#{res.has_method_name}
        
        def get_#{res.has_method_name}(nm)
          {:#{res.has_method_name} => nm}
        end
      EOE
    end
    
    # When a new resource is created, the class gets stored as a defined resource
    # in the defined_resources resources class variable
    def self.inherited(subclass)
      defined_resources << subclass
    end
    
    # Note that this resource has been defined already
    def self.method_defined!
      @defined = true
    end
    
    # Query if this resource has been defined yet
    def self.method_defined?
      defined
    end
    
    def self.defined
      @defined ||= false
    end
    
    # Storage of defined resources that are stored when
    # the subclass'd resource is subclassed
    def self.defined_resources
      @defined_resources ||= []
    end
    
    # HELPERS FOR RESOURCES
    # Print objects
    # This helper takes an object and prints them out with as expected
    # Case of:
    #   Number:
    #     Integer of the format \d\d\d      => 0644
    #     Else                              => 79
    #   String
    #     String of the format \d\d\d\d     => 0655
    #     String of the format \d\d\d       => 0644
    #     Else                              => "String"
    #   Proc object
    #     Calls the proc object
    #   Array
    #     All                               => [ "a", "b" ]
    #   Symbol
    #     All                               => :a
    #   Hash
    #     All                               => :a => "a", :b => ["b"]
    #   Object
    #     All                               => object
    def print_variable(obj)
      case obj
      when Fixnum
        case obj
        when /^\d{3}$/
          "0#{obj.to_i}"
        else
          "#{obj.to_i}"
        end        
      when String
        case obj
        when /^\d{4}$/
          "#{obj}"
        when /^\d{3}$/
          "0#{obj}"
        else
          "\"#{obj}\""
        end
      when Proc
        obj.call # eh
      when Array
        "[ #{obj.map {|e| print_variable(e) }.reject {|a| a.nil? || a.empty? }.join(", ")} ]"
      when nil
        nil
      when Symbol
        ":#{obj}"
      when Hash
        "#{obj.map {|k,v| ":#{k} => #{print_variable(v)}" unless v == obj }.compact.join(",\n")}"
      else
        "#{obj}"
      end
    end
    
    
    private
    # Get parent of class
    def get_parent_of_class(klass)
      if parent.is_a? klass
        parent
      elsif parent && !parent.is_a?(PoolParty::Pool)
        parent.cloud
      else
        nil
      end      
    end
  end
end

Dir["#{File.dirname(__FILE__)}/resources/*.rb"].each {|lib| require lib }