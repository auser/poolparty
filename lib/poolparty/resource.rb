module PoolParty
  class Resource < Base
    
    attr_reader :exists
    
    default_options()
        
    # Dependency resolver methods
    
    # print_to_chef
    # When the dependency resolver comes through and resolves
    # this resource, it will come through and check if it resolves
    # to chef by checking it it responds to the 
    #  print_to_chef
    # method. The contents of the method are considered an ERB
    # template and will be rendered as an ERB template.
    def print_to_chef
      <<-EOE
        # <%= self.class.has_method_name %>
        <%= ordered_resources.each do |res| %>
            #{res.print_to_chef}
        <% end %>
      EOE
    end
    
    # Should this resource exist on the remote systems
    # which is a lookup of the instance variable 
    # on the instance of the resource
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
    
    # Define the resource methods for all the resources sublcassed by Resource
    # this creates the methods:
    #   has_<resource_name>
    #   does_not_have_<resource_name>
    #   <resource_name>
    # on the Base class
    # The has_ method calls exists! on the resource, then places the resource
    # in the ordered_resources array
    def self.define_resource_methods
      ddputs "Defining resources..."
      defined_resources.each do |res|
        ddputs "Defining resource: #{res}"
        Base.class_eval <<-EOE
          def has_#{res.has_method_name}(a={},b={},&block)
            ddputs "New #{res.has_method_name}(\#{a.inspect}, \#{b.inspect}, \#{block})"
            obj = #{res}.new(a,b,&block)
            obj.exists!
            ordered_resources << obj
            obj
          end
          def does_not_have_#{res.has_method_name}(a={},b={},&block)
            obj = has_#{res.has_method_name}(a,b,&block)
            obj.does_not_exist!
            obj
          end
          def #{res.has_method_name}s
            ordered_resources.select {|q| q if q.class.to_s =~ /#{res.to_s.classify}/ }
          end
          alias :#{res.has_method_name} :has_#{res.has_method_name}
        EOE
      end
    end
    
    # When a new resource is created, the class gets stored as a defined resource
    # in the defined_resources resources class variable
    def self.inherited(bclass)
      defined_resources << bclass
    end
    
    # Storage of defined resources that are stored when
    # the subclass'd resource is subclassed
    def self.defined_resources
      @defined_resources ||= []
    end
    
    
  end
end

Dir["#{File.dirname(__FILE__)}/resources/*.rb"].each {|lib| require lib }