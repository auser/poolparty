module Context
  class SharedBehavior < Module
    def self.create_from_behavior(beh) # :nodoc:
      mod = self.new
      mod._behavior = beh
      
      mod
    end
  
    def _behavior=(beh) # :nodoc:
      @_behavior = beh
    end
  
    def included(arg) # :nodoc:
      @_behavior.call
    end
  end
end

class Test::Unit::TestCase
  class << self
    # Share behavior among different contexts.  This creates a module (actually, a Module subclass)
    # that is included using the +use+ method (or one of its aliases) provided by context or +include+ 
    # if you know the module's constant name.
    #
    # ==== Examples
    #   
    #   shared "other things" do
    #     it "should do things but not some things" do
    #       # behavior is fun
    #     end
    #   end
    #
    #   use "other things"
    #   # or...
    #   it_should_behave_like "other things"
    #
    #   shared :client do
    #     it "should be a client to our server" do
    #       # TODO: client behavior here
    #     end
    #   end
    #
    #   use :client
    #   # or...
    #   uses "client"
    #   behaves_like "client"
    #
    def shared(name, &block)
      case name.class.name
      when "String"
        name = name.to_module_name
      when "Symbol"
        name = name.to_s.to_module_name
      else
        raise ArgumentError, "Provide a String or Symbol as the name of the shared behavior group"
      end
      
      Object.const_set(name, Context::SharedBehavior.create_from_behavior(block))
    end
    
    %w(shared_behavior share_as share_behavior_as shared_examples_for).each {|m| alias_method m, :shared}
    
    # Pull in behavior shared by +shared+ or a module.  
    #
    # ==== Examples
    #   
    #   shared "other things" do
    #     it "should do things but not some things" do
    #       # behavior is fun
    #     end
    #   end
    #
    #   use "other things"
    #   # or...
    #   it_should_behave_like "other things"
    #
    #   module Things
    #   end
    #
    #   uses Things
    #
    def use(shared_name)
      case shared_name.class.name
      when "Context::SharedBehavior", "Module"
        include shared_name
      when "String"
        include Object.const_get(shared_name.to_module_name)
      when "Symbol"
        include Object.const_get(shared_name.to_s.to_module_name)
      else
        raise ArgumentError, "Provide a String or Symbol as the name of the shared behavior group or the module name"
      end
    end
    
    %w(uses it_should_behave_like behaves_like uses_examples_from).each {|m| alias_method m, :use}
  end
end