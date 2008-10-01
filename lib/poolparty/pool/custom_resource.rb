require File.dirname(__FILE__) + "/resource"

module PoolParty
  def available_custom_resources
    $available_custom_resources ||= []
  end
  module DefinableResource
    def define_resource(name, &block)
      symc = "#{name}".classify
      klass = symc.class_constant(PoolParty::Resources::CustomResource, {:preserve => true}, &block)
      PoolParty::Resources.module_eval &block
      klass
    end    
  end

  module Resources
    
    def call_function(str, opts={}, &block)
      returning PoolParty::Resources::CallFunction.new(str, opts, &block) do |o|
        resource(:call_function) << o
      end
    end
                
    # Resources for function call
    class CallFunction < Resource
      def initialize(str="", opts={}, &block)
        @str = str
        super(opts, &block)
      end
      def to_string(prev="")
        returning Array.new do |arr|
          arr << "#{prev}#{@str}"
        end.join("\n")
      end
    end
        
    class CustomResource < Resource
      def initialize(name=:custom_method, opts={}, &block)
        @name = name
        super(opts, &block)
      end
      
      def self.inherited(subclass)
        PoolParty::Resources.available_custom_resources << subclass
        super(subclass)
      end
      
      def to_string(prev="")
        returning Array.new do |output|
          output << "#{prev} # Custom Functions\n"
          output << self.class.custom_functions_to_string(prev)
        end.join("\n")        
      end
    end
    
    # Stub methods
    # TODO: Find a better solution
    def custom_function(*args, &block)
    end
    def self.custom_function(*args, &block)
    end      
    
  end    
end