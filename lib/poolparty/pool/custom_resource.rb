require File.dirname(__FILE__) + "/resource"

module PoolParty
  def define_resource(name, &block)
    symc = "#{name}".classify
    klass = "#{symc}".class_constant(PoolParty::Resources::CustomResource, {:preserve => true}, &block)
    PoolParty::Resources::CustomMethods.module_eval &block
    klass
  end
  
  module Resources
    
    def call_function(str, opts={}, &block)
      puts "call_function: #{str}"
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
          arr << @str
        end.join("\n")
      end
    end
    
    class CustomResource < Resource
      def initialize(name=:custom_method, opts={}, &block)
        puts "CustomResource: #{name}"
        @name = name
        super(opts, &block)
      end
      
      def self.custom_function(str)
        custom_functions << str
      end
      
      def self.custom_functions
        @custom_functions ||= []
      end
      
      def self.custom_functions_to_string(prev="")
        returning Array.new do |output|
          custom_functions.each do |function_string|
            output << "#{prev}\t#{function_string}"
          end
        end.join("\n")
      end      
      
      def to_string(prev="")
        returning Array.new do |output|
          output << "#{prev} # Custom Functions\n"
          output << self.class.custom_functions_to_string(prev)
        end.join("\n")        
      end            
    end
    
    # A module just to store CustomMethods
    class CustomMethods
      def added_modules
        @added_modules ||= {}
      end
      def self.add_methods_from(mod_name, &block)
      end
      def self.custom_function(*args)        
      end
      def self.available_methods
        (self.methods + instance.methods).sort - Module.methods
      end
      def self.instance
        @instance ||= new
      end
    end
    
  end    
end