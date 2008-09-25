require File.dirname(__FILE__) + "/resource"

module PoolParty
  def define_resource(name, &block)
    symc = "#{name}".classify
    klass = "#{symc}".class_constant(PoolParty::Resources::CustomResource, {:preserve => true}, &block)
    PoolParty::Resources::CustomMethods.add_methods_from(name, &block)
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
        @name = name
        super(opts, &block)
      end
      
      def resources
        @resources ||= {}
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
      def self.added_methods
        @added_methods ||= {}
      end
      def self.add_methods_from(mod_name, &block)
        @old_methods = available_methods
        module_eval &block
        added_methods[mod_name] = (available_methods - @old_methods)
        mod_name
      end
      def self.call_method(method, *args, &block)        
        return nil unless available_methods.include?(method)
        mod_name = added_methods.reject {|k,v| k unless v.include?(method) }.keys.first
        added_methods[mod_name.downcase.to_sym].send method, *args, &block
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