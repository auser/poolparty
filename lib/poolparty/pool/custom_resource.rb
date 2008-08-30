require File.dirname(__FILE__) + "/resource"

module PoolParty
  module Resources
    
    def define_resource(name, &block)
      custom_resource(name, &block) << PoolParty::Resources::CustomResource.new(name, &block)
    end
    
    def custom_resource(name, &block)
      resources[:custom_resource] ||= PoolParty::Resources::CustomResource.new(name, &block)
    end
    
    class CustomResource < Resource
      attr_reader :name, :function_string, :function_calls
      
      def initialize(name=:custom_method, &block)
        @name = name
        self.instance_eval &block if block
        valid?
        push self
      end
      
      def valid?
        raise ResourceException.new("#{@name} must define a custom_function") unless @function_string
        raise ResourceException.new("#{@name} must define a custom_usage") unless @function_calls
      end
      
      def custom_function(str)
        @function_string = str
      end
      
      def custom_usage(&block)
        PoolParty::Resources.module_eval &block
        @function_calls = ""
      end
      
      def to_string
        returning Array.new do |output|
          output << "#{prev} # Custom Functions\n"
          instances.each do |resource|
            output << "#{prev}\t#{resource.function_calls}"
          end
        end.join("\n")
        
      end
      
      def function_strings(prev="")
        returning Array.new do |output|
          output << "#{prev} # Custom Functions"
          instances.each do |resource|
            output << "#{prev}\t#{resource.function_string}"
          end
        end.join("\n")
      end
    end        
    
  end    
end