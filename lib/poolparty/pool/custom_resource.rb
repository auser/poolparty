require File.dirname(__FILE__) + "/resource"

module PoolParty
  def available_custom_resources
    $available_custom_resources ||= []
  end

  module Resources
    
    def call_custom_function(str, opts={}, parent=self, &block)
      add_resource(:call_function, opts.merge({:str => str, :name => str.keyerize}), parent, &block)
    end
                
    # Resources for function call
    class CallFunction < Resource
      def to_string(pre="")
        returning Array.new do |arr|
          arr << "#{pre}#{str}"
        end.join("\n")
      end
    end
        
    class CustomResource < Resource
      def self.inherited(subclass)
        PoolParty::Resources.available_custom_resources << subclass
        super
      end
      
      def to_string(pre="")
        returning Array.new do |output|
          output << "#{pre} # Custom Functions\n"
          output << self.class.custom_functions_to_string(pre)
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