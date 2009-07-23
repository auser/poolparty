module PoolParty
  module Resources
    
    class Variable < Resource
      
      dsl_methods :name, :value
      
      def initialize(key, value=nil)
        if key.is_a?(Hash)
          super
        else
          super(:name => key, :value => value)
        end        
      end
      
      # Chef uses separate files for variables, so we'll have to open the variable file 
      # and set the variable there
      def print_to_chef
        # Variable
        # TODO: Variable => <%= name %>
        "poolparty[:#{name}] = #{value}"
      end
      
    end
    
  end
end