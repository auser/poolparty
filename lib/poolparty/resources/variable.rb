module PoolParty
  module Resources
    
    class Variable < Resource
      
      default_options(
        :name => nil, 
        :value => nil
      )
      
      def initialize(k, v=nil)
        case k
        when Hash
          super
        else
          if value.is_a?(Hash)
            super(v.merge(:name => k))
          else
            super(:name => k, :value => v)
          end
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