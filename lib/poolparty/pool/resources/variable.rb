module PoolParty    
  module Resources
        
    class Variable < Resource
      
      default_options({
        :name => "var",
        :value => ""
      })
      
      def to_string(prev="\t")
        "$#{name} = #{value_string}"
      end
      
      def value_string
        value.to_option_string
        # case value.class.to_s
        # when "Array"
        #   "[ #{value.map{|a| "'#{a}'"}.join(", ")} ]"
        # else
        #   "'#{value}'"
        # end
      end
      
    end
    
  end
end