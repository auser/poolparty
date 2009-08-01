=begin rdoc
  Gives access to conditional statements
  
  == Usage
    has_case "os" do
      when_is :ubuntu, "git-core"
      else_is "git"
    end
  
  translates to:
  
    case "os"
    when :ubuntu
      "git-core"
    else
      "git"
    end
=end
module PoolParty
  module Resources
    
    class Conditional < Resource
      
      default_options(
        :name => nil
      )
      
      def self.has_method_name
        "case"
      end
      
      # When is
      def when_is(val, output)
        conditions << [val, output]
      end
      
      def else_is(output)
        conditions.push([:else, output])
      end
      
      def conditions
        @conditions ||= []
      end
      
      def print_to_chef
        arr = []
        arr << "case #{print_variable(name)}"
        conditions.each do |cond|
          if cond[0] == :else
            arr << "else"
            arr << "  #{print_variable(cond[1])}"
            break
          else
            arr << "when #{print_variable(cond[0])}"
            arr << "  #{print_variable(cond[1])}"
          end
        end
        arr << "end"
        arr.join("\n")
      end
      
    end
    
  end
end