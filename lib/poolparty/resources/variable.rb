=begin rdoc
== Variable

Describes a variable to use in a template. One of the powerful hidden features behind puppet is that you can use templating for your templates

== Usage

  has_variable(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The name of the variable to use in the template
* <tt>value</tt> The value of the variable in the template

To use these variables, in your Erb template, reference them like so
  
  name: <%= variablename %>

== Examples

  has_variable(:name => "name", :value => "#{cloud.name}")
=end
module PoolParty
  module Resources
    
    class Variable < Resource
      
      default_options(
        :name => nil, 
        :value => nil
      )
      
      def initialize(k, v=nil, exists=true)
        case k
        when Hash
          super
        else
          if value.is_a?(Hash)
            super(v.merge(:name => k))
          else
            super({:name => k, :value => v})
          end
        end
      end
      
      # Chef uses separate files for variables, so we'll have to open the variable file 
      # and set the variable there
      def print_to_chef
        # Variable
        # TODO: Variable => <%= name %>
        # "poolparty[:#{name}] = #{value}"
        :no_print
      end
      
    end
    
  end
end