module PoolParty    
  module Resources
        
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
    class Variable < Resource
      dsl_methods :name, :value
    end
    
  end
end