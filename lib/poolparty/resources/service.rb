module PoolParty    
  module Resources
        
=begin rdoc
== Service

The service resource specifies a service that must be running on the nodes

== Usage

  has_service(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> Name of the service to be running

== Examples

  has_service(:name => "apache2")
=end
    class Service < Resource
      
      dsl_methods :name       # Name of the service
      
      default_options({
        :ensures => "enable",
        :enable => true
      })
      
      def present
        :start
      end
      def absent
        :stop
      end
      
    end
    
  end
end