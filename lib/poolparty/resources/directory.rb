module PoolParty    
  module Resources
        
=begin rdoc rdoc
== Directory

The directory resource is used to describe a directory that should be present
on all of the instances.

== Usage

  has_directory 'name', :from => '/local/path', :to => '/path/on/server/'
  
The above example will place the contents of '/local/path' at '/path/on/server/name'

  has_directory(:name => '/etc/apache2')

== Options

* <tt>name</tt> Describe the location of the file with the name
* <tt>mode</tt> Describe the mode of the file (default: 644)
* <tt>owner</tt> The owner of the file (default: poolparty user)

=end


    class Directory < Resource
      
      dsl_methods :owner
      
      default_options({
        :recursive => true,
        :mode => 0644
      })
      
      def present
        :create
      end
      
    end
    
  end
end