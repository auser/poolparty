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
module PoolParty
  module Resources
    
    class Directory < Resource
      
      default_options(
        :recursive  => true,
        :mode       => "0644",
        :owner      => "root",
        :group      => "root",
        :recursive  => true
      )

      def after_loaded
        requires get_user(owner) if owner && owner != "root"
      end
      
      def print_to_chef
        <<-EOE
directory "<%= name %>" do
  action :<%= exists ? :create : :delete %>
  recursive <%= print_variable(recursive) %>
  mode <%= print_variable(mode) %>
  owner <%= print_variable(owner) %>
  group <%= print_variable(group) %>
end
        EOE
      end
      
    end
    
  end
end
