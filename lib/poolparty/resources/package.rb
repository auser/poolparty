=begin rdoc

== Package

The package resources defines a package that must be present on all of the
instances This will install the "name_of_package" package with the package
provider (apt, yum, etc)

== Usage

  has_package "name_of_package"
  has_package(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The package name. The default provider for your OS will be picked by the dependency resolver

== Examples

  has_package(:name => 'apache2')

=end

module PoolParty
  module Resources
    
    class Package < Resource
      
      default_options(
        :action         => :install,
        :version        => nil,
        :response_file  => nil,
        :source         => nil,
        :options        => nil
      )
      
      def print_to_chef
        str = <<-EOE
package "<%= name %>" do
  action <%= print_variable(action ? action : (exists ? :install : :remove)) %>
EOE
        str << "  options <%= print_variable(options) %>\n" if options
        str << "  version <%= print_variable(version) %>\n" if version
        str << "  source <%= print_variable(source) %>\n" if source
        str << "  response_file <%= print_variable(response_file) %>\n" if response_file
        str << "end"
      end
      
    end
    
  end
end
