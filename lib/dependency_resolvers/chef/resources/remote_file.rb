=begin rdoc

== Remote File

The remote_file resource is used to describe a file that is hosted on the master instance and propagated to the other instances

== Usage

  has_remote_file(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> Describe the location of the file with the name
* <tt>mode</tt> Describe the mode of the file (default: 644)
* <tt>owner</tt> The owner of the file (default: poolparty user)
* <tt>content</tt> The contents of the file
* <tt>source</tt> Used to describe a file that is hosted on the master instance.
* <tt>template</tt> The file contents are described with the template. The location given must be readable
  
To write a file to the template directory, use:

    copy_template_to_storage_directory(filepath)

== Example
  has_remote_file(:name => "/etc/haproxy.cfg") do
    mode 644
    template File.join(File.dirname(__FILE__), "..", "templates/haproxy.conf")
  end
=end

module PoolParty
  module Resources
    
    class RemoteFile < FileResource
      
      default_options(
        :cookbook => nil,
        :source   => nil,
        :checksum => nil,
        :mode     => "0644",
        :backup   => 5,
        :owner    => "root"
      )
      
      def self.has_method_name
        "remote_file"
      end
      
      def print_to_chef
        str = <<-EOE
remote_file "<%= name %>" do
  source <%= print_variable(source) %>
  action :<%= exists? ? :create : :delete %>
  backup <%= backup %>
  mode <%= print_variable(mode) %>
  owner <%= print_variable(owner) %>
EOE
        str << "  cookbook <%= print_variable(cookbook) %>\n" if cookbook
        str << "  checksum <%= print_variable(checksum) %>\n" if checksum
        str << "end"
      end
      
    end
    
  end
end