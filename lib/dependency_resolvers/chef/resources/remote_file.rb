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