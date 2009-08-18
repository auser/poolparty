=begin rdoc

== Remote Directory

The remote_directory resource is used to describe a file that is hosted on the master instance and propagated to the other instances

== Usage

  has_remote_directory(:name => '...') do
    # More options. 
    # This block is optional
  end

=end
module PoolParty
  module Resources
    
    class RemoteDirectory < Directory
      
      default_options(
        :cookbook     => nil,
        :files_backup => 5,
        :files_owner  => nil,
        :files_group  => nil,
        :files_mode   => "0644",
        :recursive    => true,
        :mode         => "0644",
        :owner        => "root",
        :group        => "root",
        :recursive    => false,
        :source       => nil
      )
      
      def print_to_chef
        str = <<-EOE
remote_directory "<%= name %>" do
  source <%= print_variable(source) %>
  files_backup <%= print_variable(files_backup) %>
  files_mode <%= print_variable(files_mode) %>
  action :<%= exists ? :create : :delete %>
  recursive <%= print_variable(recursive) %>
  mode <%= print_variable(mode) %>
  owner <%= print_variable(owner) %>
  group <%= print_variable(group) %>
EOE
        str << "  files_owner <%= print_variable(files_owner) %>\n" if files_owner
        str << "  files_group <%= print_variable(files_group) %>\n" if files_group
        str << "end"
      end
      
    end
    
  end
end