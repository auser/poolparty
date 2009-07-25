module PoolParty
  module Resources
    
    class Directory < Resource
      
      default_options(
        :recursive  => true,
        :mode       => "0644",
        :owner      => "root",
        :group      => "root",
        :recursive  => false
      )
      
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