module PoolParty
  module Resources
    
    class Exec < Resource
      
      default_options(
        :path         => ["/usr/bin:/bin:/usr/local/bin:$PATH"],
        :command      => nil,
        :creates      => nil,
        :cwd          => nil,
        :environment  => nil,
        :group        => nil,
        :returns      => nil,
        :user         => nil
      )
      
      def print_to_chef
str = 'execute "<%= name %>" do
  command <%= print_variable(command || name) %>
  path <%= print_variable(path) %>
  action :<%= exists ? :run : :nothing %>
'
      str << "  creates <%= print_variable(creates) %>\n" if creates
      str << "  cwd <%= print_variable(cwd) %>\n" if cwd
      str << "  environment <%= print_variable(environment) %>\n" if environment
      str << "  group <%= print_variable(group) %>\n" if group
      str << "  returns <%= print_variable(returns) %>\n" if returns
      str << "  user <%= print_variable(user) %>\n" if user
      
      str << "end"
      end
      
    end
    
  end
end