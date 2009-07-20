module PoolParty
  module Resources
    
    class Script < Exec
            
      default_options(
        :code => nil,
        :interpreter => nil,
        # Exec
        :path => ["/usr/bin:/bin:/usr/local/bin:$PATH"],
        :command => nil,
        :creates => nil,
        :cwd => nil,
        :environment => nil,
        :group => nil,
        :returns => nil,
        :user => nil
      )
            
      def print_to_chef
str = 'script "<%= name %>" do
  code "<%= code %>"
'
      str << "  interpreter <%= print_variable(interpreter) %>\n" if interpreter
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