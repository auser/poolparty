module PoolParty
  module Resources
    
    class User < Resource
      
      default_options(
        :action   => nil,
        :comment  => nil,
        :uid      => nil,
        :gid      => nil,
        :home     => nil,
        :shell    => nil,
        :password => nil,
        :supports => {:manage_home => false}
      )
      
      def print_to_chef
str = 'user "<%= name %>" do
  action :<%= action ? print_variable(action) : (exists ? :create : :remove) %>
'
        str << "  comment <%= print_variable(comment) %>\n" if comment
        str << "  uid <%= print_variable(uid) %>\n" if uid
        str << "  gid <%= print_variable(gid) %>\n" if gid
        str << "  home <%= print_variable(home) %>\n" if home
        str << "  shell <%= print_variable(shell) %>\n" if shell
        str << "  password <%= print_variable(password) %>\n" if password
        str << "  supports <%= print_variable(supports) %>\n" if supports
        
        str << "end"
      end
      
    end
    
  end
end