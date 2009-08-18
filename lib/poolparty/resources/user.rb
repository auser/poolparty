=begin rdoc
== User

The user resource ensures there is a user created on the nodes

== Usage

  has_user(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> Name of the service to be running
* <tt>comment</tt> Comment about the user
* <tt>uid</tt> uid of the user
* <tt>gid</tt> gid of the user
* <tt>home</tt> Home directory of the user
* <tt>shell</tt> Shell type of the user
* <tt>password</tt> The hashed password

== Examples

  has_user "fred" do
    home "/home/fred"
  end
=end
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