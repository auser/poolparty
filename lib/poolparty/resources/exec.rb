=begin rdoc rdoc
== Exec
 
Ensure a command is run on the instances
 
== Usage
 
  has_exec(:name => '...') do
    # More options. 
    # This block is optional
  end
 
== Options
 
* <tt>name</tt> The name of your exec. This is optional, but nice for debugging purposes
* <tt>cwd</tt> Current working directory to execute the command (optional)
* <tt>command</tt> This describes the command to run
* <tt>path</tt> The path to run the command with (optional)
 
== Examples
  has_exec 'ps aux | grep erl | mail -s "running commands" root@root.com' do
    onlyif => 'ps aux | grep beam'
  end
=end
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
        :user         => nil,
        :action       => :run
      )
      
      def print_to_chef
str = 'execute "<%= name %>" do
  command <%= print_variable(command || name) %>
  path <%= print_variable(path) %>
  action <%= print_variable(action ? action : (exists ? :run : :nothing)) %>
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
