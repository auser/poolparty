=begin rdoc
== Service

The service resource specifies a service that must be running on the nodes

== Usage

  has_service(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> Name of the service to be running

== Examples

  has_service(:name => "apache2")
=end
module PoolParty
  module Resources
    
    class Service < Resource
      
      default_options(
        :action           => nil,
        :enabled          => nil,
        :running          => nil,
        :pattern          => nil,
        :start_command    => nil,
        :stop_command     => nil,
        :status_command   => nil,
        :restart_command  => nil,
        :reload_command   => nil,
        :supports         => nil
      )
      
      def print_to_chef
str = 'service "<%= name %>" do
  pattern <%= print_variable(pattern || name) %>
  action :<%= action ? print_variable(action) : (exists ? :enable : :disable) %>
'
        str << "  running <%= print_variable(running) %>\n" if running
        str << "  start_command <%= print_variable(start_command) %>\n" if start_command
        str << "  stop_command <%= print_variable(stop_command) %>\n" if stop_command
        str << "  status_command <%= print_variable(status_command) %>\n" if status_command
        str << "  restart_command <%= print_variable(restart_command) %>\n" if restart_command
        str << "  reload_command <%= print_variable(reload_command) %>\n" if reload_command
        str << "  supports <%= print_variable(supports) %>\n" if supports
        
        str << "end"
      end
      
    end
    
  end
end