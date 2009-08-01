=begin rdoc
== Route
  
  Manage the system routing table.
  
== Usage

  has_route "10.0.0.1" do
    gateway "10.0.0.20"
    metric 5
    route_type :net
    netmask "255.255.0.0"
  end
  
== Options
* <tt>gateway</tt> The gateway for the route
* <tt>metric</tt> The route metric as an integer
* <tt>route_type</tt> The type of route as a symbol, either :host or :net
* <tt>netmask</tt> The decimal representation of the network mask eg 255.255.255.0
* <tt>device</tt> Network interface to apply this route to
  
=end
module PoolParty
  module Resources
    
    class Route < Directory
      
      default_options(
        :gateway    => nil,
        :metric     => nil,
        :route_type => :host,
        :netmask    => nil,
        :device     => nil
      )
      
      def print_to_chef
        str = <<-EOE
route "<%= name %>" do
  gateway <%= print_variable(gateway) %>
  metric <%= print_variable(metric) %>
  route_type <%= print_variable(route_type) %>
  netmask <%= print_variable(netmask) %>
  action :<%= exists? ? :add : :delete %>
EOE
        str << "  device <%= print_variable(device) %>\n" if device
        str << "end"
      end
      
    end
    
  end
end