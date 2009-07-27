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