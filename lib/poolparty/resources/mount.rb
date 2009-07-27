module PoolParty
  module Resources
    
    class Mount < Resource
      
      default_options(
        :action       => :mount,
        :device       => nil,
        :device_type  => :device,
        :fstype       => nil,
        :options      => "rw",
        :dump         => 0,
        :pass         => 2
      )
      
      def print_to_chef
        str = <<-EOE
mount "<%= name %>" do
  action :<%= (action ? action : (exists ? :mount : :unmount)) %>
  device_type <%= print_variable(device_type) %>
EOE
        str << "  device <%= print_variable(device) %>\n" if device
        str << "  fstype <%= print_variable(fstype) %>\n" if fstype
        str << "  options <%= print_variable(options) %>\n" if options
        str << "  dump <%= print_variable(dump) %>\n" if dump
        str << "  pass <%= print_variable(pass) %>\n" if pass
        str << "end"
      end
      
    end
    
  end
end