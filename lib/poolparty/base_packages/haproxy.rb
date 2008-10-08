module PoolParty
  class Base
    plugin :haproxy do
      
      def enable
        package({:name => "haproxy"})
        
        # Startup haproxy and enable it
        has_line_in_file("ENABLED=1", "/etc/default/haproxy")
        has_line_in_file("SYSLOGD=\"-r\"", "/etc/default/syslogd")
        has_line_in_file("local0.* /var/log/haproxy.log", "/etc/syslog.conf", {:notify => 'Service["sysklogd"]'})
        
        # Restart sysklogd after we update the haproxy.log
        has_service(:name => "sysklogd") do
          ensures "running"
        end
        
        # Service is required
        has_service(:name => "haproxy")
        
        # Tempalte variables
        variable(:name => "name", :value => "#{name}")
        has_variable({:name => "nodenames", :value => list_of_node_names})
        has_variable({:name => "node_ips",  :value => list_of_node_ips})
        has_variable({:name => "port", :value => (port || Base.port)})
        has_variable(:name => "forwarding_port", :value => (forwarding_port || Base.forwarding_port))
        
        # These can also be passed in via hash
        has_remotefile(:name => "/etc/haproxy.cfg") do
          mode 644
          requires 'Package["haproxy"]'
          notify 'Service["haproxy"]'
          template File.join(File.dirname(__FILE__), "..", "templates/haproxy.conf")
        end
        
      end
      
    end  
  end
end