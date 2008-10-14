module PoolParty
  class Base
    plugin :haproxy do
      
      def enable
        execute_if("$hostname", "master") do
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
          variable(:name => "name_haproxy", :value => "#{@parent.name}")
          variable(:name => "nodenames_haproxy", :value => list_of_node_names)
          variable(:name => "node_ips_haproxy",  :value => list_of_node_ips)
          variable(:name => "ports_haproxy", :value => ([(self.respond_to?(:port) ? port : Base.port)].flatten))        
          variable(:name => "forwarding_port", :value => (respond_to?(:forwarding_port) ? forwarding_port : Base.forwarding_port))
          variable(:name => "proxy_mode", :value => (respond_to?(:proxy_mode) ? proxy_mode : Base.proxy_mode))

          # These can also be passed in via hash
          has_remotefile(:name => "/etc/haproxy.cfg") do
            mode 644
            # onlyif '$hostname == "master"'
            requires 'Package["haproxy"]'
            notify 'Service["haproxy"]'
            template File.join(File.dirname(__FILE__), "..", "templates/haproxy.conf")
          end
        end
      end
    end  
  end
end