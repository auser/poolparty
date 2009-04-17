module PoolParty
  class Base
    plugin :haproxy do
      
      def before_configure
        # We need the haproxy package
        ::Suitcase::Zipper.add("#{::File.dirname(__FILE__)}/../../../vendor/chef/apache2", "chef/cookbooks")
      end
      
      def enable
        has_chef_recipe 'apache2'
        has_service "apache2"
        
        has_package({:name => "haproxy"}) do
          stops get_service("apache2")
        end
    
        # Restart sysklogd after we update the haproxy.log
        has_service(:name => "sysklogd")    
        # Template variables
        has_variable("haproxy_name", :value => "#{name}")
        has_variable("listen_ports", :value => [ "8080" ], :namespace => "apache")
        
        has_variable("ports_haproxy", :value => ([(self.respond_to?(:port) ? port : Default.port)].flatten))        
        has_variable("forwarding_port", :value => (respond_to?(:forwarding_port) ? forwarding_port : Default.forwarding_port))
        has_variable("proxy_mode", :value => (respond_to?(:proxy_mode) ? proxy_mode : Default.proxy_mode))
    
        # Startup haproxy and enable it
        has_line_in_file(:line => "ENABLED=1", :file => "/etc/default/haproxy")
        has_line_in_file({:line => "SYSLOGD=\"-r\"", :file => "/etc/default/syslogd"})
        has_line_in_file({:line => "local0.* /var/log/haproxy.log", :file => "/etc/syslog.conf"}, {:notify => get_service("sysklogd")})

        has_exec "reloadhaproxy", 
          :command => "/etc/init.d/haproxy reload", 
          :requires => get_package("haproxy")
        
        # Service is required
        has_service("haproxy", :ensures => "running") do
          stops get_service("apache2"), :immediately
          starts get_service("apache2")
        end
        
        has_directory "/var/run/haproxy"
        
        has_file "/etc/haproxy/haproxy.cfg" do
          template "#{::File.dirname(__FILE__)}/../templates/haproxy.conf"
          calls get_exec("reloadhaproxy")
        end
        
      end
    end
  end
end