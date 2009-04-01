module PoolParty
  class Base
    plugin :poolparty_base_haproxy do
      
      def enable
        case_of "hostname" do
          when_is "master" do
            has_package({:name => "haproxy"})
          
            # Restart sysklogd after we update the haproxy.log
            has_service(:name => "sysklogd")
          
            # Template variables
            has_variable("name_haproxy", :value => "#{cloud.name}")
            has_variable("nodenames_haproxy", :value => "generate('/usr/bin/env', '/usr/bin/server-list-active', 'name')")
            has_variable("node_ips_haproxy",  :value => "generate('/usr/bin/env', '/usr/bin/server-list-active', 'ip')")
            # 
            has_variable("ports_haproxy", :value => ([(self.respond_to?(:port) ? port : Default.port)].flatten))        
            has_variable("forwarding_port", :value => (respond_to?(:forwarding_port) ? forwarding_port : Default.forwarding_port))
            has_variable("proxy_mode", :value => (respond_to?(:proxy_mode) ? proxy_mode : Default.proxy_mode))
          
            # Startup haproxy and enable it
            has_line_in_file(:line => "ENABLED=1", :file => "/etc/default/haproxy")
            has_line_in_file({:line => "SYSLOGD=\"-r\"", :file => "/etc/default/syslogd"})
            has_line_in_file({:line => "local0.* /var/log/haproxy.log", :file => "/etc/syslog.conf"}, {:notify => get_service("sysklogd")})
                    
            has_exec(:name => "reloadhaproxy", 
              :command => "/etc/init.d/haproxy reload", 
              :requires => get_package("haproxy"))
            # Service is required
            has_service("haproxy", :ensures => "running", :hasrestart => true)
            has_service(:name => "haproxy", :ensures => "running", :hasrestart => true, :notify => get_exec("reloadhaproxy"))
          
            # These can also be passed in via hash
            # has_remotefile(:name => "/etc/haproxy.cfg") do
            #   mode 644
            #   requires get_package("haproxy")
            #   notify get_service("haproxy")            
            #   template "haproxy.conf"
            # end
          end
        end
      end
    end  
  end
end