=begin rdoc
  Enables haproxy for a cloud
=end
module PoolParty
  class Base
    plugin :haproxy do
      
      default_options(
        :port => 80,
        :forwarding_port => 8080,
        :proxy_mode => "http"
      )
      
      def before_configure
      end
      
      def loaded(o={}, &block)
        set_vars_from_options(cloud.dsl_options.reject{|k,v| [:enabled, :disabled].include?(v) }) if cloud
        # Restart sysklogd after we update the haproxy.log
        has_service(:name => "sysklogd")    

        has_package "haproxy"

        # Template variables
        has_variable("haproxy_name", :value => "#{(cloud ? cloud.name : name)}")
        has_variable("listen_ports", :value => [ "8080" ], :namespace => "apache")
        
        has_variable("ports_haproxy", :value => ([(port || Default.port)].flatten))        
        has_variable("forwarding_port", :value => (forwarding_port || Default.forwarding_port))
        has_variable("proxy_mode", :value => (proxy_mode || Default.proxy_mode))
    
        # Startup haproxy and enable it
        has_line_in_file(:line => "ENABLED=1", :file => "/etc/default/haproxy")
        has_line_in_file({:line => "SYSLOGD=\"-r\"", :file => "/etc/default/syslogd"})
        has_line_in_file({:line => "local0.* /var/log/haproxy.log", :file => "/etc/syslog.conf"}, {:notify => get_service("sysklogd")})
        has_file '/var/log/haproxy.log' do
          content ''
        end
        
        has_directory "/var/run/haproxy"        

        has_exec "reloadhaproxy", 
          :command => "/etc/init.d/haproxy reload", 
          :ensures => "nothing",
          :requires => get_package("haproxy")
        
        has_file "/etc/haproxy/haproxy.cfg" do
          template "#{::File.dirname(__FILE__)}/../templates/haproxy.conf"
          calls get_exec("reloadhaproxy")
        end
        
        # Service is required
        has_service("haproxy") do
          action [:start, :enable]
        end
        
      end
    end
  end
end