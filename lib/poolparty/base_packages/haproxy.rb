module PoolParty
  class Base
    plugin :haproxy do
      
      def enable
        package({:name => "haproxy"})
        
        # Startup haproxy and enable it
        has_line_in_file("s/ENABLED=0/ENABLED=1/g", "/etc/default/haproxy")
        has_line_in_file("s/SYSLOGD=\"\"/SYSLOGD=\"-r\"/g", "/etc/default/syslogd")
        has_line_in_file("local0.* /var/log/haproxy.log", "/etc/syslog.conf", 'present', 'Service["syslogd"]')
        
        # Restart sysklogd after we update the haproxy.log
        has_service(:name => "sysklogd")
      end
      
    end  
  end
end