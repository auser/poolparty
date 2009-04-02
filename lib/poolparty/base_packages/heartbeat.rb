module PoolParty
  class Base
    plugin :poolparty_base_heartbeat do
      
      def enable        
        # execute_on_master do
        has_package({:name => "heartbeat-2"})
        has_package({:name => "heartbeat-2-dev"})
        
        has_service("heartbeat")
      
        # has_service(:name => "heartbeat", :hasstatus => true, :hasrestart => true)
        # has_directory(:name => "/etc/ha.d")
        # 
        # has_remotefile(:name => "/etc/ha.d/ha.cf") do
        #   mode 444
        #   notify service(:name => "heartbeat")
        #   template "ha.cf"
        # end
        # 
        # has_remotefile(:name => "/etc/ha.d/logd.cf") do
        #   mode 600
        #   notify service(:name => "heartbeat")
        #   template File.join(File.dirname(__FILE__), "..", "templates/logd.cf")
        # end
        # 
        # has_exec(:name => "heartbeat-update-cib", :command => "/usr/sbin/cibadmin -R -x /etc/ha.d/cib.xml", :refreshonly => true)
        #     
        # has_remotefile(:name => "/etc/ha.d/authkeys") do
        #   mode 600
        #   notify service(:name => "heartbeat")
        #   template File.join(File.dirname(__FILE__), "..", "templates/authkeys")
        # end
        #     
        # has_remotefile(:name => "/etc/ha.d/cib.xml") do
        #   mode 444
        #   notify exec(:name => "heartbeat-update-cib")
        #   template File.join(File.dirname(__FILE__), "..", "templates/cib.xml")
        # end
        # 
        # has_remotefile(:name => "/etc/ha.d/haresources") do
        #   mode 644
        #   template File.join(File.dirname(__FILE__), "..", "templates/haresources")
        # end
        # # end
        # 
        # # variables for the templates
        # has_variable(:name => "ha_nodenames", :value => "generate('/usr/bin/env', '/usr/bin/server-list-active', '-c', 'name', '-n', '#{cloud.name}')")
        # has_variable(:name => "ha_node_ips",  :value => "generate('/usr/bin/env', '/usr/bin/server-list-active', '-c', 'ip', '-n', '#{cloud.name}')")
        # 
        # has_variable({:name => "ha_timeout",  :value => (self.respond_to?(:timeout) ? timeout : "5s")})
        # has_variable({:name => "ha_port", :value => (self.respond_to?(:port) ? port : Default.port)})
          
      end

    end  
  end
end