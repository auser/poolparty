module PoolParty
  class Base
    plugin :heartbeat do
      
      def enable
        # execute_on_master do
          has_package(:name => "heartbeat") do
            has_service(:name => "heartbeat", :hasstatus => true)
            has_directory(:name => "/etc/ha.d")
            
            has_remotefile(:name => "/etc/ha.d/ha.cf") do
              mode 444
              notify service(:name => "heartbeat")
              template File.join(File.dirname(__FILE__), "..", "templates/ha.cf")
            end
            
            has_remotefile(:name => "/etc/ha.d/logd.cf") do
              mode 600
              notify service(:name => "heartbeat")
              template File.join(File.dirname(__FILE__), "..", "templates/logd.cf")
            end
            
            has_exec(:name => "heartbeat-update-cib", :command => "/usr/sbin/cibadmin -R -x /etc/ha.d/cib.xml", :refreshonly => true)
        
            has_remotefile(:name => "/etc/ha.d/authkeys") do
              mode 600
              notify service(:name => "heartbeat")
              template File.join(File.dirname(__FILE__), "..", "templates/authkeys")
            end
        
            has_remotefile(:name => "/etc/ha.d/cib.xml") do
              mode 444
              notify exec(:name => "heartbeat-update-cib")
              template File.join(File.dirname(__FILE__), "..", "templates/cib.xml")
            end
          # end
          
          # variables for the templates
          has_variable(:name => "ha_nodenames", :value => "generate('/usr/bin/env', '/usr/bin/server-list-active', '-c', 'name', '-n', '#{cloud.name}')")
          has_variable(:name => "ha_node_ips",  :value => "generate('/usr/bin/env', '/usr/bin/server-list-active', '-c', 'ip', '-n', '#{cloud.name}')")
          
          has_variable({:name => "ha_timeout",  :value => (self.respond_to?(:timeout) ? timeout : "5s")})
          has_variable({:name => "ha_port", :value => (self.respond_to?(:port) ? port : Base.port)})
          
          # Finally, let's set a few options and start it
#           has_exec(:name => "set and start") do
#             refreshonly true
#             command <<-EOC
# crm_mon -i5            
#             EOC
#           end
          
        end
        
        # execute_on_master do
        #   if list_of_node_names.size > 1
        #     has_exec(:name => "update pem for heartbeat", :refreshonly => true) do
        #       command "scp /etc/puppet/ssl/ca/ca_crl.pem #{user || Base.user}@#{list_of_node_ips[1]}:#{Base.base_config_directory}/ssl/ca"
        #     end
        #   end
        # end
        
      end

    end  
  end
end