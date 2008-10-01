module PoolParty
  class Base
    plugin :heartbeat do
      
      def enable
        has_package(:name => "heartbeat-2")
        has_service(:name => "heartbeat", :hasstatus => true, :ensure => "running")
        has_exec(:name => "heartbeat-update-cib") do          
          command "/usr/sbin/cibadmin -R -x /etc/ha.d/cib.xml"
          refreshonly true
        end
        
        # These can also be passed in via hash
        has_remotefile(:name => "/etc/ha.d/ha.cf") do
          mode 444
          requires 'Package["heartbeat-2"]'
          notify 'Service["heartbeat"]'
        end
        
        has_remotefile(:name => "/etc/ha.d/authkeys") do
          mode 400
          requires 'Package["heartbeat-2"]'
          notify 'Service["heartbeat"]'
        end
        
        has_remotefile(:name => "/etc/ha.d/cib.xml") do
          mode 444
          requires 'Package["heartbeat-2"]'
          notify 'Exec["heartbeat-update-cib"]'
        end                
        
      end
    end  
  end
end