module PoolParty
  class Base
    plugin :heartbeat do
      
      has_line_in_file "[ca]\n\tautosign = true", "/etc/puppet/puppetd.conf"
      has_line_in_file "firstpuppetmasterhost       172.16.32.40/21/eth0:0", "/etc/ha.d/haresources"

      custom_file "/etc/mon/mon.cf", <<-EOE
      hostgroup puppetmaster  localhost

      watch puppetmaster
              service puppetmasterd
                      interval 30s
                      monitor puppet.monitor
                      period wd {Mon-Sun}
                              alert stop-heartbeat.alert
      EOE

      custom_file "/usr/lib/mon/alert.d/stop-heartbeat.alert", <<-EOE
      /usr/lib/heartbeat/hb_standby
      EOE

      custom_file "/usr/lib/mon/mon.d/puppet.monitor", <<-EOE
      #!/bin/bash
      # Basic watch script
      exitstatus=0
      processid=`ps -u puppet|grep puppetmasterd|awk '{print $1}'`

      if [ "$processid" == "" ] || test `netstat -apn|grep "${processid}/ruby"|wc -l` -eq 0  ; then
              exitstatus=1
      fi

      exit $exitstatus
      EOE
    end  
  end
end