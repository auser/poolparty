module PoolParty
  class Base
    plugin :poolparty do
      
      def enable
        has_package(:name => "erlang")
        has_gem(:name => "poolparty")
                
        # Build hostsfile
        # TODO: COME BACK AND CLEAN THIS UP
        (self.respond_to?(:remote_instances_list) ? self : parent).remote_instances_list.each do |ri|
          has_host({:name => "#{ri.name}", :ip => ri.ip })
        end
        
        # Let's make sure that the puppet and puppetmaster are always running
        has_cron(:name => "restart-puppetmaster") do
          command 'if [ -e /var/run/puppetmasterd.pid ]; then ps uw -p `cat /var/run/puppetmasterd.pid` | grep -q "ruby /usr/sbin/puppetmasterd" || (rm /var/run/puppetmasterd.pid; /usr/sbin/puppetmaster start); else /usr/sbin/puppetmaster start; fi'
          user "root"
          minute 0
        end
        
        has_cron(:name => "restart-puppet") do
          command 'if [ -e /var/run/puppetd.pid ]; then ps uw -p `cat /var/run/puppetd.pid` | grep -q "ruby /usr/sbin/puppetd" || (rm /var/run/puppetd.pid; /usr/sbin/puppet start); else /usr/sbin/puppetd start; fi'
          user "root"
          minute 0
        end
        
        has_service(:name => "cron") do
          ensures "running"
        end
      end
      
    end  
  end
end