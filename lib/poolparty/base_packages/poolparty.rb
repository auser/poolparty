module PoolParty
  class Base
    plugin :poolparty do
      
      def enable
        has_package(:name => "rubygems")
        has_package(:name => "erlang")
        has_gem(:name => "poolparty")
                
        # Build hostsfile
        (self.respond_to?(:instances_list) ? self : parent).instances_list.each do |ri|
          has_host({:name => ri.name, :ip => ri.ip})
        end
        
        # Let's make sure that the puppet and puppetmaster are always running
        has_cron(:name => "restart-puppetmaster") do
          command 'if [ -e /var/run/puppetmasterd.pid ]; then ps uw -p `cat /var/run/puppetmasterd.pid` | grep -q "ruby /usr/sbin/puppetmasterd" || (rm /var/run/puppetmasterd.pid; /etc/init.d/puppetmaster start); else /etc/init.d/puppetmaster start; fi'
          user "root"
          minute 0
        end
        
        has_cron(:name => "restart-puppet") do
          command 'if [ -e /var/run/puppetd.pid ]; then ps uw -p `cat /var/run/puppetd.pid` | grep -q "ruby /usr/sbin/puppetd" || (rm /var/run/puppetd.pid; /etc/init.d/puppet start); else /etc/init.d/puppet start; fi'
          user "root"
          minute 0
        end
        
        has_service(:name => "cron") do
          ensures "running"
          enable "true"
        end
      end
      
    end  
  end
end