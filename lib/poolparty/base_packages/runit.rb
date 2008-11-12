=begin rdoc
  Runit beginning
  NOT IMPLEMENTED YET
=end
module PoolParty
  class Base
    plugin :runit do
      
      def enable
        unless enabled
          has_file(:name => "inittab", :path => "/etc/inittab", :mode => 0644, :owner => "root", :group => "root")
          has_package(:name => "runit", :ensures => "latest", :requires => get_file("inittab"))
          has_exec(:name => "/sbin/start runsvdir", :cwd => "/var/service")
          # has_remotefile(:name => "/etc/event.d/runsvdir", :notify => get_exec("/sbin/start runsvdir"))
          enabled true
        end          
      end
      
    end
  end
end