module PoolParty
  class Base
    plugin :poolparty do
      
      def enable                
        # Build hostsfile
        # TODO: COME BACK AND CLEAN THIS UP
        (self.respond_to?(:list_of_running_instances) ? self : parent).list_of_running_instances.each do |ri|
          has_host({:name => "gen_#{ri.name}", :ip => ri.ip, :alias => "#{ri.name}" })
        end
        has_host({:alias => "$hostname", :name => "$hostname", :ip => "localhost"})
        
        has_package(:name => "erlang")
        has_package(:name => "erlang-dev")
        has_package(:name => "erlang-src")
        # has_package(:name => "yaws")
        
        has_package(:name => "rubygems") do
          # These should be installed automagically by poolparty, but just in case
          # TODO: Fix the requires method with a helper
          has_gempackage(:name => "flexmock", :download_url => "http://rubyforge.org/frs/download.php/42580/flexmock-0.8.3.gem")
          has_gempackage(:name => "lockfile", :download_url => "http://rubyforge.org/frs/download.php/18698/lockfile-1.4.3.gem")
          has_gempackage(:name => "logging", :download_url => "http://rubyforge.org/frs/download.php/44731/logging-0.9.4.gem", :requires => [get_gempackage("flexmock"), get_gempackage("lockfile")])          
          
          has_gempackage(:name => "rubyforge", :download_url => "http://rubyforge.org/frs/download.php/45546/rubyforge-1.0.1.gem")
          has_gempackage(:name => "hoe", :download_url => "http://rubyforge.org/frs/download.php/45685/hoe-1.8.2.gem", :version => "1.8", :requires => get_gempackage("rubyforge"))
          has_gempackage(:name => "ZenTest", :download_url => "http://rubyforge.org/frs/download.php/45581/ZenTest-3.11.0.gem", :requires => [get_gempackage("hoe"), get_gempackage("rubyforge")])
          
          has_gempackage(:name => "rake", :download_url => "http://rubyforge.org/frs/download.php/43954/rake-0.8.3.gem")
          has_gempackage(:name => "xml-simple", :download_url => "http://rubyforge.org/frs/download.php/18366/xml-simple-1.0.11.gem")
          has_gempackage(:name => "amazon-ec2", :download_url => "http://rubyforge.org/frs/download.php/43666/amazon-ec2-0.3.1.gem", :requires => get_gempackage("xml-simple"))
          
          has_gempackage(:name => "sexp_processor", :download_url => "http://rubyforge.org/frs/download.php/45589/sexp_processor-3.0.0.gem")
          has_gempackage(:name => "ParseTree", :download_url => "http://rubyforge.org/frs/download.php/45600/ParseTree-3.0.1.gem", :requires => [get_gempackage("sexp_processor"), get_gempackage("ZenTest")])
            
          has_gempackage(:name => "ruby2ruby", :download_url => "http://rubyforge.org/frs/download.php/45587/ruby2ruby-1.2.0.gem", :requires => get_gempackage("ParseTree"))
          
          has_gempackage(:name => "activesupport", :download_url => "http://rubyforge.org/frs/download.php/45627/activesupport-2.1.2.gem")
 
          has_gempackage(:name => "RubyInline", :download_url => "http://rubyforge.org/frs/download.php/45683/RubyInline-3.8.1.gem")
          
          has_gempackage(:name => "poolparty", :download_url => "http://github.com/auser/poolparty/tree/master%2Fpkg%2Fpoolparty.gem?raw=true", :requires => [get_gempackage("ruby2ruby"), get_gempackage("RubyInline"), get_gempackage("ParseTree")])
          
          # , :ifnot => "/bin/ps aux | /bin/grep -q pm_node"
          has_exec(:name => "build_messenger", :command => ". /etc/profile && server-build-messenger -v")
          has_exec(:name => "start_node", :command => ". /etc/profile && server-start-node")
          # has_runit_service("pm_node", "pm_node", File.join(File.dirname(__FILE__), "..", "templates/messenger/node/"))
        end
        
        # execute_on_node do
        has_cron(:name => "puppetd runner", :user => Base.user, :minute => "*/5") do
          requires get_gempackage("poolparty")
          command(PoolParty::Remote::RemoteInstance.puppet_rerun_commad)
        end
        # end
        
        # Cloud panel setup
        
        # has_directory(:name => "/var/www/cloudpanel")
        
        # has_file(:name => "/etc/yaws/conf.d/localhost.conf") do
        #   template File.join(File.dirname(__FILE__), "..", "templates/yaws.conf")
        # end
        
        # Custom run puppet to minimize footprint
        # TODO: Update the offsetted times
        execute_on_master do
          has_cron(:name => "puppetd runner", :user => Base.user, :minute => "*/5") do
            requires get_gempackage("poolparty")
            command(PoolParty::Remote::RemoteInstance.puppet_master_rerun_command)
          end
          has_cron(:name => "Load handler", :user => Base.user, :minute => "*/4") do
            requires get_gempackage("poolparty")
            command(". /etc/profile && cloud-handle-load")
          end          
          has_cron(:name => "provisioning ensurer", :user => Base.user, :minute => "*/2") do
            requires get_gempackage("poolparty")
            command ". /etc/profile && cloud-ensure-provisioning"
          end
          # has_runit_service("client_server", "pm_client", File.join(File.dirname(__FILE__), "..", "templates/messenger/client/"))
          # has_runit_service("master_server", "pm_master", File.join(File.dirname(__FILE__), "..", "templates/messenger/master/"))
          # TODO: Update this so it only runs when needed          
          # has_customservice(:name => "start master server", :pattern => "/pm_master/", :bin => ". /etc/profile && server-start-master")
          # has_customservice(:name => "start client server", :pattern => "/client_service/", :bin => ". /etc/profile && server-start-client")                    
          has_exec(:name => "start master messenger", :command => ". /etc/profile && server-start-master") #, :ifnot => "/bin/ps aux | /bin/grep -q pm_master"
          has_exec(:name => "start client server", :command => ". /etc/profile && server-start-client") #, :ifnot => "/bin/ps aux | /bin/grep -q client_server"
          
          has_cron({:name => "maintain script", :command => ". /etc/profile && which cloud-maintain | /bin/sh", :minute => "*/3", :requires => [get_gempackage("poolparty"), get_cron("puppetd runner"), get_cron("Load handler"), get_service("haproxy")]})
          
          has_remotefile(:name => "/usr/bin/puppetcleaner") do
            mode 744
            template File.join(File.dirname(__FILE__), "..", "templates/puppetcleaner")
          end
        end        
        
        has_remotefile(:name => "/usr/bin/puppetrerun") do
          mode 744
          template File.join(File.dirname(__FILE__), "..", "templates/puppetrerun")
        end
        
        # has_host(:name => "puppet", :ip => (self.respond_to?(:master) ? self : parent).master.ip)
      end
      
    end  
  end
end