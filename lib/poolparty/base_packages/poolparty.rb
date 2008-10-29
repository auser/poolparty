module PoolParty
  class Base
    plugin :poolparty do
      
      def enable                
        has_package(:name => "erlang")
        has_package(:name => "erlang-dev")
        has_package(:name => "erlang-src")
        
        has_package(:name => "rubygems") do |g|
          # These should be installed automagically by poolparty, but just in case
          # TODO: Fix the requires method with a helper          
          g.has_gempackage(:name => "logging", :download_url => "http://rubyforge.org/frs/download.php/44731/logging-0.9.4.gem")
          g.has_gempackage(:name => "hoe", :download_url => "http://rubyforge.org/frs/download.php/45685/hoe-1.8.2.gem")
          g.has_gempackage(:name => "xml-simple") do |x|
            x.has_gempackage(:name => "grempe-amazon-ec2", :source => "http://gems.github.com")
          end
          
          has_gempackage(:name => "ParseTree", :download_url => "http://rubyforge.org/frs/download.php/45600/ParseTree-3.0.1.gem") do |pt|
            pt.has_gempackage(:name => "ruby2ruby", :download_url => "http://rubyforge.org/frs/download.php/45587/ruby2ruby-1.2.0.gem")
            pt.has_gempackage(:name => "activesupport", :download_url => "http://rubyforge.org/frs/download.php/45627/activesupport-2.1.2.gem") do |a|
              a.has_gempackage(:name => "poolparty", :download_url => "http://github.com/auser/poolparty/tree/master%2Fpkg%2Fpoolparty-latest.gem?raw=true")
            end
            has_gempackage(:name => "RubyInline", :download_url => "http://rubyforge.org/frs/download.php/45683/RubyInline-3.8.1.gem")
            
            has_exec(:name => "build_messenger", :command => ". /etc/profile && server-build-messenger", :requires => get_gempackage("poolparty"))
            has_exec(:name => "start_node", :command => ". /etc/profile && server-start-node", :requires => get_exec("build_messenger"))
          end          
          
        end
                
        # Build hostsfile
        # TODO: COME BACK AND CLEAN THIS UP
        (self.respond_to?(:list_of_running_instances) ? self : parent).list_of_running_instances.each do |ri|
          has_host({:name => "#{ri.name}", :ip => ri.ip })
        end
        
        # Custom run puppet to minimize footprint
        # TODO: Update the offsetted times
        has_cron(:name => "puppetd runner", :user => Base.user, :minute => "*/8") do
          command((self.respond_to?(:master) ? self : parent).master.puppet_runner_command)
        end
        
        # These are all requirements on the master
        execute_if("$hostname", "master") do
          has_cron({:name => "maintain script ", :command => ". /etc/profile && which cloud-maintain | /bin/sh", :minute => "*/3"})
          # TODO: Update this so it only runs when needed
          has_exec(:name => "start master messenger", :command => ". /etc/profile && server-start-master", :requires => get_gempackage("poolparty"))
        end
        
        # has_host(:name => "puppet", :ip => (self.respond_to?(:master) ? self : parent).master.ip)
      end
      
    end  
  end
end