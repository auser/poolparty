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
          g.has_gempackage(:name => "flexmock", :download_url => "http://rubyforge.org/frs/download.php/42580/flexmock-0.8.3.gem")
          g.has_gempackage(:name => "lockfile", :download_url => "http://rubyforge.org/frs/download.php/18698/lockfile-1.4.3.gem")
          g.has_gempackage(:name => "logging", :download_url => "http://rubyforge.org/frs/download.php/44731/logging-0.9.4.gem", :requires => [get_gempackage("flexmock"), get_gempackage("lockfile")])          
          
          g.has_gempackage(:name => "rubyforge", :download_url => "http://rubyforge.org/frs/download.php/45546/rubyforge-1.0.1.gem")
          g.has_gempackage(:name => "hoe", :download_url => "http://rubyforge.org/frs/download.php/45685/hoe-1.8.2.gem", :version => "1.8", :requires => get_gempackage("rubyforge"))
          g.has_gempackage(:name => "ZenTest", :download_url => "http://rubyforge.org/frs/download.php/45581/ZenTest-3.11.0.gem", :requires => [get_gempackage("hoe"), get_gempackage("rubyforge")])
          
          g.has_gempackage(:name => "rake", :download_url => "http://rubyforge.org/frs/download.php/43954/rake-0.8.3.gem")
          g.has_gempackage(:name => "xml-simple", :download_url => "http://rubyforge.org/frs/download.php/18366/xml-simple-1.0.11.gem") do |x|
            x.has_gempackage(:name => "grempe-amazon-ec2", :download_url => "http://rubyforge.org/frs/download.php/43666/amazon-ec2-0.3.1.gem")
          end
          
          has_gempackage(:name => "sexp_processor", :download_url => "http://rubyforge.org/frs/download.php/45589/sexp_processor-3.0.0.gem")
          has_gempackage(:name => "ParseTree", :download_url => "http://rubyforge.org/frs/download.php/45600/ParseTree-3.0.1.gem", :requires => [get_gempackage("sexp_processor"), get_gempackage("ZenTest")]) do |pt|
            pt.has_gempackage(:name => "ruby2ruby", :download_url => "http://rubyforge.org/frs/download.php/45587/ruby2ruby-1.2.0.gem", :requires => [get_gempackage("ParseTree")])
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
          has_exec(:name => "start master messenger", :command => ". /etc/profile && server-start-master", :requires => [get_gempackage("poolparty"), get_exec("build_messenger")])
          
          has_remotefile(:name => "/usr/bin/puppetcleaner") do
            mode 744
            template File.join(File.dirname(__FILE__), "..", "templates/puppetcleaner")
          end
        end
        
        # has_host(:name => "puppet", :ip => (self.respond_to?(:master) ? self : parent).master.ip)
      end
      
    end  
  end
end