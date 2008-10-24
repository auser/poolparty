module PoolParty
  class Base
    plugin :poolparty do
      
      def enable        
        has_package(:name => "erlang")
        has_package(:name => "erlang-dev")
        has_package(:name => "erlang-src")
        
        has_package(:name => "rubygems") do
          # These should be installed automagically by poolparty, but just in case
          # TODO: Fix the requires method with a helper          
          has_gempackage(:name => "logging")
          has_gempackage(:name => "xml-simple") do
            has_gempackage(:name => "grempe-amazon-ec2", :source => "http://gems.github.com")
          end
          has_gempackage(:name => "rake")
          
          has_gempackage(:name => "hoe") do
            has_gempackage(:name => "open4")
            
            has_gempackage(:name => "ParseTree", :version => "2.2.0") do
              has_gempackage(:name => "ruby2ruby")
              has_gempackage(:name => "activesupport") do
                has_gempackage(:name => "auser-poolparty", :source => "http://gems.github.com") do
                  has_exec(:name => "build_messenger", :command => ". /etc/profile && server-build-messenger")
                  has_exec(:name => "start_node", :command => ". /etc/profile && server-start-node", :requires => 'Exec["build_messenger"]')
                end
              end
              has_gempackage(:name => "RubyInline")
            end
          end          
          
        end
                
        # Build hostsfile
        # TODO: COME BACK AND CLEAN THIS UP
        (self.respond_to?(:list_of_running_instances) ? self : parent).list_of_running_instances.each do |ri|
          has_host({:name => "#{ri.name}", :ip => ri.ip })
        end
        
        # Custom run puppet to minimize footprint
        # TODO: Update the offsetted times
        has_cron(:name => "puppetd runner", :user => Base.user, :minute => [0,15,30,45]) do
          command((self.respond_to?(:master) ? self : parent).master.puppet_runner_command)
        end
        
        # These are all requirements on the master
        execute_if("$hostname", "master") do
          has_cron({:command => ". /etc/profile && which cloud-maintain | /bin/sh", :minute => "*/3"})
          # TODO: Update this so it only runs when needed
          has_exec(:name => ". /etc/profile && server-start-master")
        end
        # has_host(:name => "puppet", :ip => (self.respond_to?(:master) ? self : parent).master.ip)
      end
      
    end  
  end
end