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
          g.has_gempackage(:name => "logging")
          g.has_gempackage(:name => "xml-simple", :source => "/var/poolparty/xml-simple.gem") do |x|            
            x.has_gempackage(:name => "grempe-amazon-ec2", :source => "http://gems.github.com")
          end
          
          has_gempackage(:name => "ParseTree", :version => "2.2.0") do |pt|
            pt.has_gempackage(:name => "ruby2ruby")
            pt.has_gempackage(:name => "activesupport") do |a|
              a.has_gempackage(:name => "auser-poolparty", :source => "http://gems.github.com") do |pool|
                pool.has_exec(:name => "build_messenger", :command => ". /etc/profile && server-build-messenger") do |mess|
                  mess.has_exec(:name => "start_node", :command => ". /etc/profile && server-start-node")
                end                
              end
            end
            has_gempackage(:name => "RubyInline")
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
          has_cron({:name => "maintain script ", :command => ". /etc/profile && which cloud-maintain | /bin/sh", :minute => "*/3"})
          # TODO: Update this so it only runs when needed
          has_exec(:name => ". /etc/profile && server-start-master")
        end
        # has_host(:name => "puppet", :ip => (self.respond_to?(:master) ? self : parent).master.ip)
      end
      
    end  
  end
end