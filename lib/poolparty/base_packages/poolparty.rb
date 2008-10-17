module PoolParty
  class Base
    plugin :poolparty do
      
      def enable        
        has_package(:name => "erlang")
        has_package(:name => "rubygems")
        # These should be installed automagically by poolparty, but just in case
        # TODO: Fix the requires method with a helper
        has_gempackage(:name => "activesupport", :requires => 'Package["rubygems"]')
        has_gempackage(:name => "logging", :requires => 'Package["rubygems"]')
        has_gempackage(:name => "hoe", :requires => 'Package["rubygems"]')
        has_gempackage(:name => "xml-simple", :requires => 'Package["rubygems"]')
        has_gempackage(:name => "ParseTree", :version => "2.2.0", :requires => 'Exec["gem-package-hoe"]')
        
        has_gempackage(:name => "RubyInline", :requires => 'Exec["gem-package-ParseTree"]')
        has_gempackage(:name => "open4", :requires => 'Exec["gem-package-hoe"]')
        has_gempackage(:name => "ruby2ruby", :requires => 'Exec["gem-package-ParseTree"]')

        has_gempackage(:name => "grempe-amazon-ec2", :source => "http://gems.github.com", :requires => 'Exec["gem-package-xml-simple"]')
        has_gempackage(:name => "auser-poolparty", :source => "http://gems.github.com", :requires => ['Exec["gem-package-activesupport"]', 'Exec["gem-package-ParseTree"]'])
        
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
        end
        # has_host(:name => "puppet", :ip => (self.respond_to?(:master) ? self : parent).master.ip)
      end
      
    end  
  end
end