module PoolParty
  class Base
    plugin :poolparty do
      
      def enable        
        has_package(:name => "erlang")
        # These should be installed automagically by poolparty, but just in case
        with_options(:requires => 'Package["update-rubygems"]') do          
          has_gempackage(:name => "parsetree")          
          has_gempackage(:name => "activesupport")
          has_gempackage(:name => "logging")
          has_gempackage(:name => "hoe")
          has_gempackage(:name => "xml-simple")
          
          has_gempackage(:name => "rubyinline", :requires => 'Exec["parsetree"]')
          has_gempackage(:name => "open4", :requires => 'Exec["parsetree"]')
          has_gempackage(:name => "ruby2ruby", :requires => 'Exec["ParseTree", "hoe"]')

          has_gempackage(:name => "grempe-amazon-ec2", :source => "http://gems.github.com", :requires => 'Package["xml-simple"]')
          has_gempackage(:name => "auser-poolparty", :source => "http://gems.github.com", :requires => 'Package["activesupport", "logging", "hoe", "open4", "ruby2ruby"]')
        end                
        # Build hostsfile
        # TODO: COME BACK AND CLEAN THIS UP
        (self.respond_to?(:list_of_running_instances) ? self : parent).list_of_running_instances.each do |ri|
          has_host({:name => "#{ri.name}", :ip => ri.ip })
        end
        
        cron({:command => "cloud-maintain"}) do
          minute "*/5"
          user "puppet"
        end
        # has_host(:name => "puppet", :ip => (self.respond_to?(:master) ? self : parent).master.ip)
      end
      
    end  
  end
end