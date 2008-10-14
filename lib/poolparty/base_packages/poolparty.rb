module PoolParty
  class Base
    plugin :poolparty do
      
      def enable        
        has_package(:name => "erlang")
        # These should be installed automagically by poolparty, but just in case
        has_gempackage(:name => "activesupport")
        has_gempackage(:name => "logging")
        has_gempackage(:name => "hoe")
        has_gempackage(:name => "xml-simple")
        has_gempackage(:name => "ParseTree", :version => "2.2.0", :requires => gempackage(:name => "hoe"))
        
        has_gempackage(:name => "RubyInline", :requires => gempackage(:name => "ParseTree"))
        has_gempackage(:name => "open4", :requires => gempackage(:name => "hoe"))
        has_gempackage(:name => "ruby2ruby", :requires => gempackage(:name => "ParseTree"))

        has_gempackage(:name => "grempe-amazon-ec2", :source => "http://gems.github.com", :requires => gempackage(:name => "xml-simple"))
        has_gempackage(:name => "auser-poolparty", :source => "http://gems.github.com", :requires => [gempackage(:name => "activesupport"), gempackage(:name => "ParseTree")])
        
        # Build hostsfile
        # TODO: COME BACK AND CLEAN THIS UP
        (self.respond_to?(:list_of_running_instances) ? self : parent).list_of_running_instances.each do |ri|
          has_host({:name => "#{ri.name}", :ip => ri.ip })
        end
        
        has_cron({:command => "cloud-maintain"}) do
          minute "*/5"
        end
        # has_host(:name => "puppet", :ip => (self.respond_to?(:master) ? self : parent).master.ip)
      end
      
    end  
  end
end