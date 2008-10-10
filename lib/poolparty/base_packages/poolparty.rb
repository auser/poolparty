module PoolParty
  class Base
    plugin :poolparty do
      
      def enable
        has_package(:name => "erlang")
        # These should be installed automagically by poolparty, but just in case
        has_gem(:name => "open4")
        has_gem(:name => "activesupport")
        has_gem(:name => "logging")
        has_gempackage(:name => "grempe-amazon-ec2", :source => "http://gems.github.com")
        has_gempackage(:name => "auser-poolparty", :source => "http://gems.github.com")
                
        # Build hostsfile
        # TODO: COME BACK AND CLEAN THIS UP
        (self.respond_to?(:list_of_running_instances) ? self : parent).list_of_running_instances.each do |ri|
          has_host({:name => "#{ri.name}", :ip => ri.ip })
        end
      end
      
    end  
  end
end