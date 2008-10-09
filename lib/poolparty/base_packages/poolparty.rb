module PoolParty
  class Base
    plugin :poolparty do
      
      def enable
        has_package(:name => "erlang")
        has_gem(:name => "open4")
        has_gem(:name => "activesupport")
        has_gem_package(:name => "auser-poolparty", :source => "http://gems.github.com")
                
        # Build hostsfile
        # TODO: COME BACK AND CLEAN THIS UP
        (self.respond_to?(:remote_instances_list) ? self : parent).remote_instances_list.each do |ri|
          has_host({:name => "#{ri.name}", :ip => ri.ip })
        end
      end
      
    end  
  end
end