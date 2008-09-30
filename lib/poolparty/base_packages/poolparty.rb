module PoolParty
  class Base
    plugin :poolparty do
      
      def enable
        has_gem(:name => "poolparty")
        
        self.parent.instances_list.each do |ri|
          has_host({:name => ri.name, :ip => ri.ip})# unless ri.class != PoolParty::Remote::RemoteInstance
        end
      end
      
    end  
  end
end