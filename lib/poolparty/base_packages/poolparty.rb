module PoolParty
  class Base
    plugin :poolparty do
      
      def enable
        has_gem(:name => "poolparty")
        
        self.parent.list_from_remote.each do |ri|
          has_host({:name => ri.name, :ip => ri.ip})# unless ri.class != PoolParty::Remote::RemoteInstance
        end
      end
      
    end  
  end
end