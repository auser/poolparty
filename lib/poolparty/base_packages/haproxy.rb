module PoolParty
  class Base
    plugin :haproxy do
      
      def enable
        
        package({:name => "haproxy"})
      end      
      
    end  
  end
end