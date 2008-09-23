module PoolParty
  class Base
    plugin :haproxy do
      
      package({:name => "haproxy"})
      
    end  
  end
end