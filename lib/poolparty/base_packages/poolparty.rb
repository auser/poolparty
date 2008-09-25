module PoolParty
  class Base
    plugin :poolparty do
      
      def enable
        has_gem(:name => "poolparty")
      end
      
    end  
  end
end