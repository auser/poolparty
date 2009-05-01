module PoolParty
  class Base
    plugin :poolparty_base_heartbeat do
      
      def enable        

        has_package({:name => "heartbeat-2"})
        has_package({:name => "heartbeat-2-dev"})
        
        has_service("heartbeat")
          
      end

    end  
  end
end