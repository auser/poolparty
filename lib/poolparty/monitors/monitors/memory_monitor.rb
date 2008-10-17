module PoolParty
  module Monitors
    
    class MemoryMonitor < BaseMonitor
      
      def run
        str = %x[free -m | grep -i mem]
        begin
          total_memory = str.split[1].to_f
          used_memory = str.split[2].to_f

          used_memory / total_memory
        rescue Exception => e
          0.0
        end
        
      end
            
    end
    
    register_monitor :memory
  end
end