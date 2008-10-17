module PoolParty
  module Monitors
    
    class CpuMonitor < BaseMonitor
      
      def run
        str = %x[uptime]
        str.split(/\s+/)[-3].to_f rescue 0.0
      end
            
    end
    
    register_monitor :cpu
  end
end