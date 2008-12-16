module PoolParty
  module Monitors
    
    class CpuMonitor < BaseMonitor
      
      def run
        stdout = %x[uptime]
        stdout.split(/\s+/)[-1].to_f rescue 0.0
      end
            
    end
    
    register_monitor :cpu
  end
end