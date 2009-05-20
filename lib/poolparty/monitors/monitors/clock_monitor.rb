# An example monitor
# access it at /monitor_clock/ or /monitor_clock/now
module Monitors
  
  class Clock < BaseMonitor
    def get(data=nil)
      Time.now
    end
    
    def get_now
      Time.now
    end
  end
  
end
