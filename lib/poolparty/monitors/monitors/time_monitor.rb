# An example monitor
# access it at /monitor_time/ or /monitor_time/now
module Monitors
  
  class Time < BaseMonitor
    def get(data=nil)
      Time.now
    end
    
    def get_now
      Time.now
    end
  end
  
end
