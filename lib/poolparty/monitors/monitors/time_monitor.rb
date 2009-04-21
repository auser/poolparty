# An example monitor
# access it at /monitor_time/ or /monitor_time/now
module Monitors
  
  class Time
    def self.get
      Time.now
    end
    
    def self.get_now
      Time.now
    end
  end
  
end
