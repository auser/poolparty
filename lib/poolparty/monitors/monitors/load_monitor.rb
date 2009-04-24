module Monitors
  
  class Load < BaseMonitor
    def get(data=nil)
      %x{"uptime"}.split[-3].to_f
    end
  end
  
end
