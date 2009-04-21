module Monitors
  
  class Load
    def self.get
      %x{"uptime"}.split[-3].to_f
    end
  end
  
end
