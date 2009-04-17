module Butterfly
  class ServerMonitor
    def self.load
      %x{"uptime"}.split[-3].to_f
    end
  end
end