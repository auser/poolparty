=begin rdoc
  Cloud monitor just is a test monitor to hold cloud info
=end

module Monitors
  
  class Cloud < BaseMonitor
    
    def get(_unused=nil)
      my_cloud["options"]["minimum_instances"]
    end
    
  end
  
end