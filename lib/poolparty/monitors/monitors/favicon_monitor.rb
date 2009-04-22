=begin rdoc
  Simple dummy monitor to return a blank response to favicon requests.
=end
module Monitors
  
  class Favicon < BaseMonitor
    def get(data=nil)
      ''
    end
  end
  
end