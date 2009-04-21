=begin rdoc
  Simple dummy monitor to return a blank response to favicon requests.
=end
module Monitors
  
  class Favicon
    def self.get
      ''
    end
  end
  
end