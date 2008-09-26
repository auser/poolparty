require File.dirname(__FILE__) + "/display"

module PoolParty
  module Console
    
    include Display
    
    # Load a file that contains a pool into memory
    def load_pool(filename)
      unless filename && ::File.file?(filename)
        puts "Could not load pool"
        exit
      else
        PoolParty::Script.inflate(open(filename).read, File.dirname(filename))
      end
    end
    
    # Clear all the pools and reload the console
    # Call within console to reset and reload the entire poolparty base
    # as well
    def reload!      
      reset!
    end
    
  end
end

class Object
  include PoolParty::Console
end