require File.dirname(__FILE__) + "/display"

module PoolParty
  module Console
    
    include Display
    
    # Load a file that contains a pool into memory
    def load_pool(filename)
      filename = filename.chomp
      unless filename && ::File.readable?(filename)
        puts "Could not load pool: #{filename}"
        exit
      else
        PoolParty::Script.inflate(open(filename).read, File.dirname(filename))
      end
    end
    
    def extract_cloud_from_options(o)
      o.cloudname ? [cloud(o.cloudname.downcase.to_sym)] : [clouds[clouds.keys.first]]
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