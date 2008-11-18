require File.dirname(__FILE__) + "/display"

module PoolParty
  module Console
    
    include Display
    
    # Load a file that contains a pool into memory
    def load_pool(filename)

      unless filename && ::File.readable?(filename)
        puts "Please specify your cloud with -s, move it to ./pool.spec or in your POOL_SPEC environment variable"
        exit(1)
      else
        PoolParty::Script.inflate(open(filename).read, filename)
      end
    end
    
    def extract_cloud_from_options(o)
      o.cloudname ? [cloud(o.cloudname.downcase.to_sym)] : clouds.collect {|n,cl| cl}
    end
    
    def extract_pool_from_options(o)
      o.poolname ? [pool(o.poolname.downcase.to_sym)] : pools.collect {|n,pl| pl}
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