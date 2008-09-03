module PoolParty
  module Console
    
    # Load a file that contains a pool into memory
    def load_pool(filename)
      PoolParty::Script.inflate open(filename).read
    end
    
    # Clear all the pools and reload the console
    # Call within console to reset and reload the entire poolparty base
    # as well
    def reload!      
      reset!
      require "#{File.join(File.dirname(__FILE__), %w(.. poolparty.rb))}"
      require "#{File.join(File.dirname(__FILE__), %w( console.rb))}"
    end
    
    def pool_print(p=nil, prev="")
      returning String.new do |out|
        out << "hi"
      end
    end
    
  end
end

class Object
  include PoolParty::Console
end