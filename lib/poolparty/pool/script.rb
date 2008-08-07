module PoolParty
  
  class Script
    
    def self.inflate(script, file="__SCRIPT__")
      apool = new
      apool.instance_eval script, file
      apool.inflate
    end
    
    def inflate
      @pool.inflate if @pool
    end
    
  end
  
end