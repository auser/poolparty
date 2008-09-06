module PoolParty
  
  class Script
        
    def self.inflate_file(file)
      inflate open(file).read
    end
    
    def self.inflate(script, file="__SCRIPT__")
      apool = new
      apool.instance_eval script, file
      apool.inflate
    end
    
    def inflate
      pools.map {|name,pool| pool.inflate } unless pools.empty?
    end
    
  end
  
end