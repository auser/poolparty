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
    
    def self.to_ruby(opts={},&blk)
      blk.to_ruby(opts)
    end
    
    def self.for_save_string
      returning Array.new do |out|
        pools.collect {|n,pl| pl}.each do |pl|
          out << "pool :#{pl.name} do"
          clouds.each do |n,cl|
            out << <<-EOE
  cloud :#{cl.name} do
    #{cl.minimum_runnable_options.map {|o| "#{o} #{cl.send(o).respec_string}"}.join("\n")}
  end
            EOE
          end
          out << "end"
        end
      end.join("\n")
    end
    
    def self.save!(to_file=true)
      write_to_file_in_storage_directory(Base.default_specfile_name, for_save_string) if to_file
      for_save_string
    end
    
  end
  
end