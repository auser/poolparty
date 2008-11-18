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
    
    def self.for_save_string(in_cloud=nil)
      returning Array.new do |out|
        (in_cloud ? [in_cloud] : clouds.collect {|n,cl| cl }).each do |cl|
          out << <<-EOE
cloud :#{cl.name} do
#{cl.minimum_runnable_options.map {|o| "\t#{o} #{cl.send(o).respec_string}"}.join("\n")}
end
          EOE
        end
      end.join("\n")
    end
    
    def self.save!(cl=nil,to_file=true)
      write_to_file_in_storage_directory(Base.default_specfile_name, for_save_string(cl)) if to_file
      for_save_string
    end
    
  end
  
end