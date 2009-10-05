module PoolParty
    
  class Pool < Base
    attr_accessor :verbose, :very_verbose, :debugging, :very_debugging
    def cloud(name, &block)
      clouds[name] = Cloud.new(name, {:parent => self}, &block)
    end
    def clouds
      @clouds ||= {}
    end
    
    at_exit do
      puts <<-EOE
----> Running #{pool.name}
      EOE
      pool.run
    end
    
    def run
      clouds.each do |cloud_name, cld|
        puts "---- Starting to build cloud #{cloud_name}"
        cld.run
      end
    end
  end

end