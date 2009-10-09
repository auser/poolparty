module PoolParty
    
  class Pool < Base
    attr_accessor :verbose, :very_verbose, :debugging, :very_debugging, :do_not_execute
    
    def cloud(name, &block)
      clouds[name] = Cloud.new(name, {:parent => self}, &block)
    end
    def clouds
      @clouds ||= {}
    end
        
    at_exit do
      if pool.auto_execute
        puts <<-EOE
  ----> Running #{pool.name} #{pool.do_not_execute}
        EOE
        pool.run
      end
    end
    
    def run
      clouds.each do |cloud_name, cld|
        puts "---- Starting to build cloud #{cloud_name}"
        cld.run
      end
    end
  end

end