module PoolParty
    
  class Pool < Base
    attr_accessor :verbose, :very_verbose, :debugging, :very_debugging, :auto_execute
    
    def cloud(name, &block)
      clouds[name.to_s] = Cloud.new(name.to_s, {:parent => self}, &block)
    end
    
    def clouds
      @clouds ||= {}
    end
    
    # Run command/s on all nodes in the pool.
    # Returns a hash in the form of {cloud => [{instance_id=>result}]}
    def cmd(commands, opts={})
      results = {}
      threads = clouds.collect do |name, c|
        Thread.new{ results[ name ] = c.cmd(commands, opts) }
      end
      threads.each{ |aThread| aThread.join }
      results
    end
        
    at_exit do
      if pool.auto_execute
        puts <<-EOE
----> Running #{pool.name} #{pool.auto_execute}
        EOE
        pool.run
      end
    end
    
    # === Description
    #
    # Set / Get the recipe_set which will be executed on the remote
    # host
    def recipe_set name = nil
      @selected_recipe_set ||= :default
      if name
        @selected_recipe_set = name.to_sym
      end
      @selected_recipe_set
    end
    
    def run
      clouds.each do |cloud_name, cld|
        puts "----> Starting to build cloud #{cloud_name}"
        cld.run
      end
    end

    def to_hash
      c = {}
      clouds.each do |cloud_name, cld|
        cld.nodes.collect do |node|
          h = {}
          [:dns_name, :private_ip, :public_ip].each do |f|
            h[f] = node[f]
          end
          c[cloud_name] = h
        end
      end
      c
    end

  end

end
