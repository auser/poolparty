module PoolParty
    
  class Pool < Base
    attr_accessor :verbose, :very_verbose, :debugging, :very_debugging, :auto_execute
    
    def cloud(name, &block)
      c = Cloud.new(name.to_s, {:parent => self}, &block)
      clouds[name.to_s] = c
      # Create a dummy security group for tagging purposes. Do not want to
      # conflict with advance usage of security groups
      c.security_group "#poolparty-#{c.proper_name}"
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
    # Set / Get the chef_step which will be executed on the remote
    # host
    def chef_step name = nil
      @selected_chef_step ||= :default
      if name
        @selected_chef_step = name.to_sym
      end
      @selected_chef_step
    end
    
    def run
      clouds.each do |cloud_name, cld|
        puts "----> Starting to build cloud #{cloud_name}"
        cld.run
      end
    end

    def to_hash
      c = clouds.collect do |cloud_name, cld|
        nodes = cld.nodes.collect do |node|
          h = {}
          [:dns_name, :private_ip, :public_ip].each do |f|
            h[f] = node[f]
          end
          h
        end
        { cloud_name => nodes }
      end
      h = c.inject({})do |old, new|
       old.merge! new
      end 
      {:clouds => h }
    end

  end

end
