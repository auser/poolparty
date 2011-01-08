module PoolParty
  class Cloud < Base
    default_options(
      :description            => "PoolParty cloud",
      :minimum_instances      => 1,
      :maximum_instances      => 3
    )

    # returns an instance of Keypair
    # You can pass either a filename which will be searched for in ~/.ec2/ and ~/.ssh/
    # Or you can pass a full filepath
    def keypair(n=nil, extra_paths=[])
      return @keypair if @keypair
      @keypair = case n
      when String
        Keypair.new(n, extra_paths)
      when nil
        fpath = CloudProviders::CloudProvider.default_keypair_path/"#{proper_name}"
        File.exists?(fpath) ? Keypair.new(fpath, extra_paths) : generate_keypair(extra_paths)
      else
        raise ArgumentError, "There was an error when defining the keypair"
      end
    end

    private
    def generate_keypair(extra_paths=[])
      puts "Generate the keypair for this cloud because its not found: #{proper_name}"
      cloud_provider.send :generate_keypair, proper_name
      Keypair.new(proper_name, extra_paths)
    end

    def after_initialized
      raise PoolParty::PoolPartyError.create("NoCloudProvider", <<-EOE
You did not specify a cloud provider in your clouds.rb. Make sure you have a block that looks like:

  using :ec2
      EOE
      ) unless cloud_provider
      security_group(proper_name, :authorize => {:from_port => 22, :to_port => 22}) if security_groups.empty?
    end

    public
    def instances(arg)
      case arg
      when Range
        minimum_instances arg.first
        maximum_instances arg.last
      when Fixnum
        minimum_instances arg
        maximum_instances arg
      when Hash
        minimum_instances arg[:instances].to_i
        maximum_instances arg[:instances].to_i
        # nodes(arg)
      else
        raise PoolParty::PoolPartyError.create("DslMethodCall", "You must call instances with either a number, a range or a hash (for a list of nodes)")
      end
    end

    # Upload the source to dest ( using rsync )
    def upload source, dest
      @uploads ||= []
      @uploads << { :source => source, :dest => dest }
    end

    # The pool can either be the parent (the context where the object is declared)
    # or the global pool object
    def pool
      parent || pool
    end

    def tmp_path
      "/tmp/poolparty" / pool.name / name
    end

    public

    attr_reader :cloud_provider
    def using(provider_name, &block)
      return @cloud_provider if @cloud_provider
      @cloud_provider = "#{provider_name}".constantize(CloudProviders).send(:new, provider_name, :cloud => self, &block)
      # Decorate the cloud with the cloud_provider methods
      (class << self; self; end).instance_variable_set('@cloud_provider', @cloud_provider)
        (class << self; self; end).class_eval do
          @cloud_provider.public_methods(false).each do |meth|
            next if respond_to?(meth) || method_defined?(meth) || private_method_defined?(meth)
            eval <<-EOE
              def #{meth}(*args, &block)
                @cloud_provider.send(:#{meth}, *args, &block)
              end
            EOE
        end
      end
    end

    def chef(chef_type=:solo, &block)
      raise ArgumentError, "Chef type must be one of #{Chef.types.map{|v| ":" + v.to_s}.join(",")}." unless Chef.types.include?(chef_type)
      @chef||=Chef.get_chef(chef_type,self,&block)
    end
    # compile the cloud spec and execute the compiled system and remote calls
    def run
      puts "  running on #{cloud_provider.class}"
      cloud_provider.run
      unless @chef.nil?
        compile!
        bootstrap!
      end
    end


    # TODO: Incomplete and needs testing
    # Shutdown and delete the load_balancers, auto_scaling_groups, launch_configurations,
    # security_groups, triggers and instances defined by this cloud
    def teardown
      raise "Only Ec2 teardown supported" unless cloud_provider.name.to_s == 'ec2'
      puts "! Tearing down cloud #{name}"
      # load_balancers.each do |name, lb|
      #   puts "! Deleting load_balaner #{lb_name}"
      #   lb.teardown
      # end
      load_balancers.each do |lb|
        puts "-----> Tearing down load balancer: #{lb.name}"
        lb.teardown
      end

      rds_instances.each do |rds|
        puts "-----> Tearing down RDS Instance: #{rds.name}"
        rds.teardown
      end
      # instances belonging to an auto_scaling group must be deleted before the auto_scaling group
      #THIS SCARES ME! nodes.each{|n| n.terminate_instance!}
      # loop {nodes.size>0 ? sleep(4) : break }
      if autoscalers.empty?
        nodes.each do |node|
          node.terminate!
        end
      else
        autoscalers.each do |a|
          puts "-----> Tearing down autoscaler #{a.name}"
          a.teardown
        end
      end
      # autoscalers.keys.each do |as_name|
      #   puts "! Deleting auto_scaling_group #{as_name}"
      #   cloud_provider.as.delete_autoscaling_group('AutoScalingGroupName' => as_name)
      # end
      #TODO: keypair.delete # Do we want to delete the keypair?  probably, but not certain
    end

    def reboot!
      orig_nodes = nodes
      if autoscalers.empty?
        puts <<-EOE
No autoscalers defined
  Launching new nodes and then shutting down original nodes
        EOE
        # Terminate the nodes
        orig_nodes.each_with_index do |node, i|
          # Start new nodes
          print "Starting node: #{i}...\n"
          expand_by(1)
          print "Terminating node: #{i}...\n"
          node.terminate!
          puts ""
        end
      else
        # Terminate the nodes
        @num_nodes = orig_nodes.size
        orig_nodes.each do |node|
          node.terminate!
          puts "----> Terminated node: #{node.instance_id}"
          # Wait for the autoscaler to boot the next node
          puts "----> Waiting for new node to boot via the autoscaler"
          loop do
            reset!
            break if nodes.size == @num_nodes
            $stdout.print "."
            $stdout.flush
            sleep 1
          end
        end
      end
      run
      puts ""
    end

    def compile!
      unless @uploads.nil?
        puts "Uploading files via rsync"
        @uploads.each do |upload|
          rsync upload[:source], upload[:dest]
        end
      end
      @chef.compile! unless @chef.nil?
    end

    def bootstrap!
      cloud_provider.bootstrap_nodes!(tmp_path)
    end

    def configure!
      compile!
      cloud_provider.configure_nodes!(tmp_path)
    end

    def reset!
      cloud_provider.reset!
    end

    def ssh(num=0)
      nodes[num].ssh
    end

    def rsync(source, dest)
      nodes.each do |node|
        node.rsync(:source => source, :destination => dest)
      end
    end

    # TODO: list of nodes needs to be consistentley sorted
    def nodes
      cloud_provider.nodes.select {|a| a.in_service? }
    end

    # Run command/s on all nodes in the cloud.
    # Returns a hash of instance_id=>result pairs
    def cmd(commands, opts={})
      key_by = opts.delete(:key_by) || :instance_id
      results = {}
      threads = nodes.collect do |n|
        puts "result for #{n.instance_id} ==> n.ssh(#{commands.inspect}, #{opts.inspect})"
        Thread.new{ results[ n.send(key_by) ] = n.ssh(commands, opts) }
      end
      threads.each{ |aThread| aThread.join }
      results
    end

    def cssh(user = nil, use_keypair = nil)
      opts = []
      opts.push "-i #{keypair.full_filepath}" if use_keypair
      opts.push "-l #{user}" if user
      opts = opts.join(" ")
      puts "cssh -o '#{opts}' " + nodes.map{|n| n.public_ip}.join(" ") + " &"
      system "cssh -o '#{opts}' " + nodes.map{|n| n.public_ip}.join(" ") + " &"
    end

    # Explicit proxies to cloud_provider methods
    def run_instance(o={}); cloud_provider.run_instance(o);end
    def terminate_instance!(o={}); cloud_provider.terminate_instance!(o);end
    def describe_instances(o={}); cloud_provider.describe_instances(o);end
    def describe_instance(o={}); cloud_provider.describe_instance(o);end

    def proper_name
      "#{parent.name}-#{name}"
    end
  end
end
