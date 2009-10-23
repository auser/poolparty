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
      security_group if security_groups.empty?
      setup_extras
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
        nodes(arg)
      else
        raise PoolParty::PoolPartyError.create("DslMethodCall", "You must call instances with either a number, a range or a hash (for a list of nodes)")
      end
    end
    
    # Chef    
    def chef_repo(filepath=nil)
      return @chef_repo if @chef_repo
      @chef_repo = filepath.nil? ? nil : File.expand_path(filepath)
    end
    
    def chef_attributes(hsh={}, &block)
      @chef_attributes ||= ChefAttribute.new(hsh, &block)
    end
    
    def recipe(recipe_name, hsh={})
      _recipes << recipe_name unless _recipes.include?(recipe_name)
      _attributes.merge!(recipe_name => hsh) unless hsh.empty?
    end
    
    def recipes(*recipes)
      recipes.each do |r|
        recipe(r)
      end
    end
    
    private
    
    def _recipes
      @_recipes ||= []
    end
    def _attributes
      @_attributes ||= {}
    end
    
    # The NEW actual chef resolver.
    def build_tmp_dir
      base_directory = tmp_path/"etc"/"chef"
      puts "Copying the chef-repo into the base directory from #{chef_repo}"
      FileUtils.mkdir_p base_directory/"roles"   
      if File.directory?(chef_repo)
        FileUtils.cp_r chef_repo, base_directory 
      end
      puts "Creating the dna.json"
      chef_attributes.to_dna [], base_directory/"dna.json", {:run_list => ["role[#{name}]"]}
      write_solo_dot_rb
      write_chef_role_json tmp_path/"etc"/"chef"/"roles/#{name}.json"
    end
    
    def write_solo_dot_rb(to=tmp_path/"etc"/"chef"/"solo.rb")
      content = <<-EOE
cookbook_path     ["/etc/chef/chef-repo/site-cookbooks", "/etc/chef/chef-repo/cookbooks"]
role_path         "/etc/chef/roles"
log_level         :info
      EOE

      File.open(to, "w") do |f|
        f << content
      end
    end
    
    def write_chef_role_json(to=tmp_path/"etc"/"chef"/"dna.json")
      ca = ChefAttribute.new({
        :name => name,
        :json_class => "Chef::Role",
        :chef_type => "role",
        :default_attributes => chef_attributes.init_opts,
        :override_attributes => {},
        :description => description
      })
      ca.to_dna _recipes.map {|a| File.basename(a) }, to
    end
    
    def tmp_path
      "/tmp/poolparty" / pool.name / name
    end
    
    public
    
    def load_balancer(name=proper_name, o={}, &block)
      load_balancers[name] = [name, o, block]
    end
    def load_balancers;@load_balancers ||= {};end
        
    def autoscaler(name=proper_name, o={}, &block)
      autoscalers[name] = [name, o, block]
    end
    def autoscalers;@autoscalers ||= {};end
    
    def security_group(name=proper_name, o={}, &block)
      security_groups[name] = [name, o, block]
    end
    def security_groups; @security_groups ||= {};end
    
    def scaling_activities(name=nil)
      autoscalers.each{|as| as.describe_scaling_activities}
    end
    
    attr_reader :cloud_provider
    def using(provider_name, &block)
      return @cloud_provider if @cloud_provider
      @cloud_provider = "#{provider_name}".constantize(CloudProviders).send :new, provider_name, :cloud => self, &block
    end
    
    # proxy to cloud_provider
    def method_missing(m,*a,&block)
      if cloud_provider.respond_to?(m)
        cloud_provider.send(m,*a,&block)
      else
        super
      end
    end
    
    # compile the cloud spec and execute the compiled system and remote calls
    def run
      puts "  running on #{cloud_provider.class}"      
      cloud_provider.run
      unless chef_repo.nil?
        compile!
        bootstrap!
      end
    end
    
    def setup_extras
      load_balancers.each do |lb_name, lb|
        cloud_provider.load_balancer(*lb)
      end
      autoscalers.each do |as_name, as|
        cloud_provider.autoscale(*as)
      end
      security_groups.each do |sg_name, sg|
        cloud_provider.security_group(*sg)
      end
    end
    
    
    # TODO: Incomplete and needs testing
    # Shutdown and delete the load_balancers, auto_scaling_groups, launch_configurations,
    # security_groups, triggers and instances defined by this cloud
    def teardown
      raise "Only Ec2 teardown supported" unless cloud_provider.name.to_s == 'ec2'
      puts "!! Tearing down cloud #{name}"
      # load_balancers.each do |name, lb|
      #   puts "! Deleting load_balaner #{lb_name}"
      #   lb.teardown
      # end
      setup_extras
      cloud_provider.load_balancers.each do |lb|
        puts "-----> Tearing down load balancer: #{lb.name}"
        lb.teardown
      end
      # instances belonging to an auto_scaling group must be deleted before the auto_scaling group
      #THIS SCARES ME! nodes.each{|n| n.terminate_instance!}
      # loop {nodes.size>0 ? sleep(4) : break }
      if autoscalers.empty?
        nodes.each do |node|
          node.terminate!
        end
      else
        cloud_provider.autoscalers.each do |a|
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
      num_nodes = nodes.size
      if autoscalers.empty?
        puts <<-EOE
No autoscalers defined
  Launching new nodes and then shutting down original nodes
        EOE
        # Start new nodes
        expand_by(num_nodes)
        # Terminate the nodes
        orig_nodes.each do |node|
          node.terminate!
        end
      else
        # Terminate the nodes
        orig_nodes.each do |node|
          node.terminate!
          puts "----> Terminated node: #{node.instance_id}"
          # Wait for the autoscaler to boot the next node
          puts "----> Waiting for new node to boot via the autoscaler"
          loop do
            reset!
            break if nodes.size == num_nodes
            $stdout.print "."
            $stdout.flush
            sleep 1
          end
        end
      end
      puts ""
    end
    
    def compile!
      build_tmp_dir unless chef_repo.nil?
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
    def all_nodes
      cloud_provider.all_nodes
    end
    
    # Run command/s on all nodes in the cloud.
    # Returns a hash of instance_id=>result pairs
    def cmd(commands, opts={})
      opts[:key_by]= :instance_id unless opts[:key_by]
      results = {}
      threads = nodes.collect do |n|
        puts "result for #{n.instance_id} ==> #{n.ssh(commands, opts)}"
         Thread.new{ results[ n.send(opts[:key_by]) ] = n.ssh(commands, opts) }
      end
      threads.each{ |aThread| aThread.join }
      results
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
