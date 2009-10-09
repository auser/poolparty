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
    
    attr_reader :cloud_provider
    def using(provider_name, &block)
      return @cloud_provider if @cloud_provider
      @cloud_provider = "#{provider_name}".constantize(CloudProviders).send :new, provider_name, :cloud => self, &block
    end
    def run
      puts "  running on #{cloud_provider.class}"
      
      load_balancers.each do |lb_name, lb|
        cloud_provider.load_balancer(*lb)
      end
      autoscalers.each do |as_name, as|
        cloud_provider.autoscale(*as)
      end
      cloud_provider.run
      
      unless chef_repo.nil?
        compile!
        bootstrap!
      end
    end
    
    # TODO: Incomplete and needs testing
    # Shutdown and delete the load_balancers, auto_scaling_groups, launch_configurations,
    # security_groups, triggers and instances defined by this cloud
    def teardown
      raise "Only Ec2 teardown supported" unless cloud_provider.name.to_s == 'ec2'
      puts "!! Tearing down cloud #{name}"
      load_balancers.keys.each do |lb_name|
        puts "! Deleting load_balaner #{lb_name}"
        cloud_provider.elb.delete_load_balancer(:load_balancer_name => lb_name)
      end
      # instances belonging to an auto_scaling group must be deleted before the auto_scaling group
      #THIS SCARES ME! nodes.each{|n| n.terminate_instance!}
      # loop {nodes.size>0 ? sleep(4) : break }
      autoscalers.keys.each do |as_name|
        puts "! Deleting auto_scaling_group #{as_name}"
        cloud_provider.as.delete_autoscaling_group('AutoScalingGroupName' => as_name)
      end
      #TODO: keypair.delete # Do we want to delete the keypair?  probably, but not certain
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
    
    def ssh(num=0)
      nodes[num].ssh
    end
    
    def nodes
      cloud_provider.nodes.select {|a| a.in_service? }
    end
    
    def proper_name
      "#{parent.name}-#{name}"
    end
  end
end
