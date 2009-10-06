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
      chef_attributes.merge!(_attributes)
      chef_attributes.to_dna _recipes.map {|a| File.basename(a) }, base_directory/"dna.json"
      
      write_solo_dot_rb
      write_chef_role_json tmp_path/"etc"/"chef"/"roles/#{name}.json"
    end
    
    def write_solo_dot_rb(to=tmp_path/"etc"/"chef"/"solo.rb")
      content = <<-EOE
cookbook_path     "/etc/chef/cookbooks"
node_path         "/etc/chef/nodes"
log_level         :info
file_store_path  "/etc/chef"
file_cache_path  "/etc/chef"
      EOE

      File.open(to, "w") do |f|
        f << content
      end
    end
    
    def write_chef_role_json(to=tmp_path/"etc"/"chef"/"dna.json")
      File.open(to, "w") do |f|
        f << JSON.pretty_generate({
          :name => name,
          :description => description,
          :recipes => _recipes.map {|r| File.basename(r) }
        })
      end
    end
    
    def tmp_path
      "/tmp/poolparty" / pool.name / name
    end
    
    public
    
    def load_balancer(name=proper_name, o={}, &block);load_balancers[name] = [name, o, block];end
    def load_balancers;@load_balancers ||= {};end
        
    def autoscaler(name=proper_name, o={}, &block);autoscalers[name] = [name, o, block];end
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
        build_tmp_dir
        cloud_provider.bootstrap_nodes!(tmp_path)
      end
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
