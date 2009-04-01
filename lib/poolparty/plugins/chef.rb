module PoolParty
  class ChefRecipe
    include Dslify
  end
  class ChefTemplate
    include Dslify
  end
  class Chef
    
    plugin :chef do
      def before_load(o, &block)        
        bootstrap_gems "chef", "ohai"
        bootstrap_commands [
          "mkdir -p /etc/chef/cookbooks /etc/chef/cache"      
        ]
      end
      
      def recipe_files
        @recipe_files ||= []
      end
      
      def recipe file=nil, o={}, &block
        if file
          file = ::File.expand_path file
          basedir = "/tmp/poolparty/dependencies/recipes/cookbooks/poolparty"
          ::FileUtils.mkdir_p "#{basedir}/recipes" unless ::File.directory? basedir
          ::File.cp file, "#{basedir}/recipes/default.rb"
          
          if o[:templates]
            ::FileUtils.mkdir_p "#{basedir}/templates/default/"
            
            o[:templates].each do |f|
              ::File.cp f, "#{basedir}/templates/default/#{::File.basename(f)}"
            end
          end
          recipe_files << basedir
        # TODO: Enable neat syntax from within poolparty
        # else
        #   recipe = ChefRecipe.new
        #   recipe.instance_eval &block          
        #   ::File.open("/tmp/poolparty/chef_main.rb", "w+") {|f| f << @recipe.options.to_json }
        #   recipe_dirs << "/tmp/poolparty/poolparty_chef_recipe.rb"
        end
      end
      
      def json file=nil, &block
        if file
          @json_file = file
        else
          unless @recipe
            @recipe = ChefRecipe.new
            @recipe.instance_eval &block
            @recipe.recipes(@recipe.recipes? ? (@recipe.recipes << "poolparty") : ["poolparty"])
            ::File.open("/tmp/poolparty/dna.json", "w+") {|f| f << @recipe.options.to_json }
            @json_file = "/tmp/poolparty/dna.json"
          end
        end
      end
      
      def include_recipes *recps
        unless recps.empty?
          recps.each do |rcp|
            Dir[::File.expand_path(rcp)].each do |f|
              added_recipes << f
            end            
          end
          bootstrap_commands ["cp -R /var/poolparty/dependencies/chef/recipes/* /etc/chef/cookbooks"] unless added_recipes.empty?
        end
      end
      
      def config file=""
        if ::File.file? file
          ::Suitcase::Zipper.add(file, "chef")
        else
          conf_string = if file.empty?
# default config
            <<-EOE
cookbook_path     "/etc/chef/cookbooks"
node_path         "/etc/chef/nodes"
log_level         :info
file_store_path  "/etc/chef"
file_cache_path  "/etc/chef"
            EOE
          else
            open(file).read
          end
          require "tempfile"
          ::File.open("/tmp/poolparty/chef_config.rb", "w+") do |tf|
            tf << conf_string
          end
          ::Suitcase::Zipper.add("/tmp/poolparty/chef_config.rb", "chef")
        end
        bootstrap_commands ["cp /var/poolparty/dependencies/chef/chef_config.rb /etc/chef/solo.rb"]
        @config_set = true
      end
      
      def added_recipes
        @added_recipes ||= []
      end
      
      # TODO: Change to before_configure
      def before_bootstrap
        config unless @config_set
        added_recipes.each do |rcp|
          ::Suitcase::Zipper.add(rcp, "chef/recipes")
        end
        if @json_file
          bootstrap_commands ["cp /var/poolparty/dependencies/chef/json/#{::File.basename(@json_file)} /etc/chef/dna.json"]
          ::Suitcase::Zipper.add(@json_file, "chef/json")
        end
        recipe_files.each do |rf|
          ::Suitcase::Zipper.add(rf, "chef/recipes")
        end
      end
      
    end
    
  end
end