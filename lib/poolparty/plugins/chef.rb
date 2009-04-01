module PoolParty
  class ChefRecipe
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
          if ::File.file? file
            file = ::File.expand_path file
            basedir = "/tmp/poolparty/dr_configure/tmp/recipes/main"
            ::FileUtils.mkdir_p "#{basedir}/recipes" unless ::File.directory? basedir
            ::File.cp file, "#{basedir}/recipes/default.rb"

            if o[:templates]
              ::FileUtils.mkdir_p "#{basedir}/templates/default/"
              o[:templates].each {|f| ::File.cp f, "#{basedir}/templates/default/#{::File.basename(f)}" }
            end
            
            recipe_files << basedir
          end
        # TODO: Enable neat syntax from within poolparty
        else
          raise <<-EOR
            PoolParty currently only supports passing recipes as files. Please specify a file in your chef block and try again"
          EOR
        #   recipe = ChefRecipe.new
        #   recipe.instance_eval &block          
        #   ::File.open("/tmp/poolparty/chef_main.rb", "w+") {|f| f << @recipe.options.to_json }
        #   recipe_dirs << "/tmp/poolparty/poolparty_chef_recipe.rb"
        end
      end
      
      def json file=nil, &block
        if file
          if ::File.file? file
            ::File.cp file, "/tmp/poolparty/dna.json"
            @json_file = "/tmp/poolparty/dna.json"
          elsif file.is_a?(String)
            require "tempfile"
            ::File.open("/tmp/poolparty/dna.json", "w+") do |tf|
              tf << file # is really a string
            end
          else
            raise <<-EOM
              Your json must either point to a file that exists or a string. Please check your configuration and try again
            EOM
          end
        else
          unless @recipe
            @recipe = ChefRecipe.new
            @recipe.instance_eval &block
            @recipe.recipes(@recipe.recipes? ? (@recipe.recipes << ["main", "poolparty"]) : ["main", "poolparty"])
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
        end
      end
      
      def config file=""
        if ::File.file? file
          @config_file = file
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
          ::File.open("/tmp/poolparty/chef_config.rb", "w+") do |tf|
            tf << conf_string
          end
          @config_file = "/tmp/poolparty/chef_config.rb"
        end
      end
      
      def added_recipes
        @added_recipes ||= []
      end
      
      def before_configure
        config unless @config_file
        ::Suitcase::Zipper.add(@config_file, "chef")
        added_recipes.each do |rcp|
          # ::FileUtils.cp_r rcp, "/tmp/poolparty/dr_configure/recipes/"
          ::Suitcase::Zipper.add(rcp, "chef/recipes")
        end        
        ::Suitcase::Zipper.add(@json_file, "chef/json") if @json_file
        recipe_files.each do |rf|
          # ::FileUtils.cp_r rf, "/tmp/poolparty/dr_configure/recipes/#{::File.basename(rf)}"
          ::Suitcase::Zipper.add(rf, "chef/recipes") 
        end
      end
      
    end
    
  end
end