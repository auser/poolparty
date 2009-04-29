require "tempfile"
# BIG TODO: Slim the place where the content is gathered from
module PoolParty
  class ChefRecipe
    include Dslify
  end
  class Chef
    define_resource :chef_recipe do
    end
    define_resource :chef_library do
    end
    
    plugin :include_chef_recipe do
      def loaded(opts={}, &block)
        parent.has_chef_recipe ::File.basename(name)
      end
      def before_configure
        ::Suitcase::Zipper.add(name, "chef/cookbooks") if ::File.exist?(name)
      end
    end
    
    plugin :chef do
      def before_load(o, &block)
      end
      
      def loaded o={}, &block
      end
      
      def recipe_files
        @recipe_files ||= []
      end
      
      def basedir
        @basedir ||= "#{Default.tmp_path}/dr_configure/chef/cookbooks/main"
      end
      
      def recipe(file=nil, o={}, &block)
        if file
          file = ::File.expand_path(file)
          ::FileUtils.mkdir_p "#{basedir}/recipes"
                    
          unless ::File.file?(file)
            tfile = Tempfile.new("main-poolparty-recipe")
            tfile << file # copy the string into the temp file
            file = tfile.path
          end
          ::FileUtils.rm "#{basedir}/recipes/default.rb" if ::File.file?("#{basedir}/recipes/default.rb")
          ::File.cp file, "#{basedir}/recipes/default.rb"
          
          templates o[:templates] if o[:templates]
          
          recipe_files << basedir
          # ::Suitcase::Zipper.add(basedir, "chef/cookbooks")
        # TODO: Enable neat syntax from within poolparty
        
        else
          raise <<-EOR
            PoolParty currently only supports passing recipes as files. Please specify a file in your chef block and try again"
          EOR
        end
      end
      
      def templates(templates=[])
        if templates
          ::FileUtils.mkdir_p "#{basedir}/templates/default/"
          templates.each do |f|
            f = ::File.expand_path(f)
            if ::File.file?(f)
              ::File.cp f, "#{basedir}/templates/default/#{::File.basename(f)}"
            elsif ::File.directory?(f)
              Dir["#{f}/**"].each {|f| ::File.cp f, "#{basedir}/templates/default/#{::File.basename(f)}" }
            else
              tfile = Tempfile.new("main-poolparty-recipe")
              tfile << f # copy the string into the temp file
              ::File.cp tfile.path, "#{basedir}/templates/default/#{::File.basename(f)}"
            end
          end
        end
      end
      
      def json file=nil, &block
        if file
          if ::File.file? file
            ::Suitcase::Zipper.add_content_as(open(file).read, "dna.json", "chef")
          elsif file.is_a?(String)
            ::Suitcase::Zipper.add_content_as(file, "dna.json", "chef")
          else
            raise <<-EOM
              Your json must either point to a file that exists or a string. Please check your configuration and try again
            EOM
          end
        else
          unless @recipe
            @recipe = ChefRecipe.new
            @recipe.instance_eval &block if block
            @recipe.recipes(recipe_files.empty? ? ["poolparty"] : ["poolparty", "main"])
            # ::File.open("#{Default.tmp_path}/dr_configure/dna.json", "w+") {|f| f << @recipe.options.to_json }
            ::Suitcase::Zipper.add_content_as(@recipe.options.to_json, "dna.json", "chef")
            
            configure_commands ["cp -f /var/poolparty/dr_configure/chef/dna.json /etc/chef/dna.json"]
          end
        end
      end
      
      def include_recipes *recps
        unless recps.empty?
          recps.each do |rcp|
            Dir[::File.expand_path(rcp)].each do |f|
              included_recipes << f
            end            
          end
        end
      end
      
      def included_recipes
        @included_recipes ||= []
      end
      
      def config file=""
        if ::File.file? file
          ::Suitcase::Zipper.add_content_as(open(file).read, "solo.rb", "chef")
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
          # ::FileUtils.mkdir_p "#{Default.tmp_path}/trash" unless ::File.directory? "#{Default.tmp_path}/trash"
          # ::File.open("#{Default.tmp_path}/trash/solo.rb", "w+") do |tf|
          #   tf << conf_string
          # end
          ::Suitcase::Zipper.add_content_as(conf_string, "solo.rb", "chef")
          # ::Suitcase::Zipper.add("#{Default.tmp_path}/trash/solo.rb", "chef")
        end
      end
      
      def added_recipes
        @added_recipes ||= []
      end
      
      def before_bootstrap
        bootstrap_gems "chef", "ohai"
        bootstrap_commands [
          "mkdir -p /etc/chef/cookbooks /etc/chef/cache"          
        ]
      end
      def before_configure
        config
        json
        
        included_recipes.each do |f|
          ::Suitcase::Zipper.add(f, "chef/cookbooks")
        end
        
        if ::File.directory?("/etc/chef")
          ::Suitcase::Zipper.add("/etc/chef/cookbooks/*", "chef/cookbooks")
          ::Suitcase::Zipper.add("/etc/chef/dna.json", "chef/json")
          ::Suitcase::Zipper.add("/etc/chef/solo.rb", "chef/")
        end
        
      end
      
    end
    
  end
end