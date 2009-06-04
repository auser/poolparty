require "tempfile"
# BIG TODO: Slim the place where the content is gathered from
module PoolParty
      
  module Plugin
    
    class Chef < Plugin
      def before_load(o, &block)
      end
      
      def loaded o={}, &block
      end
      
      def recipe_files
        @recipe_files ||= []
      end
      
      def basedir
        @basedir ||= "#{cloud.tmp_path}/dr_configure/chef/cookbooks/main"
      end
      
      def recipe(file=nil, o={}, &block)
        if file
          file = search_in_known_locations(file)
          raise RecipeNotFoundError.new(file) unless file
          
          ::FileUtils.mkdir_p "#{basedir}/recipes" unless ::File.directory?("#{basedir}/recipes")          
          ::FileUtils.rm "#{basedir}/recipes/default.rb" if ::File.file?("#{basedir}/recipes/default.rb")
          # ::FileUtils.cp file, "#{basedir}/recipes/default.rb"
          ::File.open("#{basedir}/recipes/default.rb", "w") {|f| f << open(file).read }
                    
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
              ::FileUtils.cp f, "#{basedir}/templates/default/#{::File.basename(f)}"
            elsif ::File.directory?(f)
              Dir["#{f}/**"].each {|f| ::FileUtils.cp f, "#{basedir}/templates/default/#{::File.basename(f)}" }
            else
              tfile = Tempfile.new("main-poolparty-recipe")
              tfile << f # copy the string into the temp file
              ::FileUtils.cp tfile.path, "#{basedir}/templates/default/#{::File.basename(f)}"
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
            @recipe = has_chef_recipe "poolparty", &block
            @recipe.instance_eval(&block) if block
            @recipe.recipes(recipe_files.empty? ? ["poolparty"] : ["poolparty", "main"])
            
            ::Suitcase::Zipper.add_content_as(@recipe.dsl_options.to_json, "dna.json", "chef")
            
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
          ::Suitcase::Zipper.add_content_as(conf_string, "solo.rb", "chef")
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
  class RecipeNotFoundError < StandardError
    def initialize(n)
      super("The recipe you specified cannot be found: #{n}")
    end 
  end
end