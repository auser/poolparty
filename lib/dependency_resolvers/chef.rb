=begin rdoc
  Base dependency_resolver
=end
# Require the chef-only resources
$:.unshift("#{File.dirname(__FILE__)}/chef")

%w(http_request remote_directory).each do |res|
  require "resources/#{res}"
end

module PoolParty
  module DependencyResolvers
    
    class Chef < Base
      
      class << self
        attr_reader :meal
        
        def before_compile
          raise PoolParty::PoolPartyError.create("ChefCompileError", "No compile_directory is specified. Please specify one.") unless compile_directory
          FileUtils.mkdir_p compile_directory unless ::File.directory?(compile_directory)
        end
        
        def after_compile(o)
          compile_default_recipe(o)
          compile_variables
          compile_files
          compile_recipes          
        end
           
        # compile the resources
        # Compiles the resource appropriately against the type
        # If the resource is a variable, then we don't have any output
        # as the output will be handled in after_compile with compile_variables
        # If the resource is a file, then add it to the files array and run the
        # compile_resource command (on super) for the output. The file will later
        # be turned into a .erb template file in the compile_directory
        # Otherwise just run the output to get the default.rb recipe
        def compile_resource(res)
          # Apply meta_functions here
          o = case res
          when Resources::Variable
            # do variable stuff
            variables << res
            nil
          when Resources::FileResource
            files << res
            super
          else
            super
          end
          
          apply_meta_functions(res, o)
        end
        
        default_attr_reader :variables, []
        default_attr_reader :files, []
        
        private
        
        # Take this string and apply metafunctions to the string on the resource
        def apply_meta_functions(resource, str)
          str
        end
        
        # Take the variables and compile them into the file attributes/poolparty.rb
        def compile_variables
          FileUtils.mkdir_p compile_directory/"attributes" unless ::File.directory?(compile_directory/"attributes")
          File.open(compile_directory/"attributes"/"poolparty.rb", "w") do |f|
            f << "# PoolParty variables\n"
            f << "poolparty Mash.new unless attribute?('poolparty')\n"
            variables.each do |var|
              f << "poolparty[:#{var.name}] = #{handle_print_variable(var.value)}\n"
            end
          end
        end
        
        # Compile the files
        def compile_files
          FileUtils.mkdir_p compile_directory/"files" unless ::File.directory?(compile_directory/"files")
          files.each do |fi|
            fpath = compile_directory/"templates"/"default"/"#{fi.path}.erb"
            FileUtils.mkdir_p File.dirname(fpath) unless File.directory?(File.dirname(fpath))
            File.open(fpath, "w") do |f|
              f << fi.content
            end
          end
        end
        
        # compile the recipes
        # TODO
        def compile_recipes
        end
        
        def compile_default_recipe(content)
          FileUtils.mkdir_p compile_directory/"recipes" unless ::File.directory?(compile_directory/"recipes")
          File.open(compile_directory/"recipes"/"default.rb", "w") do |f|
            f << content
          end
        end
        
      end
            
    end
    
  end
end