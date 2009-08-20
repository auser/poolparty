=begin rdoc
Base dependency_resolver
=end
module DependencyResolvers
  
  class Chef < Base
    
    class << self
      attr_reader :cookbook_directory, :base_cookbook_directory
      
      def before_compile
        @cookbook_directory = compile_directory/"cookbooks"/"poolparty"
        @base_cookbook_directory = compile_directory/"cookbooks"
        raise PoolParty::PoolPartyError.create("ChefCompileError", "No compile_directory is specified. Please specify one.") unless compile_directory
        FileUtils.mkdir_p cookbook_directory unless ::File.directory?(cookbook_directory)
      end
      
      def after_compile(o)
        compile_default_recipe(o)
        compile_variables
        compile_files
        compile_recipes
        
        write_dna_json
        write_solo_dot_rb
      end
      
      def compile_command
        "/usr/bin/chef-solo -c /etc/chef/solo.rb -j /etc/chef/dna.json"
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
        when PoolParty::Resources::Variable
          # do variable stuff
          variables << res
          ""
        when PoolParty::Resources::FileResource
          files << res
          super
        when PoolParty::Resources::ChefAttributesFile
          attribute_files << res
          ""
        when PoolParty::Resources::ChefRecipe
          recipes << res
          super
        else
          super
        end
        
        apply_meta_functions(res, o) if res.is_a?(PoolParty::Resource) && res.print_to_chef != :no_print
      end
      
      default_attr_reader :variables, []
      default_attr_reader :files, []
      default_attr_reader :attribute_files, []
      default_attr_reader :recipes, []
      
      def require_chef_only_resources
        # Require the chef-only resources
        $:.unshift("#{File.dirname(__FILE__)}/chef")
        
        to_define_resoures = []
        %w( http_request remote_directory remote_file route script chef_attributes_file chef_recipe).each do |res|
          require "resources/#{res}"
          PoolParty::Resource.define_resource("PoolParty::Resources::#{res.classify}".constantize)
        end
      end
      
      private
      
      # Take the print_to_chef string and apply metafunctions to the string on the resource
      # If there are no meta functions on the resource, do not touch the resulting
      # string
      def apply_meta_functions(resource, str)
        regex = /[(.*)do(\w*)?(.*)]?(\w)*end$/
        
        add = []
        apply_meta_notifies(resource, add) if resource.meta_notifies
        apply_meta_subscribes(resource, add) if resource.meta_subscribes

        if resource.meta_not_if
          tmp = "  not_if "
          tmp += resource.meta_not_if[1] == :block ? "do #{resource.meta_not_if[0]} end" : "\"#{resource.meta_not_if[0]}\""
          add << tmp
        end
        
        if resource.meta_only_if
          tmp = "   only_if "
          tmp += resource.meta_only_if[1] == :block ? "do #{resource.meta_only_if[0]} end" : "\"#{resource.meta_only_if[0]}\""
          add << tmp
        end
        
        add << "  ignore_failure #{resource.print_variable(resource.ignore_failure)}" if resource.ignore_failure
        add << "  provider #{resource.print_variable(resource.provider)}" if resource.provider
        
        return str if add.empty?
        newstr = str.chomp.gsub(regex, "")
        "#{newstr}#{add.join("\n")}\nend"
      end
      
      def apply_meta_notifies(resource, add)
        # The meta_notifies is a hash that looks like: {:file => [["pool_name", :reload]]}
        resource.meta_notifies.each do |ty, arr|
          arr.each do |nm, action|
            add << "  notifies :#{action}, resources(:#{chef_safe_resource(ty)} => \"#{nm}\")"
          end
        end
      end
      
      def apply_meta_subscribes(resource, add)
        # The meta_subscribes is a hash that looks like: {:file=>[["pool_name", :reload, :immediately]]
        resource.meta_subscribes.each do |ty, arr|
          arr.each do |nm, action, at_time|
            # subscribes :reload, resources\(:service => "apache"\), :delayed
            add << "  subscribes :#{action}, resources(:#{chef_safe_resource(ty)} => \"#{nm}\"), :#{at_time}"
          end
        end
      end
      
      # Cleanup for chef resource output
      # Not particularly clean, but a necessary evil because
      # certain resources don't reflect the chef output
      # such as has_exec corresponds to execute
      def chef_safe_resource(name)
        case name
        when :exec
          "execute"
        else
          name
        end
      end
      
      # Take the variables and compile them into the file attributes/poolparty.rb
      def compile_variables
        # Make sure the attributes/ directory is there
        FileUtils.mkdir_p cookbook_directory/"attributes" unless ::File.directory?(cookbook_directory/"attributes")        
        
        f = cookbook_directory/"attributes"/"poolparty.rb"
        # Collect the file pointers that will be using to print out the attributes
        file_pointers = {:poolparty => File.open(f, "w")}
        variables.each do |var|
          if var.parent && !var.parent.is_a?(PoolParty::Cloud)
            f = cookbook_directory/"attributes"/"#{var.parent.has_method_name}.rb"
            File.unlink f if File.file?(f)
            file_pointers[var.parent.has_method_name] = File.open(f, "w+")
          end
        end
        # Make sure the attribute exists in each file
        file_pointers.each do |n,f|
          f << "\n#{n} Mash.new unless attribute?(\"#{n}\")\n"
        end
        variables.each do |var|
          
          var_val = handle_print_variable(ProxyObject.new(var, @caller).compile(:value))
          if var.parent && !var.parent.is_a?(PoolParty::Cloud)
            file_pointers[var.parent.has_method_name] << "#{var.parent.has_method_name}[:#{var.name}] = #{var_val}\n"
          else
            file_pointers[:poolparty] << "poolparty[:#{var.name}] = #{var_val}\n"
          end
        end
        # Close the files
        file_pointers.each {|k,v| v.close }
      end
      
      # Compile the files
      def compile_files
        FileUtils.mkdir_p cookbook_directory/"files" unless ::File.directory?(cookbook_directory/"files")
        files.each do |fi|
          fpath = cookbook_directory/"templates"/"default"/"#{fi.path}.erb"
          FileUtils.mkdir_p File.dirname(fpath) unless File.directory?(File.dirname(fpath))
          content = fi.template ? open(fi.template).read : fi.content
          File.open(fpath, "w") do |f|
            f << content
          end
        end
        
        # Compile the attribute files
        attribute_files.each do |res|
          fpath = cookbook_directory/"attributes"/"#{File.basename(res.path)}"
          FileUtils.mkdir_p File.dirname(fpath) unless File.directory?(File.dirname(fpath))
          File.open(fpath, "a") do |f|
            f << res.content
          end
        end
      end
      
      # compile the recipes
      def compile_recipes
        recipes.each do |recipe|
          ddputs("[Chef recipe] Copying #{recipe.full_path} into the chef directory: #{base_cookbook_directory}")
          FileUtils.cp_r recipe.full_path, base_cookbook_directory
        end
      end
      
      # Write the dna.json out
      def write_dna_json
        File.open(compile_directory/"dna.json", "w") do |f|
          f << JSON.generate({:recipes => ["poolparty"]})
        end
      end
      
      def compile_default_recipe(content)
        FileUtils.mkdir_p cookbook_directory/"recipes" unless ::File.directory?(cookbook_directory/"recipes")
        File.open(cookbook_directory/"recipes"/"default.rb", "w") do |f|
          f << content
        end
      end
      
      def write_solo_dot_rb
        content = <<-EOE
cookbook_path     "/etc/chef/cookbooks"
node_path         "/etc/chef/nodes"
log_level         :info
file_store_path  "/etc/chef"
file_cache_path  "/etc/chef"
        EOE
        
        File.open(compile_directory/"solo.rb", "w") do |f|
          f << content
        end
      end
      
    end
    
  end
  
end