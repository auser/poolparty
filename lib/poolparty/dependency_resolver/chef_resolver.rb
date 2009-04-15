module PoolParty
  
  class ChefResolver< DependencyResolver
    
    def compile(props=@properties_hash, tabs=0, default_namespace="poolparty")
      cld_name = default_namespace
      comp(cld_name, props, tabs)
    end
    
    def comp(cld_name, props, tabs)
      base_dir cld_name
      basedir = build_base_recipe_directory( cld_name )
      # handle_print_variables(props[:options]) if props && props.has_key?(:options)
      
      default_recipe = [ 
        resources_to_string(props[:resources],tabs),
        services_to_string(props[:services],tabs)
      ].join("\n")
      ::File.open("#{basedir}/recipes/default.rb", "w+") {|f| f << default_recipe }        
      ::Suitcase::Zipper.add(basedir, "chef/recipes/cookbooks")
      default_recipe
    end
    
    def build_base_recipe_directory(nm)
      [ "recipes", "templates", "attributes" ].each do |bdir|
        ::FileUtils.mkdir_p "#{base_dir}/#{bdir}" unless ::File.directory? "#{base_dir}/#{bdir}"
      end
      base_dir
    end
    
    def base_dir(nm=nil)
      @base_dir ||= "#{Default.tmp_path}/dr_configure/chef/recipes/#{nm}"
    end
    
    def options_to_string(opts,tabs=0)
      handle_print_variables(opts) if opts
    end
    
    def resources_to_string(opts,tabs=0)
      out = []
      if opts

        if opts[:variable] && !opts[:variable].empty?
          vars = opts.delete(:variable)          
          handle_print_variables(vars)
        end
        
        if opts.has_key?(:line_in_file)
          lines = opts.delete(:line_in_file).inject([]) do |sum, l|
            sum << PoolParty::Resources::Exec.new(:name => l[:name], :command => PoolParty::Resources::LineInFile.command(l[:line], l[:file]) ).to_properties_hash
          end          
          if lines && lines.size > 0            
            opts.has_key?(:exec) ? (opts[:exec] << lines) : opts.merge!(:exec => lines)
          end
        end
        
        out << opts.map do |type, arr|
          arr.map do |res|
            real_type = handle_types(type)
            real_name = handle_names(type, res)
            res = before_filter_check_on_hash(res, real_name)
            "#{tf(tabs)}#{real_type} \"#{real_name}\" do\n#{tf(tabs+1)}#{hash_flush_out(res).compact.join("\n#{tf(tabs+1)}")}\n#{tf(tabs)}end"
          end
        end
      end
      out.join("\n")
    end
    
    def handle_print_variables(vars)
      
      out = ["\npoolparty Mash.new unless attribute?('poolparty')\n\n"]
      vars.each do |varhash|
        out << "#{varhash[:namespace] || "poolparty"}[:#{varhash[:name]}] = #{to_option_string(varhash[:value])}"
      end
      ::File.open("#{base_dir}/attributes/poolparty.rb", "w+") do |f| 
        f << out.join("\n")
      end
    end
    
    def handle_chef_vars(nm, varname)
      case varname
      when :enable
        "action"
      else
        "#{nm}[:#{varname}]"
      end
    end
    
    def handle_types(ty)
      case ty
      when :exec
        "execute"
      when :file
        "template"
      when :symlink
        "link"
      when :line_in_file
        "execute"
      else
        ty
      end
    end
    
    def handle_names(ty, res)
      case ty
      when :exec
        res.name
      else
        res.name
      end
    end
    
    # TODO: This is brittle, need to find a way to make them reactive, rather than 
    # separate (the key/value pairs)
    def hash_flush_out(hash, pre="", post="")      
      hash.map do |k,v|
        key = to_chef_key(k)
        res = to_option_string(v)
        (key.nil? || res.nil?) ? nil : "#{pre}#{key} #{res}#{post}"
      end
    end
    
    def handle_print_service(klassname, klassarray, tabs)
      case klassname
      when nil
        nil
      else
        kname = klassname.to_s.gsub(/pool_party_/, '').gsub(/_class/, '')
        str = "\n#{tf(tabs)}# #{kname}\n"
        str << "#{tf(tabs+1)}"
        klassarray.each do |hsh|
          str << compile(hsh,tabs+1, klassname)
        end        
        str << "#{tf(tabs)}"
      end
    end
    
    # Check if the hash has content and that the content exists here. This is used
    # to provide a check
    def before_filter_check_on_hash(hsh, nm)
      if hsh.has_key?(:content)
        cont = hsh.delete(:content)
        temp_file = "#{base_dir}/templates/default/#{nm}.erb"
        ::FileUtils.mkdir_p(::File.dirname(temp_file)) unless ::File.directory? temp_file
        ::File.open(temp_file, "w+") {|f| f.print cont }
        hsh.merge!({:source => "#{nm}.erb"})
      end
      # 
      hsh.delete(:require) if hsh.has_key?(:require)
      hsh
    end
    
    # Turn the services into strings using the handle_print_service method
    # Here we can strip out non-meaningful chef services
    def services_to_string(opts,tabs=0)
      if opts
        str = ""
        [:control_statements, :conditional].each do |k|
          opts.delete(k)
        end
        opts.map do |klassname, klasshash|
          str << handle_print_service(klassname, klasshash, tabs)
        end
        str
      end
    end
    
    # Take the keys from the resource hash and turn them into chef-like
    # meaningful keys. This is how helpers are created for chef
    def to_chef_key(key)
      case key
      when :ensures
        nil
      when :reloads
        "notifies :reload,"
      when :calls
        "notifies :run,"
      when :stops
        "notifies :stop,"
      when :starts
        "notifies :start,"
      when :enable
        nil
      when :if_not
        "not_if"
      when :notif
        "not_if"
      when :onlyif
        "only_if"
      else
        "#{key}"        
      end
    end
    
    # Resolve the value of the resource hash into a meaningful chef
    # value. Resources are turned into resource strings here
    def to_option_string(obj)
      case obj
      when PoolParty::Resources::Resource
        "resources(:#{handle_types(obj.class.to_s.top_level_class.downcase.to_sym)} => \"#{obj.name}\")"
      when Fixnum
        "#{obj.to_i}"
      when String
        "\"#{obj}\""
      when Array
        if obj[1] && [:immediately, :delayed].include?(obj[1])
          "#{to_option_string(obj[0])}, :#{obj[1]}"
        else
          "[ #{obj.map {|e| to_option_string(e) }.reject {|a| a.nil? || a.empty? }.join(", ")} ]"
        end        
      when nil
        nil
      when Hash
        "#{obj.map {|k,v| ":#{k} => #{to_option_string(v)}" unless v == obj }.compact.join(",\n")}"
      else
        "#{obj}"
      end
    end
    
  end

end