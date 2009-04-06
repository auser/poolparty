module PoolParty
  
  class ChefResolver< DependencyResolver
    
    def compile(props=@properties_hash, tabs=0, default_namespace="poolparty")
      cld_name = default_namespace
      comp(cld_name, props, tabs)
    end
    
    def comp(cld_name, props, tabs)
      base_dir cld_name
      basedir = build_base_recipe_directory( cld_name )
      handle_print_variables(props[:options], cld_name) if props && props.has_key?(:options)
      
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
      @base_dir ||= "/tmp/poolparty/dr_configure/chef/recipes/#{nm}"
    end
    
    def options_to_string(opts,tabs=0)
      # opts.map do |k,v| 
      #   res = to_option_string(v)
      #   next unless res && !res.empty?
      #   # "#{tf(tabs)}$#{k} = #{res}"
      handle_print_variables(opts, opts.name) if opts
      # end.join("\n") if opts
    end
    
    def resources_to_string(opts,tabs=0)
      out = []
      if opts
        
        if opts.has_key?(:variable)
          vars = opts.delete(:variable)          
          handle_print_variables(vars, (opts.name rescue "default"))
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
    
    def handle_print_variables(vars, nm)
      out = ["\n#{nm} Mash.new unless attribute?('#{nm}')\n\n"]
      vars.each do |varname, value|
        vname = handle_chef_vars(nm, varname)
        val = to_option_string(value)
        out << "#{vname} = #{val}" if vname && val
      end
      ::File.open("#{base_dir}/attributes/#{nm}.rb", "w+") do |f| 
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
    
    def before_filter_check_on_hash(hsh, nm)
      if hsh.has_key?(:content)
        cont = hsh.delete(:content)
        temp_file = "#{base_dir}/templates/default/#{nm}.erb"
        ::FileUtils.mkdir_p(::File.dirname(temp_file)) unless ::File.directory? temp_file
        ::File.open(temp_file, "w+") {|f| f.print cont }
        hsh.merge!({:source => "#{nm}.erb", :variables => hsh})
      end
      # 
      hsh.delete(:require) if hsh.has_key?(:require)
      hsh
    end
    
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
    
    def to_chef_key(key)
      case key
      when :ensures
        nil
      when :enable
        nil
      when :onlyif
        "only_if"
      else
        "#{key}"        
      end
    end
    
    def to_option_string(obj)
      case obj
      when PoolParty::Resources::Resource
        "resources(:#{obj.class.to_s.top_level_class.downcase} => \"#{obj.name}\")"
      when Fixnum
        "#{obj.to_i}"
      when String
        "\"#{obj}\""
      when Array
        "[ #{obj.map {|e| to_option_string(e) }.reject {|a| a.nil? || a.empty? }.join(", ")} ]"
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