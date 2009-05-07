=begin rdoc
  ChefResolver
  
  This takes the internal structure
  {
    :options => {},
    :services => [{}],
    :resources => []
  }
  
  and creates a chef recipe to reflect.
  
  /cookbooks/
    namespace/
      recipes/
      templates/
      attributes/
  
  TODO: This class is somewhat all over the place. Slim it down
=end
module PoolParty
  
  class ChefResolver< DependencyResolver
    
    # Compile and add to the zipper
    def compile(props=@properties_hash, tabs=0, default_namespace="poolparty")
      base_dir(default_namespace)
      build_base_recipe_directory( default_namespace )
      
      # ::Suitcase::Zipper.add( base_dir, "chef/cookbooks")
      
      _compile(props, tabs, default_namespace)
    end
    
    def _compile(props=@properties_hash, tabs=0, default_namespace="poolparty")
      cld_name = default_namespace
      comp(cld_name, props, tabs)
    end
    
    def comp(cld_name, props, tabs)
      
      default_recipe = resources_to_string(props[:resources],tabs)
      ::File.open("#{base_dir}/recipes/default.rb", "w+") {|f| f << default_recipe }
      
      default_recipe
    end
    
    def build_base_recipe_directory(nm)
      dputs "Making new #{base_dir}"
      ::FileUtils.mkdir_p "#{base_dir}"
      
      [ "recipes", "templates", "attributes" ].each do |bdir|
        ::FileUtils.mkdir_p "#{base_dir}/#{bdir}" #unless ::File.directory? "#{base_dir}/#{bdir}"
      end
      
      ::File.open("#{base_dir}/attributes/poolparty.rb", "w") do |f| 
        f << "poolparty Mash.new unless attribute?('poolparty')\n"
      end
      
      base_dir
    end
    
    def base_dir(nm="poolparty")
      @base_dir ||= "#{Default.tmp_path}/dr_configure/chef/cookbooks/#{nm}"
    end
    
    def options_to_string(opts,tabs=0)
      handle_print_variables(opts) if opts
    end
    
    def resources_to_string(opts,tabs=0)
      out = []
      out << opts.map do |resource|
        case ty = resource.delete(:pp_type)
        when "variable"
          handle_print_variable(resource)
        when "chef_recipe"
          "#{tf(tabs)}include_recipe #{to_option_string(resource.name)}"
        when "chef_library"
          "#{tf(tabs)}require #{to_option_string("/etc/chef/lib/#{resource.name}")}"
        when "plugin"
          handle_print_service(resource, tabs)
        else
          real_type = handle_chef_types(ty)
          real_name = resource[:name]
          res = before_filter_check_on_hash(resource, real_name)          
          "#{tf(tabs)}#{real_type} \"#{real_name}\" do\n#{tf(tabs+1)}#{hash_flush_out(res).compact.join("\n#{tf(tabs+1)}")}\n#{tf(tabs)}end"
        end
      end      
      out.compact.join("\n")
    end
    
    def handle_print_variable(varhash)
      o = []
      if varhash[:namespace]
        o << ["\n#{varhash[:namespace]} Mash.new unless attribute?('#{varhash[:namespace]}')"]
        o << "#{varhash[:namespace]}['#{varhash[:name]}'] = #{to_option_string(varhash[:value])}\n"
      else
        o << "poolparty['#{varhash[:name]}'] = #{to_option_string(varhash[:value])}"
      end
      ::File.open("#{base_dir}/attributes/poolparty.rb", "a+") do |f| 
        f << o.join("\n")
        f << "\n"
      end
      nil
    end
        
    def handle_chef_types(ty)
      case ty.to_sym
      when :exec
        "execute"
      when :file
        "template"
      when :symlink
        "link"
      when :line_in_file
        "execute"
      when :chef_deploy_definition
        "deploy"
      else
        ty
      end
    end

    # flush out the hash into something meaningful
    def hash_flush_out(hash, pre="", post="") 
      hash.map do |k,v|
        if o = handle_actions(k,v)
          o
        else
          key = to_chef_key(k)
          res = to_option_string(v)
          (key.nil? || res.nil?) ? nil : "#{pre}#{key} #{res}#{post}"
        end
      end
    end
    
    def handle_print_service(plugin, tabs)
      if plugin && plugin.has_key?(:resources) && !plugin[:resources].empty?
        kname = plugin.delete(:name).to_s.gsub(/pool_party_/, '').gsub(/_class/, '')
        str = "\n#{tf(tabs)}# #{kname}\n"
        str << "#{tf(tabs)}"
        str << _compile(plugin,tabs+1, kname)
        str << "#{tf(tabs)}"
      end
      str
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
      hsh.delete(:name) # we don't need the names in the methods
      hsh
    end
    
    # Handle ensures
    def handle_actions(key,value)
      case key
      when :ensures
        value.nil? ? nil : "action :#{value}"
      else
        nil
      end
    end
    
    # Take the keys from the resource hash and turn them into chef-like
    # meaningful keys. This is how helpers are created for chef
    # 
    # * <tt>reloads</tt> - todo
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
      when :not_if
        "not_if"
      when :only_if
        "only_if"
      else
        "#{key}"        
      end
    end
    
    # Resolve the value of the resource hash into a meaningful chef
    # value. Resources are turned into resource strings here
    #
    def to_option_string(obj)
      case obj
      when PoolParty::Resources::Resource
        "resources(:#{handle_chef_types(obj.class.to_s.top_level_class.downcase.to_sym)} => \"#{obj.name}\")"
      when Fixnum
        case obj
        when /^\d{3}$/
          "0#{obj.to_i}"
        else
          "#{obj.to_i}"
        end        
      when String
        case obj
        when /^\d{4}$/
          "#{obj}"
        when /^\d{3}$/
          "0#{obj}"
        else
          "\"#{obj}\""
        end
      when Proc
        obj.call # eh
      when Array
        # If we are sending a notifies with a second argument
        if obj[1] && [:immediately, :delayed].include?(obj[1])
          "#{to_option_string(obj[0])}, :#{obj[1]}"
        else
          "[ #{obj.map {|e| to_option_string(e) }.reject {|a| a.nil? || a.empty? }.join(", ")} ]"
        end        
      when nil
        nil
      when Symbol
        ":#{obj}"
      when Hash
        "#{obj.map {|k,v| ":#{k} => #{to_option_string(v)}" unless v == obj }.compact.join(",\n")}"
      else
        "#{obj}"
      end
    end
    
  end

end