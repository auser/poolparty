# Class: PuppetResolver< DependencyResolver
#
#
module PoolParty
  
  class PuppetResolver< DependencyResolver
    
    def initialize(hsh=nil)
      super(hsh)
    end
    
    def self.compile(props)
      "class poolparty {
        #{new(props).compile}
      }"
    end
    
    def compile(props=@properties_hash, tabs=0)
      resources_to_string(props[:resources],tabs)
    end
    
    def options_to_string(opts,tabs=0)
      opts.map do |k,v| 
        res = to_option_string(v)
        next unless res && !res.empty?
        "#{tf(tabs)}$#{k} = #{res}"
      end.join("\n") if opts
    end
    
    def resources_to_string(opts,tabs=0)
      out = []
      if opts
        if opts.has_key?(:variable)
          vars = opts.delete(:variable)
          out << vars.map do |res, arr|
            handle_print_resource(res, :variable, tabs)
          end
        end

        out << opts.map do |type, arr|
          arr.map do |res|
            handle_print_resource(res, type, tabs)
          end
        end
      end
      out.join("\n")
    end
    
    def resources_to_string(opts,tabs=0)
      out = []        
      out << opts.map do |resource|
        case ty = resource.delete(:pp_type)
        when "variable"
          handle_print_variable(resource[:name], resource[:value], :variable)
        when "plugin"
          handle_print_service(resource.delete(:name), resource, tabs)
        else
          real_name = resource[:name]
          handle_print_resource(resource, ty.to_sym, tabs)
        end
      end      
      out.join("\n")
    end
    
    
    def permitted_option?(ty, key)
      true
    end
    
    def hash_flush_out(hash, pre="", post="")
      setup_hash_for_output(hash).map do |k,v|
        hash.empty? ? nil : "#{pre}#{k} => #{v}#{post}"
      end
    end
    
    def setup_hash_for_output(hsh)
      ty = hsh.delete(:klasstype)
      if hsh.has_key?(:ensures)
        hsh.delete(:ensures)
        hsh[:ensure] = case ty.to_s
        when "directory"
          "directory"
        when "symlink"
          hsh[:source]
        else
          "present"
        end
      end

      if hsh.has_key?(:requires)
        hsh[:require] = hsh.delete(:requires)
      end
      new_hsh ={}
      hsh.each do |k,v|
        new_hsh.merge!({k => to_option_string(v)})
      end
      new_hsh
    end
        
    def to_option_string(obj)
      case obj
      when PoolParty::Resources::Resource
        case obj
        when PoolParty::Resources::Directory
          "File[\"#{obj.name}\"]"
        else
          "#{obj.class.to_s.top_level_class.capitalize}[\"#{obj.name}\"]"
        end        
      when Fixnum
        "#{obj}"
      when String
        obj = obj
        obj =~ /generate\(/ ? "#{obj}" : "\"#{obj.safe_quote}\""
      when Array
        "[ #{obj.map {|e| to_option_string(e) }.reject {|a| a.nil? || a.empty? }.join(", ")} ]"
      else
        "#{obj}"
      end
    end
    
    def handle_print_service(klassname, plugin, tabs)
      kname = klassname.to_s.gsub(/pool_party_/, '').gsub(/_class/, '')
      str = "\n#{tf(tabs)}# #{kname}\n"
      str << "#{tf(tabs)}class #{kname} {"
      str << "\n#{tf(tabs+1)}#{compile(plugin,tabs+1)}"
      str << "#{tf(tabs)}} include #{kname}"        
    end
    
    def handle_print_variable(name, value, tabs)
      "$#{name} = #{to_option_string(value)}"
    end
    
    def handle_print_resource(res, type, tabs)
      case type.to_s        
      when "line_in_file"
        "#{tf(tabs)}exec { \"#{res[:file]}_line_#{tabs}\": \n#{tf(tabs+1)}command => '#{PoolParty::Resources::LineInFile.command(res[:line], res[:file])}',\n#{tf(tabs+1)}path => '/usr/local/bin:$PATH'\n#{tf(tabs)}}"
      else
        klasstype = case type.to_s
        when "directory"
          "file"
        when "symlink"
          "file"
        else
          type
        end
        res.merge!(:klasstype => type.to_s)
        "#{tf(tabs)}#{klasstype} { \"#{res.delete(:name) }\": #{res.empty? ? "" : "\n#{tf(tabs+1)}#{hash_flush_out(res.reject {|k,v| !permitted_option?(type, k) }).reject {|s| s.nil? }.join(",\n#{tf(tabs+1)}")}"}\n#{tf(tabs)}}"
      end
    end
    
    # This is the method we use to turn the options into a string to build the main 
    # puppet manifests
    def option_type(ns=[])
      a_template = (self =~ /template/) == 0
      a_service = self =~ /^[A-Z][a-zA-Z]*\[[a-zA-Z0-9\-\.\"\'_\$\{\}\/]*\]/i
      a_function = self =~/(.)*\((.)*\)(.)*/
      if is_a?(PoolParty::Resources::Resource)
        self.to_s
      else
        (a_service || a_template || a_function) ? self : "'#{self}'"
      end    
    end
    
  end

end
