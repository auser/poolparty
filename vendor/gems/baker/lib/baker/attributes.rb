=begin rdoc
  The meal of the baker
=end
module Baker
  class Attributes < Base
    
    attr_reader :attributes, :basename
    
    def initialize(opts={})
      @attributes = {}
      @basename = opts.delete(:basename) || File.dirname(cookbook_directory)
      super
    end
    
    def compile
      attributes.each do |k,v|
        print_variable(k, v)
      end
    end
    
    def variables(vars)
      case vars
      when Hash
        set_hash_attributes(vars)
      end
    end
    
    private
    
    def set_hash_attributes(hsh)
      @attributes.merge!(hsh)
    end
    
    def print_variable(var_name, var_hash)
      dir = "#{cookbook_directory}/#{attr_path}"
      ::FileUtils.mkdir_p dir unless ::File.directory?(dir)
      File.open("#{dir}/#{var_name}.rb", "w") do |f|
        f << "# variable #{basename}\n#{basename} Mash.new unless attribute?('#{basename}')\n\n"
        content = var_hash.map do |k,v|
          "#{basename}['#{var_name}']['#{k}'] = #{handle_print_variable(v)}"
        end.join("\n")
        f << content
      end
    end
    
    def attr_path
      "attributes"
    end
        
  end
end