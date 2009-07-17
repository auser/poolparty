module Baker
  class Base
    attr_accessor :meal, :key
    attr_writer   :options, :cookbook_directory
    
    def initialize(opts={})
      @options = extract_options(opts)
      options.each {|k,v| self.send("#{k}=",v) } if options
    end
    
    def compile
      raise StandardError.new("This baker doesn't implement compile. Something is wrong")
    end
    
    def cookbook_directory
      @cookbook_directory ||= "#{meal ? meal.cookbook_directory : "./"}"
    end
    
    def options
      @options ||= {}
    end
    
    def extract_options(o={})
      if o.is_a?(String)
        {:key => o}
      else
        o
      end
    end
    
    def handle_print_variable(obj)
      case obj
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
          "#{handle_print_variable(obj[0])}, :#{obj[1]}"
        else
          "[ #{obj.map {|e| handle_print_variable(e) }.reject {|a| a.nil? || a.empty? }.join(", ")} ]"
        end        
      when nil
        nil
      when Symbol
        ":#{obj}"
      when Hash
        "#{obj.map {|k,v| ":#{k} => #{handle_print_variable(v)}" unless v == obj }.compact.join(",\n")}"
      else
        "#{obj}"
      end
    end
    
  end
end