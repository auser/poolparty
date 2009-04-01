module PoolParty
  class HashPrinter
    def self.print_to_string(props={})
<<-EOE
Top Level
  #{print_hash(props, 1)}
EOE
    end
    def self.print_hash(props={}, tabs=0)
      out = []
      if props[:options]
        out << props[:options].map {|k,v| "#{print_tabs(tabs)}#{k} => #{print_value(v)}"}.join("#{print_tabs(tabs)}\n")
      end
      if props[:resources]
        props[:resources].each do |ty,r|
          out << [
            "#{print_tabs(tabs)}#{ty.to_s.capitalize}",
            r.map {|h| h.map {|k,v| "#{print_tabs(tabs+1)}#{k} => #{print_value(v)}"}}.join("#{print_tabs(tabs+1)}\n")
          ].join("\n")
        end
      end
      if props[:services]
        props[:services].each do |nm,s|
          out << [
            "#{print_tabs(tabs)}#{nm.to_s.capitalize}",
            print_hash(s,tabs+1)
          ].join("\n")
        end
      end
      out.join("\n")
    end
    def self.print_tabs(tabs=1)
      "#{"\t"*tabs}"
    end
    def self.print_value(v)
      case v
      when Array
        "[#{v.join(", ")}]"
      else
        v
      end
    end
  end
end