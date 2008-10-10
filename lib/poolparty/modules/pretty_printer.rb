module PoolParty
  module PrettyPrinter
    
    def pretty_print(prev="\t")
      returning Array.new do |out|
        out << pretty_name(prev, self)
        out << pretty_options(prev, self)
        
        if self.respond_to?(:clouds)
          clouds.each do |name, cl|
            out << pretty_name(prev*2, cl)
            out << pretty_options(prev*2, cl) #cl.pretty_print("#{prev}\t")
          end
        end
        if self.respond_to?(:plugins)
          out << "#{prev}\t\tPlugins"
          out << "#{prev}\t\t" + plugins.map {|a| a}.join("\n")
        end
      end.join("\n")
    end
    
    # Gather options on the object
    # Do not print if the option is nil or empty.
    # Also, don't show the option if the option is empty or the default option on the cloud    
    def pretty_options(prev, o)
      return "" unless o.respond_to?(:options)
      print_options = (o.respond_to?(:parent) && o.parent && o.parent.respond_to?(:options)) ? 
        (o.options.delete_if {|k,v| o.parent.options.has_key?(k) && o.parent.options[k] == o.options[k] && !o.options[k].nil? } ) : 
        o.options
      print_options = print_options.map {|k,v| [k, o.send(k.to_sym).to_s] }.inject({}) { |r,e| r[e[0]] = e[1] unless o.class.default_options[e[0]] == e[1] || e[1].nil? || e[1].empty?; r }
      print_options.flush_out("#{prev}\t")
    end
    
    def pretty_name(prev, o)
      "#{prev}#{o.class.to_s.top_level_class.capitalize}: #{o.name if o.respond_to?(:name)}"
    end
    
  end
end