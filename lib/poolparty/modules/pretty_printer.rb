module PoolParty
  module PrettyPrinter
    
    def pretty_print(prev="")
      returning Array.new do |out|
        out << "#{prev}#{self.class.to_s.top_level_class.capitalize}: #{self.name if self.respond_to?(:name)}"
        print_options = self.respond_to?(:parent) ? 
          (self.options.delete_if {|k,v| parent.options.has_key?(k) && !self.class.default_options.has_key?(k)} ) : 
          self.options
        out << print_options.flush_out("#{prev}\t")
        
        if self.respond_to?(:clouds) && self.is_a?(Cloud)
          clouds.each do |name, cl|
            out << cl.pretty_print("#{prev}\t") unless self == cl
          end
        end
      end.join("\n")
    end
    
  end
end