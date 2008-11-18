module PoolParty    
  module Resources
    
    def execute_on_master(parent=self, &block)
      execute_if("$hostname", "==", "master", {}, parent, &block)
    end
    
    def execute_on_node(parent=self, &block)
      execute_if("$hostname", "!=", "master", {:notequal => true}, parent, &block)
    end
    
    def execute_if(attr_s="$hostname", comparison="==", str="", cust_opts={}, parent=self, &block)
      # parent = parent.is_a?(PoolParty::Cloud::Cloud) ? parent : parent.parent
      opts = {:attribute => attr_s, :equal => str, :comparison => comparison}.merge(cust_opts)
      options = (parent.respond_to?(:options) && parent) ? parent.options.merge!(opts) : opts
      # @c = PoolParty::Resources::Conditional.new(options, parent, &block)
      # conditional(options, parent, &block)
      parent.add_resource(:conditional, options, parent, &block)
    end
    
    class Conditional < Resource
      
      def initialize(opts={}, parent=self, &block)
        name "#{opts[:name] ? opts[:name] : opts[:attribute]} #{opts[:comparison]} #{opts[:equal]}"
        attribute opts[:attribute]
        equal opts[:equal]
        notequal opts.has_key?(:notequal) ? opts[:notequal] : false
        super
      end
      
      # This way, we only get other resources, and not the conditional
      # itself as a resource
      def virtual_resource?
        true
      end
      
      def disallowed_options
        [:comparison, :notequal]
      end
      
      def printable?
        false
      end
      
      def duplicatable?
        false
      end
      
      def to_string(pre="")
        returning Array.new do |output|
          output << "# #{name.sanitize}"
          output << "case #{attribute} {"
          if notequal
            output << "#{equal} : {}"
            output << "default : { #{resources_string_from_resources(resources)} }"
          else
            output << "#{equal} : { #{resources_string_from_resources(resources)} }"
            output << "default : {}"
          end                    
          output << "}"
        end.join("\n")
      end
      
    end
    
  end
end