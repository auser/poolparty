module PoolParty    
  module Resources
    
    def execute_on_master(parent=self, &block)
      execute_if("$hostname", "==", "master", parent, &block)
    end
    
    def execute_on_node(parent=self, &block)
      execute_if("$hostname", "!=", "master", parent, &block)
    end
    
    def execute_if(attr_s="$hostname", comparison="==", str="", parent=self, &block)
      # parent = parent.is_a?(PoolParty::Cloud::Cloud) ? parent : parent.parent
      opts = {:attribute => attr_s, :equal => str, :comparison => comparison}
      options = parent.respond_to?(:options) ? parent.options.merge!(opts) : opts
      # @c = PoolParty::Resources::Conditional.new(options, parent, &block)
      parent.add_resource(:conditional, options, parent, &block)
      # @c
    end
    
    class Conditional < Resource
      
      def initialize(opts={}, parent=self, &block)
        name "#{opts[:name] ? opts[:name] : opts[:attribute]} #{opts[:comparison]} #{opts[:equal]}"
        attribute opts[:attribute]
        equal opts[:equal]
        super
      end
      
      # This way, we only get other resources, and not the conditional
      # itself as a resource
      def virtual_resource?
        true
      end
      
      def disallowed_options
        [:comparison]
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
          output << "#{equal} : { #{resources_string_from_resources(resources)} }"
          output << "default : {}"
          output << "}"
        end.join("\n")
      end
      
    end
    
  end
end