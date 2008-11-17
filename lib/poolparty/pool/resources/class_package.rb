module PoolParty    
  module Resources
    
    # Wrap all the resources into a class package from 
    def classpackage_with_self(parent=self, &block)
      @cp = PoolParty::Resources::Classpackage.new(parent.options, parent, &block)
      @cp.run_in_context {@resources = parent.resources}
      # parent.instance_eval {@resources = nil}
      @cp
    end
                
    class Classpackage < Resource
            
      default_options({
        :name => "custom"
      })
      
      def initialize(opts={}, parent=self, &block)
        # Take the options of the parents
        # set_parent(parent, false) if parent
        set_vars_from_options(opts) unless opts.empty?
        # self.instance_eval &block if block
        run_setup(parent, &block) if block
        # self.run_in_context &block if block
        # store_block(&block)        
        loaded
      end
                        
      def to_string
        returning String.new do |output|
          output << "# #{name.sanitize}"
          output << "\nclass #{name.sanitize.downcase} {\n"
          output << resources_string_from_resources(resources)
          output << "\n}\n"
        end
      end
      
      def include_string
        "include #{name.sanitize.downcase}"
      end
      
      def name(*args)
        args.empty? ? (@name || (!parent.nil? && parent.name) || "custom_#{Time.now.to_i}") : @name ||= args.first
      end
      def printable?
        false
      end

    end
    
    def resources_string_from_resources(resources, pre="\t")
      @variables = resources.extract! {|name,resource| name == :variable}
      returning Array.new do |str|
        unless @variables.empty?
          str << "\n# Variables \n"
          @variables.each do |name, variable|
            str << variable.to_string("#{pre}")
          end          
        end
        
        resources.each do |type, resource|
          str << "\n#{pre*2}# #{type}\n"
          str << resource.to_string("#{pre*2}")
        end        
      end.join("\n")
    end
    
  end
end