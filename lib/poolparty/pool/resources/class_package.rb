module PoolParty    
  module Resources
    
    def global_classpackages
      $global_classpackage_store ||= []
    end
    
    def in_global_classpackages?(name)
      !get_from_global_classpackage_store(name).nil?
    end
    
    def get_from_global_classpackage_store(key)
      global_classpackages.select {|a| a if key == a.name }.first
    end
    
    def store_into_global_classpackage_store(r)
      global_classpackages << r unless in_global_classpackages?(r.name)
    end
    
    # Wrap all the resources into a class package from 
    def classpackage_with_self(parent=self, &block)
      name = (parent.options.name || Classpackage.name(parent).to_s).sanitize
      if in_global_classpackages?(name) 
        @@cp = get_from_global_classpackage_store(name)
        @@cp.run_in_context(parent, &block) if block
      else
        @@parent_resources = parent.resources
        @@cp = parent.add_resource(:classpackage, parent.options.merge(:name => name), parent)

        @@cp.run_in_context(parent) do
          @@parent_resources.each do |ty, res|
            resources[ty] = res unless ty == :classpackage
          end
        end
        parent.instance_eval do
          @resources = {:classpackage => [@@cp]}
        end
        @@cp.instance_eval &block if block
        
        store_into_global_classpackage_store(@@cp)
      end
      @@parent_resources = nil
      @@cp
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
      
      def printable?
        false
      end
      
      def self.name(parent=nil)
        "custom_#{parent ? parent.object_id.to_s : "parent"}"
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