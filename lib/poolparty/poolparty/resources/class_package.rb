module PoolParty    
  module Resources
    
    def global_classpackages
      @global_classpackage_store ||= []
    end
    
    def in_global_classpackages?(name)
      !get_from_global_classpackage_store(name).nil?
    end
    
    def get_from_global_classpackage_store(key)
      global_classpackages.select {|a| a if key == a.name }.first
    end
    
    def store_into_global_classpackage_store(r)
      arr = r.is_a?(Array) ? r : [r]
      arr.each do |a|
        global_classpackages << a unless in_global_classpackages?(a.name)
      end
    end
    
    # Wrap all the resources into a class package from 
    def classpackage_with_self(parent=self, &block)
      name = (parent && parent.options.name || Classpackage.name(parent).to_s).sanitize
      
      if in_global_classpackages?(name)
        returning get_from_global_classpackage_store(name) do |cls|
          cls.run_in_context(parent, &block) if block
        end
      else
        @@parent_resources = parent.resources
        @@cp = parent.add_resource(:classpackage, parent.options.merge(:name => name), parent, &block)
        @@cp = @@cp.is_a?(Array) ? @@cp[-1] : @@cp
        
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
      return @@cp
      @@parent_resources = @@cp = nil
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
                        
      def to_string(pre="")
        if resources.size > 0 && not_printed?
          returning Array.new do |output|
            output << "#{pre}class #{name.sanitize.downcase} {"
            output << "#{pre}#{resources_string_from_resources(resources)}"
            output << "#{pre}}"
            output << include_string
            @not_printed = false
          end.join("\n")
        else
          ""
        end
      end
      
      def not_printed?
        true
      end
      
      def include_string
        "include #{name.sanitize.downcase}"
      end
      
      def virtual_resource?
        true
      end
      
      def printable?
        false
      end
      
      def self.name(parent=self)
        "custom_#{parent ? parent.object_id.to_s : "parent"}"
      end

    end
    
  end
end