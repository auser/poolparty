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
    
    # Wrap all the resources into a class package.
    # This method will first check to see if a class has already been declared
    # and run the containing block on it to attach the new resources on to the new block
    # If the class does not exist, then it is the responsibility of this method to pull
    # the resources from the parent into the new class package resource and remove them
    # from the parent. This way we can conveniently write classes into the manifest, 
    # giving us separation for variables and the like.
    # Finally, the method will remove the all resources from the contianing parent and add
    # the class package as the resource. 
    # Note that it only removes resources that are not class packages, so this method will
    # not remove other classes that have been attached to the same resource.
    # TODO CLEAN THIS UP
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
        set_vars_from_options(opts) unless opts.empty?
        run_setup(parent, &block) if block
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