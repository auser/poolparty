module PoolParty    
  module Resources
    
    def classpackage_with_self(options={}, parent=self, &block)
      @cp = PoolParty::Resources::Classpackage.new(options, parent, &block)
      @cp.instance_eval {@resources = parent.resources}
      parent.instance_eval {@resources = nil}
      @cp
    end
                
    class Classpackage < Resource
            
      default_options({
        :name => nil
      })
                  
      def to_string
        returning String.new do |output|
          output << "\nclass #{(self.send key).sanitize} {\n"
          output << resources_string_from_resources(resources)
          output << "\n}\n"
        end
      end

    end
    
    def resources_string_from_resources(resources, prev="\t")
      @variables = resources.extract! {|name,resource| name == :variable}
      returning Array.new do |str|
        unless @variables.empty?
          str << "\n# Variables \n"
          @variables.each do |name, variable|
            str << variable.to_string("#{prev}")
          end          
        end
        
        resources.each do |type, resource|
          str << "\n#{prev*2}# #{type}\n"
          str << resource.to_string("#{prev*2}")
        end        
      end.join("\n")
    end
    
  end
end