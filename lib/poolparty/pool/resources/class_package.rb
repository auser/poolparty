module PoolParty    
  module Resources
    
    def classpackage_with_self(opts={}, parent=self, &block)
      @cp = PoolParty::Resources::Classpackage.new(options, parent, &block)
      @cp.instance_eval {@resources = parent.resources}
      parent.instance_eval {@resources = nil}
      @cp
    end
                
    class Classpackage < Resource
            
      default_options({
        :name => "custom"
      })
                  
      def to_string
        returning String.new do |output|
          output << "\nclass #{key.sanitize} {\n"
          output << variable_string_from_resources(resource(:variable))
          output << resources_string_from_resources(resources)
          output << "\n}\n"
        end
      end
            
    end
    
    # Various helpers to turn resources into strings
    def variable_string_from_resources(variables)
      if variables
        returning String.new do |str|
          str << "\n# Variables \n"
          str << variables.to_string("#{prev}")
        end
      else
        ""
      end
    end
    
    def resources_string_from_resources(res, prev="\t")
      if res
        returning String.new do |output|
          res.each do |type, resource|
            unless type == :variable
              output << "\n#{prev*2}# #{type}\n"
              output << resource.to_string("#{prev*2}")
            end
          end          
        end
      else 
        ""
      end
    end
    
  end
end