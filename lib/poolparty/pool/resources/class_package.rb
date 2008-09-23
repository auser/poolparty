module PoolParty    
  module Resources
    
    def class_package(opts={}, &block)
      PoolParty::Resources::Classpackage.new(opts, &block)
    end
    
    class Classpackage < Resource      
      include Resources
      
      default_options({
        :name => "custom"
      })
      
      def resources
        @@class_resources ||= {}
      end
      def to_string
        returning Array.new do |output|
          output << "class #{name} {"
          resources.map {|k,resource| output << resource.to_string("\t") }
          output << "}"
        end.join("\n")
      end
      
      def <<(*args)
        args.each {|arg| 
          type = arg.class.to_s.top_level_class.to_sym
          resource(type) << arg unless arg.class.to_s == "PoolParty::Resources::Classpackage"
        }
      end
      alias_method :push, :<<
      
    end
    
  end
end