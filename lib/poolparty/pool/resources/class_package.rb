module PoolParty    
  module Resources
    
    def class_package(opts={}, &block)
      PoolParty::Resources::ClassPackage.new(opts, &block)
    end
    
    class ClassPackage < Resource      
      include Resources
      
      default_options({
        :name => "custom"
      })
      
      def initialize(opts={}, &block)
        super(opts, &block)
      end
      
      def to_s
        returning Array.new do |output|
          output << "class #{name} {"
          resources.map {|resource| output << resource.to_s }
          output << "}"
        end.join("\n")
      end
      
      def <<(*args)
        args.each {|arg| 
          type = arg.class.to_s.top_level_class.to_sym
          resources[type] ||= arg
        }
      end
      alias_method :push, :<<
      
    end
    
  end
end