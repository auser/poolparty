=begin rdoc
  Base dependency_resolver
=end
module PoolParty
  module DependencyResolvers
    
    class Base
            
      def self.compile(array_of_resources=[])
        out = []
        array_of_resources.each do |res|          
          if res.respond_to?(compile_method_name)
            po = ProxyObject.new(res)
            out << po.compile(compile_method_name)
          end
        end
        out.join("\n")
      end
      
      # The name of the method that the resource
      # should respond to to compile
      # Format:
      #   print_to_<dependency_resolver.name>
      def self.compile_method_name
        @compile_method_name ||= "print_to_#{name.to_s.top_level_class}".to_sym
      end
      
    end
    
  end
end