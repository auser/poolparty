module PoolParty
      
  module Plugin
    module ClassMethods
      def attr_accessor(*args)        
        args.each {|arg| self.class.send :attr_accessor, arg }
      end
    end
    
    module InstanceMethods      
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
    end
    
    class Plugin
      include CustomFunction
      attr_accessor :parent
      
      def initialize(parent)
        @parent = parent
        yield if block_given?
      end
      
      def container
        @parent.parent.container
      end
      
      def has_file(filelocation, opts={})
        container.files.merge({ File.basename(filelocation).to_sym => opts })
      end

      def has_line_in_file(line, file)
        opts = {
          :file => file,
          :line => line
        }
        container.lines.merge({ line.gsub(/\s+/, '').to_sym => opts })
      end
      
      def package(package, opts={})
        container.packages.merge({ :package => opts })
      end
      
      def gem(gem)
        package(gem, {:provider => "gem", :require => "Package[rubygems]"})
      end        
      
      def template(file)
        raise Exception.new("Template cannot be found. Check your path again (#{file})") unless File.file?(file)
        container.templates << file
      end
      
      # Core additions
      def custom_function(name, str)
        self.class.send :define_method, name do
          output str
        end
      end
      def custom_functions_file(filename)
        output open(filename).read
      end
      def set
        yield if block_given?
      end

    end
    
  end
end