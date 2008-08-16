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
      
      def file(filelocation, opts={})
        container.files.merge!({ File.basename(filelocation).to_sym => opts })
      end

      def line_in_file(line, file)
        opts = {
          :file => file,
          :line => line
        }
        container.lines.merge!({ line.gsub(/\s+/, '').to_sym => opts })
      end
      
      def package(package, opts={})
        container.packages.merge!({ package.to_sym => opts })
      end
      
      def gem(gem)
        package(gem, {:provider => "gem", :require => "Package[rubygems]"})
      end
      
      def call(*args)
        args.each {|arg| container.custom_calls << arg }
      end
      
      def function(*args)
        args.each {|arg| container.custom_functions << arg }
      end
      
      def template(file)
        raise Exception.new("Template cannot be found. Check your path again (#{file})") unless File.file?(file)
        container.templates << file
      end
      
      # Core additions
      def set
        yield if block_given?
      end
      alias_method :configure, :set
      
      def method_missing(m, *args, &block)
        if m.to_s =~ /has/
          self.send m.to_s.gsub(/has_/, '').to_sym, *args, &block
        else
          super
        end
      end
    end
    
  end
end