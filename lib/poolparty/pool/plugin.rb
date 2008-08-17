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
      
      def initialize(parent=nil)
        @parent = parent
        yield if block_given?
      end
      
      def container
        @parent.parent.container
      end
      
      def file(filelocation, opts={})
      end

      def line_in_file(line, file)
      end
      
      def package(package, opts={})
      end
      
      def gem(gem)
        package(gem, {:provider => "gem", :require => "Package[rubygems]"})
      end
      
      def call(*args)
      end
      
      def function(*args)
      end
      
      def template(file)
        raise Exception.new("Template cannot be found. Check your path again (#{file})") unless File.file?(file)
        file
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