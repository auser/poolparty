module PoolParty
      
  module Plugin
    module ClassMethods
    end
    
    module InstanceMethods
      def attr_accessor(*args)        
        args.each {|arg| self.class.send :attr_accessor, arg }
      end
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
    end
    
    class Plugin
      include CustomFunction
      
      def has_file(filelocation, opts={})
        output <<-EOF
          file { #{File.basename(filelocation, File.extname(filelocation))}:
              file  => #{filelocation},
              owner => #{opts[:owner] || "root"},
              group => #{opts[:group] || "root"}, 
              mode => #{opts[:mode] || "440"}
          }
        EOF
      end

      def has_line_in_file(line, file)
        append_if_no_such_line
        output <<-EOM
        append_if_no_such_line{ #{line[-1..10]}:
                   file => "#{file}",
                   line => "#{line}"}
        EOM
      end
      
      def package(package, opts={})
        output <<-EOM
          package {
            #{package}: 
              ensure => installed
              #{opts.map {|k,v| "#{k} => #{v}"}}
          }
        EOM
      end
      
      def gem(gem)
        package(gem, {:provider => "gem", :require => "Package[rubygems]"})
      end        
      
      def template(file)
        raise Exception.new("Template cannot be found. Check your path again (#{file})") unless File.file?(file)
        file
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