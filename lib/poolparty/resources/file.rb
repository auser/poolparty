module PoolParty    
  module Resources
        
=begin rdoc rdoc
== File

The file resource is used to describe a file that should be present on all of the instances.

== Usage

  has_file(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> Describe the location of the file with the name
* <tt>mode</tt> Describe the mode of the file (default: 644)
* <tt>owner</tt> The owner of the file (default: poolparty user)
* <tt>content</tt> The contents of the file
* <tt>source</tt> Used to describe a file that is hosted on the master instance.
* <tt>template</tt> The file contents are described with the template. The location given must be readable
  
To write a file to the template directory, use:

  copy_template_to_storage_directory(filepath)

== Example
  has_file(:name => '/etc/motd', :content => 'Hey and welcome to your node today!')
=end
    class File < Resource
      has_searchable_paths(:dir => "templates")
      
      dsl_methods :name,            # The name, the full path of the file
                  :owner,           # String that describes the owner of the file
                  :content,         # A string that describes the content of the file
                  :template,        # The file that describes the content of the file
                  :render_as        # Render the content (Erb) (default: Erb)
                  
      default_options(
        :mode => "644"              # A string indicating the mode of the file
      )
      
      def loaded(o={}, &block)
        has_directory ::File.dirname(name)
      end
      
      def present
        :create
      end
      
      def absent
        :delete
      end
      
      def after_create
        run_render = dsl_options.include?(:render_as) ? dsl_options.delete(:render_as) : false
        
        if dsl_options[:template]
          filename = find_file(dsl_options.delete(:template))
          file = ::File.basename( filename )
          raise TemplateNotFound.new("no template given") unless file

          template_opts = (parent ? options.merge(parent.options) : options)
          options.merge!(:content => run_render ? Template.compile_file(filename, template_opts.merge(:renderer => run_render)).gsub("\"", "\"") : open(filename).read)
        end
        
        if dsl_options.include?(:content)
          cont = dsl_options.delete(:content)
          template_opts = (parent ? options.merge(parent.options) : options).merge(:renderer => run_render)
          options.merge!(:content => run_render ? Template.compile_string(cont, template_opts) : cont)
        end
      end
      
      # def method_missing m, *a, &block
      #   super rescue ::File.send(m, *a, &block)
      # end
      
      def variable(k,v)
        dsl_option(k,v)
      end

    end
    
  end
end