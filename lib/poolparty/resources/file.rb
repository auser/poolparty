module PoolParty    
  module Resources
        
    class File < Resource
      
      def present
        :create
      end
      
      def absent
        :delete
      end
      
      def after_create
        run_render = dsl_options.include?(:render_as) ? dsl_options.delete(:render_as) : false
        
        if dsl_options.include?(:template)          
          filename = ::File.expand_path(dsl_options.template)
          dsl_options.delete(:template)
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
      
      def method_missing m, *a, &block
        super rescue ::File.send(m, *a, &block)
      end
      
    end
    
  end
end