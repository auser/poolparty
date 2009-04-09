module PoolParty    
  module Resources
        
    class File < Resource
      
      def after_create
        if dsl_options.include?(:template)          
          filename = ::File.expand_path(dsl_options.template)
          dsl_options.delete(:template)
          file = ::File.basename( filename )
          raise TemplateNotFound.new("no template given") unless file

          template_opts = (parent ? options.merge(parent.options) : options)
          options.merge!(:content => Template.compile_file(filename, template_opts).gsub("\"", "\""))
        end
        
        if dsl_options.include?(:content)
          cont = dsl_options.delete(:content)
          template_opts = (parent ? options.merge(parent.options) : options)
          options.merge!(:content => ((dsl_options.include?(:erb) && dsl_options.delete(:erb)) ? Template.compile_string(cont, template_opts) : cont))
        end
      end
      
    end
    
  end
end